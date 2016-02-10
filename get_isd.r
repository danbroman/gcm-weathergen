#######DESCRIPTION###############################
#processes HAD ISD data downloaded from http://www.metoffice.gov.uk/hadobs/hadisd/
#subsets based on bounding box
#calculates daily min/max/mean based on hourly obs.
#qc / filters data based on no. days, length of gaps, no. observations per day
#worldmet and rnoaa packages can access noaa isd data directly and have similar subset ability, but they've been buggy and slow
#################################################
##load packages
library(data.table)
library(dplyr)	
library(ncdf4)
library(lubridate)

#rh from air temp and dewpoint using august-roche-magnus approximation
rh = function(ta, td){
	rh_c <- 100 * exp((17.625 * td) / (243.04 + td)) / exp((17.625 * ta) / (243.04 + ta))
	return(rh_c)
}
##user inputs
lon_min = -20
lon_max = 20
lat_min = 5
lat_max = 25
start_date = as.Date('1973-01-01')
end_date = as.Date('2014-12-31')
frac_thresh = 0.9 #min fraction of daily observations
nhour_thresh = 2 #min number of observations per day
nmiss_thresh = 60 #max number of consecutive days missing 
#location of ncdf files
dir_dat = '/Users/danbroman/Documents/Graduate School/Data/HadISD/WMO_600000-699999_2013f/' 
header_dat = 'hadisd.1.0.2.2013f.' #based on version of HADISD
#location of station info file
isd_info = fread('/Users/danbroman/Documents/Graduate School/Data/HadISD/isd-history.csv')
#location of output
dir_output = '/Users/danbroman/Documents/Graduate School/Heat Stress1/'
#run name - output file tag
run_name = 'isd_wafr'

##preprocess
time_base = as.POSIXct('1973-01-01 00:00:00')
time_ref = data.table(time = seq(from = time_base, to = Sys.time(), by = 'hour'))
time_ref$timeind = 0:(nrow(time_ref) - 1)

isd_info = isd_info %>% setNames(tolower(names(isd_info))) %>% dplyr::mutate(usaf = as.numeric(usaf), wban = as.numeric(wban),lon = as.numeric(lon), lat = as.numeric(lat), elev = as.numeric(elev)) %>% dplyr::mutate(begin = as.Date(paste(substr(begin, 1, 4), substr(begin, 5, 6), substr(begin, 7, 8), sep = '-')), end = as.Date(paste(substr(end, 1, 4), substr(end, 5, 6), substr(end, 7, 8), sep = '-')), code = paste(usaf, wban, sep = '-'))

nday_thresh = length(seq(from = start_date, to = end_date, by = 'day')) * frac_thresh #min number of days 

##filter stations
isd_set = isd_info %>% dplyr::filter(lon >= lon_min, lon <= lon_max, lat >= lat_min, lat <= lat_max, begin <= start_date, end >= end_date) 

##read in data
dat_dt = NULL
for(i in 1:nrow(isd_set)){
	isd_set_temp = isd_set[i]
	code_temp = isd_set$code[i]
	nc_temp = try(nc_open(paste0(dir_dat, header_dat, code_temp, '.nc')))
	if(length(nc_temp) > 1){
		nvars = nc_temp$nvars
		time_temp = ncvar_get(nc_temp, 'time')
		dat_temp = data.table(timeind = time_temp)
		isd_set_temp_dt = isd_set_temp[rep(1, nrow(dat_temp))] 
		dat_temp = bind_cols(dat_temp, isd_set_temp_dt)
		for(j in 1:15){
			var_temp = nc_temp$var[[j]]
			varname_temp = var_temp$name 
			varmissval_temp = var_temp$missval
			val_temp = ncvar_get(nc_temp, varname_temp)
			val_temp[which(val_temp == varmissval_temp)] = NA
			val_temp = val_temp %>% data.table() %>% setNames(varname_temp)
			dat_temp = bind_cols(dat_temp, val_temp)
		}
		nc_close(nc_temp)
		dat_dt = bind_rows(dat_dt, dat_temp)
	}
}

##qc / filter data
dat_dt = dat_dt %>% left_join(time_ref)
dat_day = dat_dt %>% dplyr::mutate(year = year(time), month = month(time), day = day(time), jd = as.numeric(format(time, '%j')), rh = rh(temperatures, dewpoints)) %>% group_by(usaf, name, ctry, lon, lat, elev, year, month, day, jd) %>% dplyr::summarise(prcp = sum(precip1_depth, na.rm = T), tmin = min(temperatures, na.rm = T), tmax = max(temperatures, na.rm = T), tmean = mean(temperatures, na.rm = T), rhmin = min(rh, na.rm = T), rhmax = max(rh, na.rm = T), rhmean = mean(rh, na.rm = T), windmin = min(windspeeds, na.rm = T), windmax = max(windspeeds, na.rm = T), windmean = mean(windspeeds, na.rm = T), nhour = n()) %>% group_by(name) %>% dplyr::mutate(date = as.Date(paste(year, month, day, sep = '-'))) %>% dplyr::mutate(nday = n(), date_diff = as.numeric(difftime(date, lag(date))))
dat_day_qc = dat_day %>% dplyr::filter(nhour >= nhour_thresh)

filter_set = dat_day %>% group_by(name, usaf) %>% dplyr::summarise(date_diff = max(date_diff, na.rm = T), nday = max(nday)) %>% dplyr::filter(date_diff <= nmiss_thresh, nday >= nday_thresh)
dat_day_qc = dat_day_qc %>% ungroup() %>% dplyr::filter(usaf %in% filter_set$usaf)

##save data
write.csv(isd_set, paste0(dir_output, run_name, '-metadata.csv'))
saveRDA(isd_set, paste0(dir_output, run_name, '-metadata.rda'))

write.csv(dat_day_qc, paste0(dir_output, run_name, '.csv'))
saveRDA(dat_day_qc, paste0(dir_output, run_name, '.rda'))
