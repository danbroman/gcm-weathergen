#######DESCRIPTION###############################
#extracts shortwave solar radiation from NCEP/NCAR solar radiation given a set of coordinates
#################################################
##load packages
library(data.table)
library(dplyr)	
library(ncdf4)
library(lubridate)

##user inputs
dir_raw = '/Users/danbroman/Documents/Graduate School/Data/20th Century Renanlysis/'
dir_meta = '/Users/danbroman/Documents/Heat Stress/data/'
dir_dat = '/Users/danbroman/Documents/Heat Stress/data/'
run_name = 'isd_wafr'
meta_file = 'isd_wafr-metadata.rda'
run_name = 'nnr-srad-wafr'

##preprocess
file_list = list.files(dir_raw, pattern = '*.nc')
metadata = readRDS(paste0(dir_meta, meta_file))

##process
#get indices
nc_temp = nc_open(paste(dir_raw, file_list[1], sep = ''))
lon_nnr = ncvar_get(nc_temp, 'lon') - 180
lat_nnr = ncvar_get(nc_temp, 'lat')

metadata = metadata %>% group_by(lon) %>% dplyr::mutate(lon_rg = lon_nnr[which.min(abs(lon - lon_nnr))]) %>% group_by(lat) %>% dplyr::mutate(lat_rg = lat_nnr[which.min(abs(lat - lat_nnr))])
lon_list = metadata$lon_rg
lat_list = metadata$lat_rg

#time
time_base = as.POSIXct('1800-01-01 00:00:00')
time_ref = data.table(time = round(seq(from = time_base, to = Sys.time(), by = 'hour'), 'hour'))
time_ref$timeind = 0:(nrow(time_ref) - 1)

#read in data
solarrad_dat = NULL
for(i in 1:length(file_list)) {
	file_temp = file_list[i]
	nc_temp = nc_open(paste0(dir_raw, file_temp))
	solarrad_temp = ncvar_get(nc_temp, 'dswrf')
	time_temp = ncvar_get(nc_temp, 'time')
	test = as.numeric(solarrad_temp)
	solarrad_dat_temp = data.table(lon = lon_nnr, lat = rep(lat_nnr, each = length(lon_nnr)), timeind = rep(time_temp, each = length(lon_nnr) * length(lat_nnr)), val = test)
	solarrad_dat_temp = solarrad_dat_temp %>% dplyr::filter(lon >= min(lon_list), lon <= max(lon_list), lat >= min(lat_list), lat <= max(lat_list))
	solarrad_dat = bind_rows(solarrad_dat, solarrad_dat_temp)
}

#3hr to daily
solarrad_dat = solarrad_dat %>% left_join(time_ref) %>% dplyr::mutate(date = as.Date(time)) %>% group_by(date, lon, lat) %>% dplyr::summarise(val = mean(val))

#pull out timeseries for each station location
solarrad_dat_sta = NULL
for(i in 1:nrow(metadata)) {
	metadata_temp = metadata[i]
	lon_temp = metadata_temp$lon_rg
	lat_temp = metadata_temp$lat_rg
	solarrad_dat_sta_temp = solarrad_dat %>% dplyr::filter(lon == lon_temp, lat == lat_temp) %>% dplyr::rename(srad = val) %>% dplyr::select(date, srad) %>% dplyr::mutate(usaf = metadata_temp$usaf, name = metadata_temp$name)
	solarrad_dat_sta = bind_rows(solarrad_dat_sta, solarrad_dat_sta_temp)
}
solarrad_dat_sta = solarrad_dat_sta %>% dplyr::mutate(year = year(date), month = month(date), day = day(date)) %>% dplyr::select(usaf, name, year, month, day, srad)
saveRDS(solarrad_dat_sta, paste0(dir_dat, run_name, '.rda'))
 

