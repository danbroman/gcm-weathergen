#######DESCRIPTION###############################
#identifies station clusters using seasonal weather 
#data - clusters for each season and variable
#################################################
library(data.table)
library(dplyr)	
library(lubridate)
library(tools)
library(stringr)
library(ggplot2)
library(cluster)
library(zoo)

##user inputs
dir_dat = ''
meta_file = ''

##read in and process data
weather_dat = readRDS(paste0(dir_dat, 'isd_wafr', '.rda'))
seas_dt = data.table(month = 1:12, seas = rep(c('jfm', 'amj', 'jas', 'ond'), each = 3))
weather_dat = weather_dat %>% left_join(seas_dt) %>% dplyr::mutate(THI = thi(tmean, rhmean)) %>% dplyr::mutate(name = str_replace_all(name, "[[:punct:]]", " "))

##clustering
weather_dat_seas = weather_dat %>% group_by(name, seas, year) %>% dplyr::summarise(tmin.seas = mean(tmin, na.rm = T), tmax.seas = mean(tmax, na.rm = T), rhmin.seas = mean(rhmin, na.rm = T), rhmax.seas = mean(rhmax, na.rm = T), prcp.seas = mean(prcp, na.rm = T), prcpocc.seas = mean(prcpocc, na.rm = T)) %>% tidyr::gather(key = variable, value = value, prcp.seas, prcpocc.seas, tmin.seas, tmax.seas, rhmin.seas, rhmax.seas) %>% dplyr::mutate(value_fill = na.approx(value)) %>% ungroup()%>% data.table() 
weather_dat_seas = weather_dat_seas %>% dplyr::filter(name != 'MATAM OURO SOGUI') #missing lots of 1974 data

seas_list = unique(weather_dat_seas $seas)
variable_list = unique(weather_dat_seas $variable)
clust_dt = NULL
for(j in 1:length(seas_list)){
	for(k in 1:length(variable_list)){
		weather_dat_seas_set = weather_dat_seas %>% dplyr::filter(seas == seas_list[j], variable == variable_list[k]) %>% dplyr::select(-seas, -variable, -value) %>% tidyr::spread(name, value_fill) %>% dplyr::select(-year)
		clust_temp = data.table(name = names(weather_dat_seas_set), seas = seas_list[j], variable = variable_list[k], clust_pam = pam(t(weather_dat_seas_set), 3)$clustering, clust_kmeans = as.numeric(unlist(kmeans(t(weather_dat_seas_set), 3)$cluster)))
		clust_dt = rbind_list(clust_dt, clust_temp)
	}
}
clust_dt = data.table(clust_dt)
write.csv(clust_dt, paste0(dir_dat, 'clust_table.csv'), row.names = F)
saveRDS(clust_dt, paste0(dir_dat, 'clust_table.rda'))
