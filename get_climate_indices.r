#######DESCRIPTION###############################
#downloads standard climate indices from NOAA ESRL
#http://www.esrl.noaa.gov/psd/data/climateindices/list/
#################################################
##load libraries
library(data.table)
library(dplyr)	
library(lubridate)
library(readr)
library(tidyr)

##user inputs
dir_dat = ''
run_name = 'esrl_indices'
indices_list = c('pna', 'nao', 'soi', 'tna', 'tsa', 'pdo', 'nina3', 'nina1', 'nina34')
esrl_header = 'http://www.esrl.noaa.gov/psd/data/correlation/'

##read in data
dat_dt = NULL
for(i in 1:length(indices_list)){
	indices_temp = indices_list[i]
	url_temp = paste0(esrl_header, indices_temp, '.data')
	dat_temp = read_lines(url_temp)
	yr_list = gsub("[[:space:]]", "", dat_temp[1])
	yr_start = as.numeric(substr(yr_list, 1, 4))	
	yr_end = as.numeric(substr(yr_list, 5, 8))	
	nyears = length(yr_start:yr_end)
	na_val = as.numeric(dat_temp[(nyears + 2)])
	dat_temp_dt = read.table(textConnection(dat_temp[2:(nyears + 1)])) %>% gather(month, value, -V1) %>% dplyr::mutate(month = as.numeric(month), index = indices_temp, value = ifelse(value == na_val, NA, value))
	dat_dt = bind_rows(dat_dt, dat_temp_dt)
}

##save data
saveRDS(dat_dt, paste0(dir_dat, run_name, '.rda'))
