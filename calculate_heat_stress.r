#######DESCRIPTION###############################
#calculates heat stress measures using processed had isd data
#and processed nnr solar radiation data
#################################################
##load libraries
library(data.table)
library(dplyr)	
library(lubridate)

thi = function(ta, rh){
	thi_c = 0.8 * ta + rh / 100 * (ta - 14.4) + 46.4
	return(thi_c)
}

##user inputs
dir_dat = '/Users/danbroman/Documents/Heat Stress/data/'
run_name_isd = 'isd_wafr'
run_name_nnr = 'nnr-srad-wafr'
run_name = 'wafr'
##read in data
isd_dat = readRDS(paste0(dir_dat, run_name_isd, '.rda'))
nnr_dat = readRDS(paste0(dir_dat, run_name_nnr, '.rda'))

##process
weather_dat = isd_dat %>% left_join(nnr_dat)
weather_dat = weather_dat %>% dplyr::mutate(THI = thi(tmean, rhmean), BgT = (1.33 * tmean - 2.65 *sqrt(tmean) + 3.21 *log((srad + 1), base = 10) +3.5)) %>% dplyr::mutate(S.BgT = 1 / (1 + exp(-(BgT - 25) / 2.25))) %>% dplyr::mutate(HLI.lo = 1.3 * BgT + 0.28 * (rhmean - windmean) + 10.66, HLI.hi = 1.55 * BgT + 0.38 * (rhmean - 0.5*windmean) + exp(2.4 - windmean) + 8.62) %>% dplyr::mutate(HLI = S.BgT * HLI.hi + (1 - S.BgT) * HLI.lo)

saveRDS(weather_dat, paste0(dir_dat, run_name, '-heatstress.rda'))
