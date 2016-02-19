#######DESCRIPTION###############################
#runs single-site stochastic weather generator
#with seasonal covariates
#generates daily prcp, tmax, tmin, rhmax, rhmin
#windmax, windmin, and srad
#################################################
##load libraries
library(dplyr)
library(tidyr)
library(data.table)
library(boot)
library(foreach)
library(doParallel)
library(lubridate)
na.set = function(na.inds, dat){
	inds.start = NULL
	inds.start[1] = na.inds[1] 
	inds.end = NULL
	k = 1
	for(i in 2:length(na.inds)){
		if(na.inds[i] - na.inds[i-1] > 1){
			k = k + 1
			inds.start[k] = na.inds[i]
			inds.end[k - 1] = na.inds[i - 1] 
		}
	}
	inds.end[k] = na.inds[length(na.inds)]

	for(i in 1:k){
		dat[inds.start[i]:inds.end[i]] = mean(dat[inds.start[i] - 1], dat[inds.end[i] + 1])
	}
	return(dat)
}
regr_tab = function(reg_model, model_name){
	regr_tab = data.frame(reg_model$coefficients)
	colnames(regr_tab) = colnames(reg_model$coefficients)
	regr_tab$Parameter = rownames(regr_tab)
	regr_tab[ ,4] = ifelse(regr_tab[ ,4] < .001, "< 0.001", 
                      ifelse(regr_tab[ ,4] < .01, "< 0.01", 
                           round(regr_tab[ ,4], 3)))
	# format the table
	regr_tab$Model = model_name
	return(regr_tab)
}
options(contrasts=c("contr.treatment","contr.poly"))

##user inputs
dir_dat = '/Users/danbroman/Documents/Heat Stress/data/'
dir_out = '/Users/danbroman/Documents/Heat Stress/data/'
run_name = 'wafr-heatstress'
covar = 'no-covar'
sta_name_sel = 'OUAGADOUGOU'
nsim = 200
pred_flag = T 

##process data
weather_data = readRDS(paste0(dir_dat, run_name, '.rda'))
weather_data_fl = filter(weather_data, name == sta_name_sel)
weather_data_fl = weather_data_fl %>% ungroup() %>% dplyr::arrange(date) %>% dplyr::mutate(prcpocc = ifelse(prcp > 0, 1, 0)) %>% data.table()
eind = nrow(weather_data_fl)
start_date = floor_date(weather_data_fl$date[1], 'month')
end_date = weather_data_fl$date[eind]
date_dt = data.table(date = seq(from = start_date, to = end_date, by = 'days'))
weather_data_fl = date_dt %>% left_join(weather_data_fl) %>% dplyr::mutate(year = year(date), month = month(date), day = day(date), jd = as.numeric(format(date, '%j')))
nod = nrow(weather_data_fl)
prcp = weather_data_fl$prcp
prcp.occ = weather_data_fl$prcpocc
tmin = weather_data_fl$tmin
tmax = weather_data_fl$tmax
rhmin = weather_data_fl$rhmin
rhmax = weather_data_fl$rhmax
windmin = weather_data_fl$windmin
windmax = weather_data_fl$windmax
sradmean  = weather_data_fl$srad

jd_seq = as.numeric(as.matrix(dplyr::select(weather_data_fl, jd)))
years = unique(weather_data_fl$year)
noy = length(years)
nom = length(unique(paste0(weather_data_fl$year, '-', weather_data_fl$month)))
noy1 = noy + 1
leap = ifelse(years %% 4 == 0, 1, 0)

inds.yrend = c(which(diff(weather_data_fl$year) != 0), nod)
inds.yrstart = c(1, inds.yrend[-length(inds.yrend)] + 1) 
yr.length = weather_data_fl$jd[inds.yrend]

daycos = NULL
daysin = NULL
daycos2x = NULL
daysin2x = NULL
daycos3x = NULL
daysin3x = NULL
daycos4x = NULL
daysin4x = NULL
daycos5x = NULL
daysin5x = NULL
	
for(i in 1:noy) {
	int = 2 * pi / yr.length[i] * 1:yr.length[i] 
	daycos = c(daycos, cos(int)) #cosine wave for seasonal cycle
	daysin = c(daysin, sin(int)) #sine wave for seasonal cycle
  	daycos2x = c(daycos2x, cos(int*2)) 
  	daysin2x = c(daysin2x, sin(int*2)) 
  	daycos3x = c(daycos3x, cos(int*3)) 
  	daysin3x = c(daysin3x, sin(int*3)) 
  	daycos4x = c(daycos4x, cos(int*4)) 
  	daysin4x = c(daysin4x, sin(int*4)) 
  	daycos5x = c(daycos5x, cos(int*5)) 
  	daysin5x = c(daysin5x, sin(int*5)) 
}

daycos1 = daycos[-1] #removes first value to consider lag-1 
daysin1 = daysin[-1] #removes first value to consider lag-1 
daycos2x1 = daycos2x[-1] #removes first value to consider lag-1
daysin2x1 = daysin2x[-1] #removes first value to consider lag-1
daycos3x1 = daycos3x[-1] #removes first value to consider lag-1
daysin3x1 = daysin3x[-1] #removes first value to consider lag-1
daycos4x1 = daycos4x[-1] #removes first value to consider lag-1
daysin4x1 = daysin4x[-1] #removes first value to consider lag-1
daycos5x1 = daycos5x[-1] #removes first value to consider lag-1
daysin5x1 = daysin5x[-1] #removes first value to consider lag-1

daycosresid = cos(2 * pi / 366 * 1:366)
daysinresid = sin(2 * pi / 366 * 1:366)
daycosresidx2 = cos(4 * pi / 366 * 1:366)
daysinresidx2 = sin(4 * pi / 366 * 1:366)
daycosresidx3 = cos(6 * pi / 366 * 1:366)
daysinresidx3 = sin(6 * pi / 366 * 1:366)
daycosresidx4 = cos(8 * pi / 366 * 1:366)
daysinresidx4 = sin(8 * pi / 366 * 1:366)
daycosresidx5 = cos(10 * pi / 366 * 1:366)
daysinresidx5 = sin(10 * pi / 366 * 1:366)

jd_seq1 = jd_seq[-1]

prcp.occ1 = prcp.occ[-1] #removes first value to consider lag-1 
prcp.occn = prcp.occ[-nod] #removes first value to consider lag-1 
	
prcp.int = prcp[prcp.occ == 1] #only considers wet days

tmin1 = tmin[-1] #removes first value to consider lag-1 
tminn = tmin[-nod] #removes last value; lag-1 vector
tmax1 = tmax[-1] #removes first value to consider lag-1 
tmaxn = tmax[-nod] #removes last value; lag-1 vector

rhmin = rhmin / 100 #convert to 0 to 1
rhmax = rhmax / 100 #convert to 0 to 1
rhmin[which(rhmin >1)] = 1
rhmax[which(rhmax >1)] = 1
rhmin1 = rhmin[-1] #removes first value to consider lag-1
rhminn = rhmin[-nod] #removes last value; lag-1 vector
rhmax1 = rhmax[-1] #removes first value to consider lag-1
rhmaxn = rhmax[-nod] #removes last value; lag-1 vector

windmin1 = windmin[-1] #removes first value to consider lag-1
windminn = windmin[-nod] #removes last value; lag-1 vector
windmax1 = windmax[-1] #removes first value to consider lag-1
windmaxn = windmax[-nod] #removes last value; lag-1 vector

sradmean1 = sradmean[-1] #removes first value to consider lag-1
sradmeann = sradmean[-nod] #removes last value; lag-1 vector

##fit models
#prcp occurrence
occ.dat = data.frame(prcp = as.factor(prcp.occ1), prcp.occn, daycos1, daysin1, daycos2x1, daysin2x1, daysin3x1, daycos3x1, daycosprcpocc = daycos1*prcp.occn, daysinprcpocc = daysin1*prcp.occn)

glm.occ = glm(prcp ~., data = occ.dat, family=binomial(), na.action=na.exclude)

#prcp intensity
int.dat = data.frame(prcp = prcp.int, daycos = daycos[prcp.occ == 1], daysin = daysin[prcp.occ == 1], daycos2x1 = daycos2x1[prcp.occ == 1], daysin2x1 = daysin2x1[prcp.occ == 1], daycos3x1 = daycos3x1[prcp.occ == 1], daysin3x1 = daysin3x1[prcp.occ == 1])

glm.int = glm(prcp ~., data = int.dat, na.action=na.exclude)  

#temperature
tmin.dat = data.frame(tmin1, tminn, tmaxn, daycos1, daysin1, daycos2x1, daysin2x1, daysin3x1, daycos3x1, daysin4x1, daycos4x1, daysin5x1, daycos5x1, prcp.occ1)
tmax.dat = data.frame(tmax1, tmaxn, tmin1, daycos1, daysin1, daycos2x1, daysin2x1, daysin3x1, daycos3x1, daysin4x1, daycos4x1, daysin5x1, daycos5x1, prcp.occ1)

lm.tmin = lm(tmin1 ~., data = tmin.dat, na.action=na.exclude)
lm.tmax = lm(tmax1 ~., data = tmax.dat, na.action=na.exclude)

tmin.resid.dat = data.table(jd = jd_seq1, resid = resid(lm.tmin))
tmin.resid.dat = group_by(tmin.resid.dat, jd)
tmin.resid.sd = summarise(tmin.resid.dat, sd = sd(resid, na.rm = T))

tmin.resid.dat2 = data.frame(sd = log(tmin.resid.sd$sd), daycosresid, daysinresid, daycosresidx2, daysinresidx2, daycosresidx3, daysinresidx3, daycosresidx4, daysinresidx4, daycosresidx5, daysinresidx5)
lm.tmin.resid = lm(sd ~., data = tmin.resid.dat2, na.action=na.exclude)

tmax.resid.dat = data.table(jd = jd_seq1, resid = resid(lm.tmax))
tmax.resid.dat = group_by(tmax.resid.dat, jd)
tmax.resid.sd = summarise(tmax.resid.dat, sd = sd(resid, na.rm = T))

tmax.resid.dat2 = data.frame(sd = log(tmax.resid.sd$sd), daycosresid, daysinresid, daycosresidx2, daysinresidx2, daycosresidx3, daysinresidx3, daycosresidx4, daysinresidx4, daycosresidx5, daysinresidx5)
lm.tmax.resid = lm(sd ~., data = tmax.resid.dat2, na.action=na.exclude)

#relative humidity
rhmin.dat = data.frame(rhmin1, rhminn, rhmaxn, daycos1, daysin1, daycos2x1, daysin2x1, daysin3x1, daycos3x1, prcp.occ1, tmin1, tmax1, tminn, tmaxn)
rhmax.dat = data.frame(rhmax1, rhmaxn, rhmin1, daycos1, daysin1, daycos2x1, daysin2x1, daysin3x1, daycos3x1, prcp.occ1, tmin1, tmax1, tminn, tmaxn)

glm.rhmin = suppressWarnings(glm(rhmin1 ~., data = rhmin.dat, family = binomial(), na.action=na.exclude))
glm.rhmax = suppressWarnings(glm(rhmax1 ~., data = rhmax.dat, family = binomial(), na.action=na.exclude))

rhmin.resid.dat = data.table(jd = jd_seq1, resid = resid(glm.rhmin))
rhmin.resid.dat = group_by(rhmin.resid.dat, jd)
rhmin.resid.sd = summarise(rhmin.resid.dat, sd = sd(resid, na.rm = T))

rhmin.resid.dat2 = data.frame(sd = log(rhmin.resid.sd$sd), daycosresid, daysinresid, daycosresidx2, daysinresidx2, daycosresidx3, daysinresidx3,  daycosresidx4, daysinresidx4, daycosresidx5, daysinresidx5)
lm.rhmin.resid = lm(sd ~., data = rhmin.resid.dat2, na.action=na.exclude)

rhmax.resid.dat = data.table(jd = jd_seq1, resid = resid(glm.rhmax))
rhmax.resid.dat = group_by(rhmax.resid.dat, jd)
rhmax.resid.sd = summarise(rhmax.resid.dat, sd = sd(resid, na.rm = T))

rhmax.resid.dat2 = data.frame(sd = log(rhmax.resid.sd$sd), daycosresid, daysinresid, daycosresidx2, daysinresidx2, daycosresidx3, daysinresidx3, daycosresidx4, daysinresidx4, daycosresidx5, daysinresidx5)
lm.rhmax.resid = lm(sd ~., data = rhmax.resid.dat2, na.action=na.exclude)

#winds
windmin.dat = data.frame(windmin1, windminn, windmaxn, daycos1, daysin1, daycos2x1, daysin2x1, daysin3x1, daycos3x1, tmin1, tmax1, tminn, tmaxn)
windmax.dat = data.frame(windmax1, windminn, windmaxn, windmin1, daycos1, daysin1, daycos2x1, daysin2x1, daysin3x1, daycos3x1, tmin1, tmax1, tminn, tmaxn)

lm.windmin = lm(windmin1 ~., data = windmin.dat, na.action=na.exclude) #fit linear model
lm.windmax = lm(windmax1 ~., data = windmax.dat, na.action=na.exclude) #fit linear model

windmin.resid.dat = data.table(jd = jd_seq1, resid = resid(lm.windmin))
windmin.resid.dat = group_by(windmin.resid.dat, jd)
windmin.resid.sd = summarise(windmin.resid.dat, sd = sd(resid, na.rm = T))

windmin.resid.dat2 = data.frame(sd = log(windmin.resid.sd$sd), daycosresid, daysinresid, daycosresidx2, daysinresidx2, daycosresidx3, daysinresidx3)
lm.windmin.resid = lm(sd ~., data = windmin.resid.dat2, na.action=na.exclude)

windmax.resid.dat = data.table(jd = jd_seq1, resid = resid(lm.windmax))
windmax.resid.dat = group_by(windmax.resid.dat, jd)
windmax.resid.sd = summarise(windmax.resid.dat, sd = sd(resid, na.rm = T))

windmax.resid.dat2 = data.frame(sd = log(windmax.resid.sd$sd), daycosresid, daysinresid, daycosresidx2, daysinresidx2, daycosresidx3, daysinresidx3)
lm.windmax.resid = lm(sd ~., data = windmax.resid.dat2, na.action=na.exclude)

#solar radiation
sradmean.dat = data.frame(sradmean1, sradmeann, daycos1, daysin1, daycos2x1, daysin2x1, daysin3x1, daycos3x1, prcp.occ1, prcp.occn, tmin1, tmax1, tminn, tmaxn)

lm.sradmean = lm(sradmean1 ~., data = sradmean.dat, na.action = na.exclude)

sradmean.resid.dat = data.table(jd = jd_seq1, resid = resid(lm.sradmean, typ = 'working'))
sradmean.resid.dat = group_by(sradmean.resid.dat, jd)
sradmean.resid.sd = summarise(sradmean.resid.dat, sd = sd(resid, na.rm = T))

sradmean.resid.dat2 = data.frame(sd = log(sradmean.resid.sd$sd), daycosresid, daysinresid, daycosresidx2, daysinresidx2, daycosresidx3, daysinresidx3)
lm.sradmean.resid = lm(sd ~., data = sradmean.resid.dat2, na.action=na.exclude)

if(pred_flag == T){ 
	coefocc  = glm.occ$coef #beta values of prcp occurrence model
  	sigmaocc = sd(resid(glm.occ, typ = 'working'), na.rm = T)
  	coefint  = glm.int$coef #beta values if prcp intensity model
	# intshape = gamma.shape(glm.int)$alpha #gamma shape parameter of prcp intensity model
	sigmatmin = exp(lm.tmin.resid$fitted.values) #standard deviation of tmin model errors
	sigmatmax = exp(lm.tmax.resid$fitted.values) #standard deviation of tmax model errors
	coeftmin = lm.tmin$coef #beta values of tmin model		
	coeftmax = lm.tmax$coef #beta values of tmax model
  	sigmarhmin = exp(lm.rhmin.resid$fitted.values)  
  	sigmarhmax = exp(lm.rhmax.resid$fitted.values)  
	coefrhmin = glm.rhmin$coef #beta values of rhmin model
	coefrhmax = glm.rhmax$coef #beta values of rhmax model
	sigmawindmin = exp(lm.windmin.resid$fitted.values) #standard deviation of windmin model errors
	sigmawindmax = exp(lm.windmax.resid$fitted.values) #standard deviation of windmax model errors
	coefwindmin = lm.windmin$coef #beta values of windmin model
	coefwindmax = lm.windmax$coef #beta values of windmax model
	sigmasradmean = exp(lm.sradmean.resid$fitted.values) #standard deviation of sradmean model errors 
	coefsradmean = lm.sradmean$coef #beta values of sradmean model
  
	cl = makeCluster(7)
	registerDoParallel(cl)
	wgen_sims = foreach(icount(nsim)) %dopar% { #simulation loop
		library(dplyr)
		library(tidyr)
		library(data.table)
		library(boot)
		#temp simulation vars
		prcp.occ.sim = rep(0,nod)
		prcp.int.sim = rep(0,nod)
		tmin.sim = rep(0,nod)
		tmax.sim = rep(0,nod)
		rhmin.sim = rep(0,nod)
		rhmax.sim = rep(0,nod)
		windmin.sim = rep(0,nod)
		windmax.sim = rep(0,nod)
		sradmean.sim = rep(0,nod)

		#starting values
	  	var.init = data.table(prcp.occ = prcp.occ[inds.yrstart], tmin = tmin[inds.yrstart], tmax = tmax[inds.yrstart], rhmin = rhmin[inds.yrstart], rhmax = rhmax[inds.yrstart], windmin = windmin[inds.yrstart], windmax = windmax[inds.yrstart], sradmean = sradmean[inds.yrstart]) %>% dplyr::filter(!is.na(prcp.occ))
	  	ind.init  = 1:nrow(var.init)
	  	ind.j1 = sample(ind.init, 1) #randomly select index of a jan 1
	  	prcp.occ.sim[1]  = var.init$prcp.occ[ind.j1]
	  	tmin.sim[1]  = var.init$tmin[ind.j1]
	  	tmax.sim[1]  = var.init$tmax[ind.j1]
	  	rhmin.sim[1]  = var.init$rhmin[ind.j1]
	  	rhmax.sim[1]  = var.init$rhmax[ind.j1]
	  	windmin.sim[1]  = var.init$windmin[ind.j1]
	  	windmax.sim[1]  = var.init$windmax[ind.j1]
		sradmean.sim[1]  = var.init$sradmean[ind.j1]
		for (k in 2:nod){
	      	jd.simday = jd_seq[k] 
			covocc = c(1, prcp.occ.sim[k - 1], daycos[k], daysin[k], daycos2x[k], daysin2x[k], daycos3x[k], daysin3x[k], daycos[k]*prcp.occ.sim[k - 1], daysin[k]*prcp.occ.sim[k - 1])					
			occerr = rnorm(1, 0, sigmaocc) 
			pk = exp(coefocc %*% covocc + occerr) / (1 + exp(coefocc %*% covocc + occerr))
			prcp.occ.sim[k] = ifelse(runif(1) < pk, 1, 0)
				
			covtmin  = c(1, tmin.sim[k-1], tmax.sim[k-1], daycos[k], daysin[k], daycos2x[k], daysin2x[k], daycos3x[k], daysin3x[k], daycos4x[k], daysin4x[k], daycos5x[k], daysin5x[k], prcp.occ.sim[k])
			tmin.sim[k] = coeftmin %*% covtmin + rnorm(1, 0, sigmatmin)
			covtmax  = c(1, tmax.sim[k-1], tmin.sim[k], daycos[k], daysin[k], daycos2x[k], daysin2x[k], daycos3x[k], daysin3x[k], daycos4x[k], daysin4x[k], daycos5x[k], daysin5x[k], prcp.occ.sim[k])
			tmax.sim[k] = coeftmax %*% covtmax + rnorm(1, 0, sigmatmax[jd.simday])
					
			ta.temp  = c(tmax.sim[k], tmin.sim[k])
			tmin.sim[k]  = min(ta.temp)
			tmax.sim[k]  = max(ta.temp)
	          				
			covrhmin = c(1, rhmin.sim[k-1], rhmax.sim[k-1],daycos[k], daysin[k], daycos2x[k], daysin2x[k], daycos3x[k], daysin3x[k], prcp.occ.sim[k], tmin.sim[k], tmax.sim[k], tmin.sim[k-1], tmax.sim[k-1])
			rhmin.sim[k] = inv.logit(coefrhmin %*% covrhmin) + rnorm(1, 0, sigmarhmin[jd.simday])
			covrhmax = c(1, rhmax.sim[k-1], rhmin.sim[k], daycos[k], daysin[k], daycos2x[k],daysin2x[k], daycos3x[k], daysin3x[k], prcp.occ.sim[k], tmin.sim[k], tmax.sim[k], tmin.sim[k-1], tmax.sim[k-1]) 
			rhmax.sim[k] = inv.logit(coefrhmax %*% covrhmax) + rnorm(1, 0, sigmarhmax[jd.simday])

	  		rh.temp  = c(rhmax.sim[k], rhmin.sim[k])
	  		rhmin.sim[k]  = ifelse(min(rh.temp) < 0, 0, min(rh.temp))
	  		rhmax.sim[k]  = ifelse(max(rh.temp) < 0, 0, max(rh.temp))
	  		rhmax.sim[k] = ifelse(rhmax.sim[k] > 1, 1, rhmax.sim[k])
			##WINDS
			covwindmin = c(1, windmin.sim[k-1], windmax.sim[k-1], daycos[k], daysin[k], daycos2x[k], daysin2x[k], daycos3x[k], daysin3x[k], tmin.sim[k], tmax.sim[k], tmin.sim[k-1], tmax.sim[k-1])
			windmin.sim[k] = coefwindmin %*% covwindmin + rnorm(1,0,sigmawindmin[jd.simday])
			covwindmax = c(1, windmin.sim[k-1], windmax.sim[k-1],windmin.sim[k], daycos[k], daysin[k], daycos2x[k],daysin2x[k], daycos3x[k], daysin3x[k], tmin.sim[k], tmax.sim[k], tmin.sim[k-1], tmax.sim[k-1])
			windmax.sim[k] = coefwindmax %*% covwindmax + rnorm(1,0,sigmawindmax[jd.simday])

			wind.temp  = c(windmin.sim[k], windmax.sim[k])
			windmin.sim[k] = ifelse(min(wind.temp) >= 0, min(wind.temp), 0) 
			windmax.sim[k] = ifelse(max(wind.temp) >= 0, max(wind.temp), 0)   
	  
				##SOLAR RAD.
			covsradmean = c(1, sradmean.sim[k-1], daycos[k], daysin[k],daycos2x[k], daysin2x[k], daycos3x[k], daysin3x[k], prcp.occ.sim[k], prcp.occ.sim[k-1], tmin.sim[k], tmax.sim[k], tmin.sim[k-1], tmax.sim[k-1])

			sradmean.sim[k] = coefsradmean %*% covsradmean + rnorm(1,0,sigmasradmean[jd.simday])
			sradmean.sim[k] = ifelse(sradmean.sim[k] >= 0, sradmean.sim[k], 0)
			} #end 'nod' loop (k)
		
		covint = cbind(1, daycos[which(prcp.occ.sim == 1)], daysin[which(prcp.occ.sim == 1)], daycos2x[which(prcp.occ.sim == 1)], daysin2x[which(prcp.occ.sim == 1)], daycos3x[which(prcp.occ.sim == 1)], daysin3x[which(prcp.occ.sim == 1)])
    
		int.temp = as.numeric(t(as.matrix(coefint)) %*% t(covint))
		int.temp[int.temp < 0] = 0
	  	# intmu = exp(abs(apply(coefint * covint, FUN = sum, MAR = 1, na.rm = T)))
	  	# intscale = intmu / intshape
	  	# # length(which(prcp.occ.sim == 1))
	  	# int.temp = rgamma(nod, intshape, scale = intscale)
		prcp.int.sim[which(prcp.occ.sim == 1)] = int.temp
		
		data.table(prcpocc = prcp.occ.sim, prcp = prcp.int.sim, tmin = tmin.sim, tmax = tmax.sim, rhmin = rhmin.sim * 100, rhmax = rhmax.sim * 100, windmin = windmin.sim, windmax = windmax.sim, srad = sradmean.sim)

	} #end simulation loop (i)
	stopCluster(cl)
 	wgen_sims_dt = do.call('bind_rows', wgen_sims) %>% dplyr::mutate(date = rep(seq(from = start_date, to = end_date, by = 'days'), nsim), sim = rep(1:nsim, each = nrow(date_dt)))
	saveRDS(wgen_sims_dt, file = paste0(dir_out, sta_name_sel, '_', run_name, '_', covar, '.rda'))
} #end pred 


anova_tbl = NULL
anova_tbl = bind_rows(anova_tbl, regr_tab(summary(lm.tmin), 'tmin'))
anova_tbl = bind_rows(anova_tbl, regr_tab(summary(lm.tmax), 'tmax'))
anova_tbl = bind_rows(anova_tbl, regr_tab(summary(glm.rhmin), 'rhmin'))
anova_tbl = bind_rows(anova_tbl, regr_tab(summary(glm.int), 'rhmax'))
anova_tbl = bind_rows(anova_tbl, regr_tab(summary(lm.windmin), 'windmin'))
anova_tbl = bind_rows(anova_tbl, regr_tab(summary(glm.rhmax), 'rhmax'))
anova_tbl = bind_rows(anova_tbl, regr_tab(summary(lm.windmax), 'windmax'))
anova_tbl = bind_rows(anova_tbl, regr_tab(summary(lm.sradmean), 'sradmean'))
anova_tbl = bind_rows(anova_tbl, regr_tab(summary(glm.occ), 'prcpocc'))
anova_tbl = bind_rows(anova_tbl, regr_tab(summary(glm.int), 'prcp'))
write.csv(anova_tbl, paste0(dir_out, sta_name_sel, '_', run_name, '_', covar, '_anova.csv'), row.names = F)
