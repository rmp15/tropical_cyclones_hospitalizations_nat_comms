# this code will take a particular cause of death
# and run the chosen model for it

# NEED TO RUN ON RCE

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
seedVal = as.numeric(args[1]) + 1

# years of analysis
years = c(1999:2014)

# expand grid of stuff (temporary)
# cod.arg = 'Congestive heart failure; nonhypertensive' ; cod.arg = gsub(" ", "_", cod.arg)

# ccs names (temporarily keeping just 10 causes of death which we thought might be interesting)
ccs.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('css_category','full_name')
ccs.names$full_name = as.character(ccs.names$full_name)
cods = ccs.names$full_name

# temporary run of 20 causes as initial test
# cods.1 = c('Congestive heart failure; nonhypertensive','Pneumonia (except that caused by tuberculosis or sexually transmitted disease)',
#         'Coronary atherosclerosis and other heart disease','Cardiac dysrhythmias','Chronic obstructive pulmonary disease and bronchiectasis',
#         'Acute myocardial infarction','Septicemia (except in labor)','Acute cerebrovascular disease',
#         'Fracture of neck of femur (hip)','Respiratory failure; insufficiency; arrest (adult)')
# cods.2 = c('Fluid and electrolyte disorders','Syncope',
#         'Secondary malignancies','Cancer of bronchus; lung','Cancer of colon',
#         'Other connective tissue disease','Cancer of pancreas','Alcohol-related disorders',
#         'Hemorrhoids','Ovarian cyst')
# cods = c(cods.1,cods.2)

# create complete grid of age, sex, and cause of death values
lags=c(0:7)
# seed.grid = expand.grid(lag=lags,cause=ccs.names[,2])
seed.grid = expand.grid(lag=lags,cause=cods)
chosen.row =seed.grid[seedVal,]

# set lag and cause of hospitalisation from seed value
lag = as.numeric(chosen.row[1,1])
cod.arg = as.character(chosen.row[1,2]) ; cod.arg = gsub(" ", "_", cod.arg)

# print arguments
print(cod.arg)
print(lag)

# directory to load data from
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/expanded_grid_hospitalisations/',years[1],'_',years[length(years)],'/')

# load cod data
dat = readRDS(paste0(dir.input,'medicare_',gsub(" ", "_", cod.arg),'_rates_expanded_grid_hospitalisations_',years[1],'_',years[length(years)],'.rds'))

# attach temperature and precipitation data (precipitation not for now)
dat.temp = data.frame()
dat.precip = data.frame()
dir.input.weather = "~/git/pollution/countries/USA/output/grid_county_intersection_raster_prism/fips/"
for(year in years){
    dat.temp.current=readRDS(paste0(dir.input.weather ,'weighted_area_raster_fips_tmean_daily_',as.character(year),'.rds'))
    dat.temp = rbind(dat.temp,dat.temp.current)

    #dat.precip.current=readRDS(paste0(dir.input.weather ,'weighted_area_raster_fips_ppt_daily_',as.character(year),'.rds'))
    #dat.precip = rbind(dat.precip,dat.precip.current)
    print(year)
}
dat.temp = dat.temp[,c(1,2,4:6)] ; #dat.precip = dat.precip[,c(1,2,4:6)]
dat.temp$fips = as.numeric(as.character(dat.temp$fips)) ; #dat.precip$fips = as.numeric(as.character(dat.precip$fips))
dat.temp$day = as.numeric(as.character(dat.temp$day)) ; #dat.precip$day = as.numeric(as.character(dat.precip$day))
dat.temp$month = as.numeric(as.character(dat.temp$month)) ; #dat.precip$month = as.numeric(as.character(dat.precip$month))
dat.temp$year = as.numeric(as.character(dat.temp$year)) ;# dat.precip$year = as.numeric(as.character(dat.precip$year))

dat = merge(dat,dat.temp,by.x=c('fipscounty','day','month','year'),by.y=c('fips','day','month','year'),all.x=TRUE)
# dat = merge(dat,dat.precip,by.x=c('fipscounty','day','month','year'),by.y=c('fips','day','month','year'),all.x=TRUE)

head(dat)

# sample for model testing
dat.sample = subset(dat,year%in%years)

# make year numeric
dat.sample$year = as.numeric(dat.sample$year)

# add missing stuff which happened because I am bad at planning
dat.sample$date = with(dat.sample,paste0(day,'/',month,'/',year))
dat.sample$date = as.Date(dat.sample$date, format="%d/%m/%Y")
dat.sample$dow = weekdays(dat.sample$date)

# fix year to be numeric
dat.sample$year = as.numeric(dat.sample$year)

library(plyr)

# create log of population
dat.sample$logpop <- log(dat.sample$population)

# create stratum with dow and fipscounty and year
dat.sample$stratum = as.factor(as.factor(dat.sample$fipscounty):as.factor(dat.sample$month):as.factor(dat.sample$dow))
# dat.sample$stratum.alt = as.factor(as.factor(dat.sample$month):as.factor(dat.sample$dow))

# reattach coastal storm event data as would need to add coastal storm events which are associated with zero cases on a fipcounty-day
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
counties.wind = readRDS(paste0(dir.input,'wind_data_',years[1],'_',years[length(years)],'.rds'))
counties.wind.all = readRDS(paste0(dir.input,'wind_data_all_',years[1],'_2016.rds'))

# make fips numeric
counties.wind$fips = as.numeric(counties.wind$fips)

# add lag to the date before matching
# function to add year, month, and day to dataset from lubridate function
add_date_info = function(dat, lag_chosen){
    dat$closest_date = as.Date(dat$closest_date, format="%Y-%m-%d") + lag_chosen
    dat$year     = lubridate::year(dat$closest_date)
    dat$month    = lubridate::month(dat$closest_date)
    dat$day      = lubridate::day(dat$closest_date)

    col_name     = paste0('event_lag',lag_chosen)
    dat[col_name]= 1

    dat = dat[
      with(dat, order(year,month,day)),
    ]

    dat = dat[,c('fips','year','month','day',col_name)]

    return(dat)
}


# calculate lag values for the unconstrained lag model
for(lag in lags){
    assign(paste0('counties.wind.edit.',lag), add_date_info(counties.wind, lag))
    print(head(get(paste0('counties.wind.edit.',lag))))

}

# single lag
counties.wind.edit = add_date_info(counties.wind, lag)
counties.wind.edit$fips = as.numeric(counties.wind.edit$fips)
dat.merged.single = merge(dat.sample,counties.wind.edit,by.x=c('year','month','day','fipscounty'),
                                                by.y=c('year','month','day','fips'),all.x=TRUE)

# add multiple lags
dates = seq(as.Date("1999-01-01"), as.Date("2014-12-31"), by="days")
fipscounty = sort(unique(as.numeric(dat$fips)))
complete.grid = expand.grid(date=dates,fips=fipscounty)
complete.grid$year = format(complete.grid$date, '%Y') ; complete.grid$year = as.numeric(complete.grid$year)
complete.grid$month = as.numeric(format(complete.grid$date, '%m'))
complete.grid$day = as.numeric(format(complete.grid$date, '%d'))
complete.grid$date = NULL
counties.wind.edit.array = complete.grid
for(lag in lags){
    print(paste0('Matching lag ',lag))
    counties.wind.edit.array = merge(counties.wind.edit.array,get(paste0('counties.wind.edit.',lag)),by=c('fips','year','month','day'), all.x=TRUE)
}
counties.wind.edit.array[is.na(counties.wind.edit.array)] <- 0

# multiple lags merge with hospitalization data
dat.sample.multiple = dat.sample
# dat.sample.multiple$fipscounty = as.character(dat.sample.multiple$fipscounty)
dat.merged.multiple = merge(dat.sample.multiple,counties.wind.edit.array,by=c('year','month','day','fipscounty'),
                                                by.y=c('year','month','day','fips'),all.x=TRUE)

# optional if you want for all counties
# counties.wind.all.edit = counties.wind.all
dat.merged.single.all = merge(dat.sample.multiple,counties.wind.all.edit,by.x=c('year','month','day','fipscounty'),
                                                by.y=c('year','month','day','fips'),all.x=TRUE)

# check missing values
dat.merged.na = dat.merged.single[rowSums(is.na(dat.merged.single)) > 0,]
dat.merged.multiple.na = dat.merged.multiple[rowSums(is.na(dat.merged.multiple)) > 0,]

# class days as event days or not
dat.merged.single$event=ifelse(is.na(dat.merged.single$storm_id)==FALSE,1,0)

dat.sample = dat.merged.single

# only include fips which have both events and non-events for single lag
dat.event.test = ddply(dat.sample, .(fipscounty), summarize, NumSubs = length(unique(event)))
dat.event.test = subset(dat.event.test,NumSubs<2)
fips_to_exclude = as.numeric(as.character(dat.event.test$fipscounty))

dat.sample = subset(dat.sample,!(fipscounty%in%fips_to_exclude))

# only include fips which have both events and non-events for multiple lag
dat.event.multiple.test = ddply(dat.merged.multiple, .(fipscounty), summarize, NumSubs = length(unique(event_lag0)))
dat.event.multiple.test = subset(dat.event.multiple.test,NumSubs<2)
fips_to_exclude = as.numeric(as.character(dat.event.multiple.test$fipscounty))

dat.sample.multiple = subset(dat.merged.multiple,!(fipscounty%in%fips_to_exclude))

library(gnm) ; library(splines) ; library(dlnm)

# for dnlm
cb1.event <- crossbasis(dat.sample$event, lag=7, argvar=list(fun="lin"), arglag=list(fun="poly",degree=4))
cb1.temp <- crossbasis(dat.sample$tmean, lag=3, argvar=list(df=5), arglag=list(fun="strata",breaks=1))
cb1.temp.multiple.lag <- crossbasis(dat.sample.multiple$tmean, lag=3, argvar=list(df=5), arglag=list(fun="strata",breaks=1))
# cb1.temp <- crossbasis(dat.sample$tmean, lag=27, argvar=list("lin"), arglag=list(fun="ns",knots=c(-20,-10,0,10,20)))

# stratified conditional poisson single lag
if(model.arg==1){
    system.time({
        mod = gnm(cases ~ event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)

        # save model results
        saveRDS(mod, paste0(dir.output,'medicare_',gsub(" ", "_", cod.arg),'_model_output_',years[1],'_',years[length(years)],'_lag',lag,'.rds'))

        # provide summary
        print(summary(mod))
        # confint(mod)
        print(exp(confint.default(mod)))

    })
}

# distributed lag unconstrained
if(model.arg==2){
    system.time({

    # mod_dlm_unconstrained = gnm(cases ~ event_lag0 + event_lag1 + event_lag2 + event_lag3 + event_lag4 + event_lag5 + event_lag6 + event_lag7 +
    #     ns(tmean, df=3) + ns(year, df=3) , data=dat.sample.multiple, offset=logpop, eliminate=factor(stratum), family=quasipoisson)

    mod_dlm_unconstrained = gnm(cases ~ event_lag0 + event_lag1 + event_lag2 + event_lag3 + event_lag4 + event_lag5 + event_lag6 + event_lag7 +
        cb1.temp.multiple.lag + ns(year, df=3) , data=dat.sample.multiple, offset=logpop, eliminate=factor(stratum), family=quasipoisson)

    # provide summary
    lag_est_mean = as.data.frame(exp(mod_dlm_unconstrained$coefficients[1:8]))
    lag_est_uncertainty = exp(confint.default(mod_dlm_unconstrained)[c(1:8),])

    dat.results = data.frame(lag=c(0:7),rr=lag_est_mean,rr.ll=lag_est_uncertainty[,1],rr.ul=lag_est_uncertainty[,2])
    names(dat.results)[2] = 'rr'

    print(
        ggplot(subset(dat.results)) +
        geom_point(aes(x=lag,y=rr)) +
        geom_errorbar(aes(x=lag,ymin=rr.ll,ymax=rr.ul)) +
        geom_hline(yintercept=1,linetype=2) +
        ggtitle(paste0(cod.arg,' (unconstrained dlm)')) +
        xlab('Lag') + ylab('RR') +
        ylim(c(0.8,1.3)) +
        theme_bw() + theme(text = element_text(size = 8),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
        )

    # visualise temperature
    pred1.temp <- crosspred(cb1.temp, mod_dlm_unconstrained, at=0:20, bylag=0.2, cumul=TRUE)
    plot(pred1.temp, "slices", var=1, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
    main=paste0("Association per 1°C for ",cod.arg = gsub("_", " ", cod.arg)))
})
}

# distributed lag constrained
if(model.arg==3){
    system.time({

    mod_dlm_constrained = gnm(cases ~ cb1.event + cb1.temp + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)

    # visualise wind event
    pred1.event <- crosspred(cb1.event, mod_dlm_constrained, at=0:1, bylag=0.2, cumul=TRUE)
    plot(pred1.event, "slices", var=1, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
    main=paste0(cod.arg = gsub("_", " ", cod.arg),' (constrained dlm)'))

    # visualise temperature
    pred1.temp <- crosspred(cb1.temp, mod_dlm_constrained, at=0:3, bylag=0.2, cumul=TRUE)
    plot(pred1.temp, "slices", var=1, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
    main=paste0("Association per 1°C for ",cod.arg = gsub("_", " ", cod.arg)))
})
}


# output directory
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/model/frequentist/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)



# save summary of model results in a way which can be compiled later
dat.results=data.frame(cause=cod.arg,lag=lag,rr=exp(mod$coefficients[1]),rr.ll=exp(confint.default(mod))[1,1],rr.ul=exp(confint.default(mod))[1,2])

# save model summary
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/summary/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
write.csv(dat.results, paste0(dir.output,'medicare_',gsub(" ", "_", cod.arg),'_model_summary_',years[1],'_',years[length(years)],'_lag',lag,'.csv'))

# LEGACY

    # mod = gnm(cases ~ (1|fipscounty) + event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)
    # mod.alt = gnm(cases ~ fipscounty + event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum.alt), family=quasipoisson)
    # mod.alt.2 = gnm(cases ~ fipscounty + event + tmean + year, data=dat.sample, offset=logpop, eliminate=factor(stratum.alt), family=quasipoisson)
    # mod.alt.2 = gnm(cases ~ fipscounty + event + tmean + year, data=dat.sample, offset=logpop, eliminate=factor(stratum.alt), family=quasipoisson)
    # mod.alt.3 = glm(formula = cases ~ fipscounty + stratum.alt + event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, family=quasipoisson)

    # mod = glm(formula = cases ~ fipscounty + month + dow + stratum.alt + event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, family=quasipoisson)
    # mod2 = gnm(cases ~  event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)
    # mod = gnm(cases ~ (1|fipscounty) + event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample[c(1:100),], offset=logpop, eliminate=factor(stratum), family=quasipoisson)
    # mod = gnm(cases ~ event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)
    # mod = glm(cases ~ event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)
    # mod = lmer(formula = cases ~ event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)
    # mod = gnm(cases ~ fipscounty + event + ns(tmean, df=3) + ns(year, df=3) , data=na.omit(dat.sample), offset=logpop, family=quasipoisson)
    # mod = gnm(cases ~ fipscounty + event , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)

    # with weather variables
    # add year variable
    # mod = gnm(cases ~ fipscounty + event + tmean , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)
    # mod = gnm(cases ~ fipscounty + event + tmean + ppt , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)