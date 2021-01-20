# this code will take a particular cause of death
# and run the chosen model for it

# NEED TO RUN ON RCE

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
seedVal = as.numeric(args[1]) + 1
lag = as.numeric(args[2])

# years of analysis
years = c(1999:2014)

# expand grid of stuff (temporary)
# cod.arg = 'Congestive heart failure; nonhypertensive' ; cod.arg = gsub(" ", "_", cod.arg)

# ccs names (temporarily keeping just 10 causes of death which we thought might be interesting)
ccs.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('css_category','full_name')
ccs.names$full_name = as.character(ccs.names$full_name)
# cods = ccs.names$full_name
cods.1 = c('Congestive heart failure; nonhypertensive','Pneumonia (except that caused by tuberculosis or sexually transmitted disease)',
        'Coronary atherosclerosis and other heart disease','Cardiac dysrhythmias','Chronic obstructive pulmonary disease and bronchiectasis',
        'Acute myocardial infarction','Septicemia (except in labor)','Acute cerebrovascular disease',
        'Fracture of neck of femur (hip)','Respiratory failure; insufficiency; arrest (adult)')
cods.2 = c('Fluid and electrolyte disorders','Syncope',
        'Secondary malignancies','Cancer of bronchus; lung','Cancer of colon',
        'Other connective tissue disease','Cancer of pancreas','Alcohol-related disorders',
        'Hemorrhoids','Ovarian cyst')
cods = c(cods.1,cods.2)
cod.arg = cods[seedVal] ; cod.arg = gsub(" ", "_", cod.arg)
print(cod.arg)

# directory to load data from
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/matched_wind_data/',years[1],'_',years[length(years)],'/')

# load cod data
dat = readRDS(paste0(dir.input,'medicare_',cod.arg,'_rates_matched_wind_events_',years[1],'_',years[length(years)],'.rds'))

# class days as event days or not
dat$event=as.factor(ifelse(is.na(dat$storm_id)==FALSE,1,0))

# day of the week
dat$date = with(dat,paste0(day,'/',month,'/',year))
dat$date = as.Date(dat$date, format="%d/%m/%Y")
dat$dow = weekdays(dat$date)

# fully expand dates, fips then match to loaded admissions data
dates = seq(as.Date("1999-01-01"), as.Date("2014-12-31"), by="days")
fipscounty = sort(unique(dat$fipscounty))
complete.grid = expand.grid(date=dates,fipscounty=fipscounty)
complete.grid$year = format(complete.grid$date, '%Y')
complete.grid$month = as.numeric(format(complete.grid$date, '%m'))
complete.grid$day = as.numeric(format(complete.grid$date, '%d'))
complete.grid$date = NULL

# merge deaths counts with complete grid to ensure there are rows with zero deaths
dat.complete = merge(complete.grid,dat,by=c('fipscounty','year','month','day'),all.x='TRUE')

# assign missing cases to have value 0
dat.complete$cases = ifelse(is.na(dat.complete$cases)==TRUE,0,dat.complete$cases)

# attach temperature and precipitation data (precipitation not for now)
dat.temp = data.frame()
dat.precip = data.frame()
dir.input.weather = "~/git/pollution/countries/USA/output/grid_county_intersection_raster_prism/fips/"
for(year in years){
    dat.temp.current=readRDS(paste0(dir.input.weather ,'weighted_area_raster_fips_tmean_daily_',as.character(year),'.rds'))
    dat.temp = rbind(dat.temp,dat.temp.current)

    dat.precip.current=readRDS(paste0(dir.input.weather ,'weighted_area_raster_fips_ppt_daily_',as.character(year),'.rds'))
    dat.precip = rbind(dat.precip,dat.precip.current)
    print(year)
}
dat.temp = dat.temp[,c(1,2,4:6)] ; dat.precip = dat.precip[,c(1,2,4:6)]
dat.temp$fips = as.numeric(as.character(dat.temp$fips)) ; dat.precip$fips = as.numeric(as.character(dat.precip$fips))
dat.temp$day = as.numeric(as.character(dat.temp$day)) ; dat.precip$day = as.numeric(as.character(dat.precip$day))
dat.temp$month = as.numeric(as.character(dat.temp$month)) ; dat.precip$month = as.numeric(as.character(dat.precip$month))
dat.temp$year = as.numeric(as.character(dat.temp$year)) ; dat.precip$year = as.numeric(as.character(dat.precip$year))

dat = merge(dat.complete,dat.temp,by.x=c('fipscounty','day','month','year'),by.y=c('fips','day','month','year'),all.x=TRUE)

head(dat)

# dat = merge(dat,dat.precip,by.x=c('fipscounty','day','month','year'),by.y=c('fips','day','month','year'),all.x=TRUE)

# sample for model testing
dat.sample = subset(dat,year%in%years)
dat.sample$fipscounty = as.factor(dat.sample$fipscounty)

# make year numeric
dat.sample$year = as.numeric(dat.sample$year)

# add missing stuff which happened because I am bad at planning
dat.sample$date = with(dat.sample,paste0(day,'/',month,'/',year))
dat.sample$date = as.Date(dat.sample$date, format="%d/%m/%Y")
dat.sample$dow = weekdays(dat.sample$date)

# fix year to be numeric
dat.sample$year = as.numeric(dat.sample$year)

# assign missing cases to have value 0
dat.sample$event = as.numeric(as.character(dat.sample$event))
dat.sample$event = ifelse(is.na(dat.sample$event)==FALSE,dat.sample$event,0)

library(plyr)

#load processed denom file and attach to sample file for model running
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/medicare_denom_processing/')
dat.denom = readRDS(paste0(dir.input,'medicare_denom_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.denom)=1:nrow(dat.denom)

# add population again
dat.sample$fipscounty = as.numeric(as.character(dat.sample$fipscounty))
dat.test = merge(dat.sample, dat.denom, by.x=c('year','fipscounty'),by.y=c('year','fips'),all.x=TRUE)
dat.test$population = dat.test$population.y ; dat.test$population.y = dat.test$population.x = NULL
dat.sample = dat.test

# create log of population
dat.sample$logpop <- log(dat.sample$population)

# run model (currently sample data) how to use more than one CPU though?
# random intercept for fipscounty
dat.sample$fipscounty = as.factor(dat.sample$fipscounty)

# create stratum with dow and fipscounty and year
dat.sample$stratum = as.factor(as.factor(dat.sample$fipscounty):as.factor(dat.sample$month):as.factor(dat.sample$dow))
dat.sample$stratum.alt = as.factor(as.factor(dat.sample$month):as.factor(dat.sample$dow))

# reattach coastal storm event data as would need to add coastal storm events which are associated with zero cases on a fipcounty-day
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
counties.wind = readRDS(paste0(dir.input,'wind_data_',years[1],'_',years[length(years)],'.rds'))

# add lag to the date before matching
# function to add year, month, and day to dataset from lubridate function
add_date_info = function(dat, lag_chosen){
    dat$closest_date = as.Date(dat$closest_date, format="%Y-%m-%d") + lag_chosen
    dat$year     = lubridate::year(dat$closest_date)
    dat$month    = lubridate::month(dat$closest_date)
    dat$day      = lubridate::day(dat$closest_date)

    dat = dat[
      with(dat, order(year,month,day)),
    ]

    return(dat)
}

counties.wind.edit = add_date_info(counties.wind, lag)

dat.merged.single = merge(dat.sample[,c(1:14,26:31)],counties.wind.edit,by.x=c('year','month','day','fipscounty'),
                                                by.y=c('year','month','day','fips'),all.x=TRUE)

# class days as event days or not
dat.merged.single$event=ifelse(is.na(dat.merged.single$storm_id)==FALSE,1,0)

dat.sample = dat.merged.single

# only include fips which have both events and non-events
dat.event.test = ddply(dat.sample, .(fipscounty), summarize, NumSubs = length(unique(event)))
dat.event.test = subset(dat.event.test,NumSubs<2)
fips_to_exclude = as.numeric(as.character(dat.event.test$fipscounty))

dat.sample = subset(dat.sample,!(fipscounty%in%fips_to_exclude))

library(gnm) ; library(splines) ; library(lme4)

system.time({

    # mod = gnm(cases ~ (1|fipscounty) + event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)
    mod = gnm(cases ~ event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)
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
})

# output directory
# dir.output.local = paste0('~/data/morbidity/US/medicare/results/model_run/wind_events/')
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# save model results
saveRDS(mod, paste0(dir.output,'medicare_',gsub(" ", "_", cod.arg),'_model_output_',years[1],'_',years[length(years)],'_lag',lag,'.rds'))

# provide summary
print(summary(mod))
# confint(mod)
print(exp(confint.default(mod)))