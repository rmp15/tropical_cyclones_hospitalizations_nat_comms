# this code will take a particular cause of death
# and run the chosen Bayesian model for it

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

# # temporary run of 20 causes as initial test
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
lags=c(-3:7)
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

# # assign missing cases to have value 0
# dat.sample$event = as.numeric(as.character(dat.sample$event))
# dat.sample$event = ifelse(is.na(dat.sample$event)==FALSE,dat.sample$event,0)

library(plyr)

# create log of population
dat.sample$logpop <- log(dat.sample$population)

# run model (currently sample data) how to use more than one CPU though?
# random intercept for fipscounty
dat.sample$fipscounty = as.factor(dat.sample$fipscounty)

# create stratum with dow and fipscounty and year
dat.sample$stratum = as.factor(as.factor(dat.sample$fipscounty):as.factor(dat.sample$month):as.factor(dat.sample$dow))

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

dat.merged.single = merge(dat.sample,counties.wind.edit,by.x=c('year','month','day','fipscounty'),
                                                by.y=c('year','month','day','fips'),all.x=TRUE)

# check missing values
dat.merged.na = dat.merged.single[rowSums(is.na(dat.merged.single)) > 0,]

# class days as event days or not
dat.merged.single$event=ifelse(is.na(dat.merged.single$storm_id)==FALSE,1,0)

dat.sample = dat.merged.single

# only include fips which have both events and non-events
dat.event.test = ddply(dat.sample, .(fipscounty), summarize, NumSubs = length(unique(event)))
dat.event.test = subset(dat.event.test,NumSubs<2)
fips_to_exclude = as.numeric(as.character(dat.event.test$fipscounty))

dat.sample = subset(dat.sample,!(fipscounty%in%fips_to_exclude))

dat.sample$e <- 1:nrow(dat.sample)

library(gnm) ; library(INLA) ; library(splines)

system.time({

    # frequentist model for reference (conditional Poisson)
    # mod = gnm(cases ~ event + ns(tmean, df=3) + ns(year, df=3) , data=dat.sample, offset=logpop, eliminate=factor(stratum), family=quasipoisson)

    # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
    fml  <- cases ~
    # global terms
    1 +                                                                     		# global intercept
    ns(year, df=3) +                                                           	    # year slopes
    ns(tmean, df=3) +                                                           	# temperature slopes
    f(stratum, model='iid', hyper = list(prec = list(prior = "normal", param = c(1, (1/100000))))) + # strata terms
    event +                                                                         # wind event 0/1
    f(e, model = "iid")                                                    		    # overdispersion term for quasipoisson

    # fml  <- cases ~
    # global terms
    # 1 +                                                                     		# global intercept
    # year +                                                           	            # year slopes
    # tmean +                                                           	            # temperature slopes
    # f(stratum, model='iid', hyper = list(prec = list(prior = "normal", param = c(1, (1/100000))))) + # strata terms
    # event +                                                                         # wind event 0/1
    # f(e, model = "iid")                                                    		    # overdispersion term for quasipoisson

})

# functions to enable age group and sex to be selected with faster AR1 structure in addition to rough run
inla.function.climate.faster <- function() {

    # INLA model rough
    system.time(mod.rough <-
    inla(formula = fml,
    family = "poisson",
    data = dat.sample,
    E = population,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    control.inla = list(diagonal=10000, int.strategy='eb',strategy='gaussian'),
    verbose=TRUE
    ))

    # INLA model proper
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.sample,
    E = population,
    control.compute = list(config=TRUE, dic=TRUE),
    control.predictor = list(link = 1),
    control.inla=list(diagonal=0),
    control.mode = list(result = mod.rough, restart = TRUE),
    #verbose=TRUE
    ))

    return(mod)
}

# output directory
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/model/bayesian/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# save model results
saveRDS(mod, paste0(dir.output,'medicare_',gsub(" ", "_", cod.arg),'_model_output_',years[1],'_',years[length(years)],'_lag',lag,'.rds'))

# provide summary
print(summary(mod))
# confint(mod)
print(exp(confint.default(mod)))

# save summary of model results in a way which can be compiled later
dat.results=data.frame(cause=cod.arg,lag=lag,rr=exp(mod$coefficients[1]),rr.ll=exp(confint.default(mod))[1,1],rr.ul=exp(confint.default(mod))[1,2])

# save model summary
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/summary/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
write.csv(dat.results, paste0(dir.output,'medicare_',gsub(" ", "_", cod.arg),'_model_summary_',years[1],'_',years[length(years)],'_lag',lag,'.csv'))

