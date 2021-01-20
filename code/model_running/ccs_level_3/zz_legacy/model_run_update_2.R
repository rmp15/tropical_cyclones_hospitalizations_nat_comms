# this code will take a particular cause of death
# and run the chosen model for it

# NEED TO RUN ON RCE

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
seedVal = as.numeric(args[1]) + 1

# years of analysis
# years = c(1999:2016)
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

# set lag and cause of hospitalisation from seed value
cod.arg = cods[seedVal] ; cod.arg = gsub(" ", "_", cod.arg)

# print arguments
print(cod.arg)

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

# reattach coastal storm event data as would need to add coastal storm events which are associated with zero cases on a fipcounty-day
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
counties.wind = readRDS(paste0(dir.input,'wind_data_',years[1],'_',years[length(years)],'.rds'))
counties.wind.all = readRDS(paste0(dir.input,'wind_data_all_',years[1],'_2016.rds'))
counties.wind.edit.array = readRDS(paste0(dir.input,'wind_data_lag_array_',years[1],'_2016.rds'))

# multiple lags merge with hospitalization data
dat.sample.multiple = dat.sample
dat.merged.multiple = merge(dat.sample.multiple,counties.wind.edit.array,by=c('year','month','day','fipscounty'),
                                                by.y=c('year','month','day','fips'),all.x=TRUE)

# check missing values
dat.merged.multiple.na = dat.merged.multiple[rowSums(is.na(dat.merged.multiple)) > 0,]

# only include fips which have both events and non-events for multiple lag
dat.event.multiple.test = ddply(dat.merged.multiple, .(fipscounty), summarize, NumSubs = length(unique(event_lag0)))
dat.event.multiple.test = subset(dat.event.multiple.test,NumSubs<2)
fips_to_exclude = as.numeric(as.character(dat.event.multiple.test$fipscounty))

dat.sample.multiple = subset(dat.merged.multiple,!(fipscounty%in%fips_to_exclude))

library(gnm) ; library(splines) ; library(dlnm)

# for dnlm
cb1.temp = crossbasis(dat.sample.multiple$tmean,argvar=list("ns", df=3),lag=7, arglag=list("ns", df=3))

# distributed lag unconstrained
system.time({

    mod_dlm_unconstrained = gnm(cases ~ event_lag0 + event_lag1 + event_lag2 + event_lag3 + event_lag4 + event_lag5 + event_lag6 + event_lag7 +
        cb1.temp + ns(year, df=3) , data=dat.sample.multiple, offset=logpop, eliminate=factor(stratum), family=quasipoisson)

})

# provide summary
lag_est_mean = as.data.frame(exp(mod_dlm_unconstrained$coefficients[1:8]))
lag_est_uncertainty = exp(confint.default(mod_dlm_unconstrained)[c(1:8),])

dat.results = data.frame(lag=c(0:7),rr=lag_est_mean,rr.ll=lag_est_uncertainty[,1],rr.ul=lag_est_uncertainty[,2])
dat.results$cause = gsub("_", " ", cod.arg)
rownames(dat.results) = seq(nrow(dat.results))
names(dat.results)[2] = 'rr'

# output directory
dir.output.plots = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/model_run/wind_events/frequentist/unconstrained_dlm/')
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dir.output.model.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/output/')

ifelse(!dir.exists(dir.output.plots), dir.create(dir.output.plots, recursive=TRUE), FALSE)
ifelse(!dir.exists(dir.output.model.summary), dir.create(dir.output.model.summary, recursive=TRUE), FALSE)
ifelse(!dir.exists(dir.output.model.output), dir.create(dir.output.model.output, recursive=TRUE), FALSE)

pdf(paste0(dir.output.plots,'medicare_rr_values_model_summary_unconstained_dlm_',cod.arg,'_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',height=0,width=0)
print(
    ggplot(subset(dat.results)) +
    geom_point(aes(x=lag,y=rr)) +
    geom_errorbar(aes(x=lag,ymin=rr.ll,ymax=rr.ul)) +
    geom_hline(yintercept=1,linetype=2) +
    ggtitle(paste0(gsub("_", " ", cod.arg),' (unconstrained dlm)')) +
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
print(
    plot(pred1.temp, "slices", var=1, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
    main=paste0("Association per 1°C for ",cod.arg = gsub("_", " ", cod.arg)))
)

dev.off()

# save summary of model results in a way which can be compiled later
saveRDS(mod_dlm_unconstrained, paste0(dir.output.model.output,'medicare_',gsub(" ", "_", cod.arg),'_model_summary_',years[1],'_',years[length(years)],'.rds'))

# save model summary
write.csv(dat.results, paste0(dir.output.model.summary,'medicare_',gsub(" ", "_", cod.arg),'_model_summary_',years[1],'_',years[length(years)],'.csv'))