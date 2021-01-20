# this code will take a particular cause of death (CCS level 1)
# and run the chosen model for it

# NEED TO RUN ON RCE

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
seedVal = as.numeric(args[1]) + 1

# for testing
# seedVal = 1

# years of analysis
years = c(1999:2014)

# load ccs lookup
code.lookup.merged = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/CCS_lookup_2015.csv')
code.lookup.merged$X = NULL
code.lookup.merged = subset(code.lookup.merged, !(ccs_level_1%in%c(11,14,15,17,18)))

# make list of broad causes of hospitalization (temporary four causes for 200GB to delete when finished)
causes_groups = unique(as.character(code.lookup.merged$ccs_level_1_description))
# causes_groups = c("Neoplasms", "Diseases of the skin and subcutaneous tissue", "Mental illness", "Diseases of the respiratory system")

# process for finding broad causes of death and matching sub causes
causes_group = causes_groups[seedVal]
code.lookup.merged.subset = subset(code.lookup.merged, ccs_level_1_description==causes_group)
causes_to_load = unique(as.character(code.lookup.merged.subset$ccs_level_3_description))
causes_to_load = trimws(causes_to_load)
# causes_group = unique(as.character(code.lookup.merged.subset$ccs_level_1_description))

# directory to load data from
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/expanded_grid_hospitalisations/',years[1],'_',years[length(years)],'/')

# CCS level 1 input file
input.file = paste0(dir.input,'medicare_',gsub(" ", "_", causes_group),'_rates_expanded_grid_hospitalisations_',years[1],'_',years[length(years)],'.rds')

# check to see if a file exists for the analysis
if(file.exists(input.file)){
    print('data already processed... loading...')
    dat = readRDS(paste0(input.file))
}

# process if file doesn't already exists
if(!file.exists(input.file)){

    print('data not processed... processing now...')

    # load hospitalizations data
    dat.all = data.frame()
    for(cod.arg in causes_to_load){
        cod.arg = gsub(" ", "_", cod.arg) ; cod.arg = gsub("/", "_", cod.arg)
        input.file.2 = paste0(dir.input,'medicare_',cod.arg,'_rates_expanded_grid_hospitalisations_',years[1],'_',years[length(years)],'.rds')
        if(file.exists(input.file.2)){
            print(cod.arg)
            dat = readRDS(input.file.2)
            head(dat)
            dat.all=rbind(dat.all,dat)
        }
        if(!file.exists(input.file.2)){
            print(paste0('Cannot find ', cod.arg, ', so skipping over...'))
        }
    }

    # resummarise by CCS level 1
    library(plyr)
    dat = ddply(dat.all,.(fipscounty,year,month,day),summarize,cases=sum(cases),population=mean(population))

    # load for future attempts so do not need to waste lots of time processing data again
    saveRDS(dat,input.file)

    # get rid to save space
    rm(dat.all)
}

# attach temperature and precipitation data (no precipitation for now)
print('attaching weather data')
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

print('merging weather data with hospitalization data')
dat = merge(dat,dat.temp,by.x=c('fipscounty','day','month','year'),by.y=c('fips','day','month','year'),all.x=TRUE)
# dat = merge(dat,dat.precip,by.x=c('fipscounty','day','month','year'),by.y=c('fips','day','month','year'),all.x=TRUE)

print('preview of merged data to check')
head(dat)

# sample for model testing
dat.sample = subset(dat,year%in%years)

# make year numeric
dat.sample$year = as.numeric(dat.sample$year)

# add date info
dat.sample$date = with(dat.sample,paste0(day,'/',month,'/',year))
dat.sample$date = as.Date(dat.sample$date, format="%d/%m/%Y")
dat.sample$dow = as.factor(weekdays(dat.sample$date))
dat.sample$doy = as.numeric(strftime(dat.sample$date, format = "%j"))

# fix year to be numeric
dat.sample$year = as.numeric(dat.sample$year)

library(plyr)

# create log of population
dat.sample$logpop <- log(dat.sample$population)

# create stratum with fipscounty and doy
dat.sample$stratum = as.factor(as.factor(dat.sample$fipscounty):as.factor(dat.sample$doy))
# dat.sample$stratum = as.factor(as.factor(dat.sample$fipscounty):as.factor(dat.sample$month):as.factor(dat.sample$dow))

# reattach coastal storm event data as would need to add coastal storm events which are associated with zero cases on a fipcounty-day
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
counties.wind = readRDS(paste0(dir.input,'wind_data_',years[1],'_',years[length(years)],'.rds'))
counties.wind.all = readRDS(paste0(dir.input,'wind_data_all_',years[1],'_2016.rds'))
counties.wind.edit.array = readRDS(paste0(dir.input,'wind_data_lag_array_',years[1],'_2016.rds'))

# multiple lags merge with hospitalization data
dat.sample.multiple = dat.sample
dat.sample = NULL
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
# cb1.temp = crossbasis(dat.sample.multiple$tmean,argvar=list("ns", df=3),lag=7, arglag=list("ns", df=3))

# temperature of the day before and day of recorded date
cb1.temp = crossbasis(dat.sample.multiple$tmean,argvar=list("ns", df=2),lag=(-1:0), arglag=list("ns", df=2))

# distributed lag unconstrained
system.time({

    mod_dlm_unconstrained = gnm(cases ~ event_lag0 + event_lag1 + event_lag2 + event_lag3 + event_lag4 + event_lag5 + event_lag6 + event_lag7 +
        cb1.temp + ns(year, df=3) + dow , data=dat.sample.multiple, offset=logpop, eliminate=factor(stratum), family=quasipoisson)

})

# distributed lag unconstrained
system.time({

    mod_dlm_unconstrained_no_temp = gnm(cases ~ event_lag0 + event_lag1 + event_lag2 + event_lag3 + event_lag4 + event_lag5 + event_lag6 + event_lag7 +
        ns(year, df=3) + dow , data=dat.sample.multiple, offset=logpop, eliminate=factor(stratum), family=quasipoisson)

})

# make list of broad causes of hospitalization
causes_groups_final=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
                                    'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
                                    'Infectious and parasitic diseases','Musculoskeletal and connective tissue diseases',
                                    'Nervous system diseases','Skin and subcutaneous tissue diseases')

# bonferroni correction CI calculation
alpha = 100 * (0.05/length(causes_groups_final))
corrected.ci = round(100 - alpha,1) / 100

# provide summary for temperature model
lag_est_mean = as.data.frame(exp(mod_dlm_unconstrained$coefficients[1:8]))
lag_est_uncertainty = exp(confint.default(mod_dlm_unconstrained)[c(1:8),])
lag_est_uncertainty_bf_corrected = exp(confint.default(mod_dlm_unconstrained,level=corrected.ci)[c(1:8),])

dat.results = data.frame(lag=c(0:7),
                        rr=lag_est_mean,rr.ll=lag_est_uncertainty[,1],rr.ul=lag_est_uncertainty[,2],
                        rr.ll.bfc=lag_est_uncertainty_bf_corrected[,1],rr.ul.bfc=lag_est_uncertainty_bf_corrected[,2])

dat.results$cause = causes_group
rownames(dat.results) = seq(nrow(dat.results))
names(dat.results)[2] = 'rr'

# provide summary for no temperature model
lag_est_mean_no_temp = as.data.frame(exp(mod_dlm_unconstrained_no_temp$coefficients[1:8]))
lag_est_uncertainty_no_temp = exp(confint.default(mod_dlm_unconstrained_no_temp)[c(1:8),])
lag_est_uncertainty_bf_corrected_no_temp = exp(confint.default(mod_dlm_unconstrained_no_temp,level=corrected.ci)[c(1:8),])

dat.results_no_temp = data.frame(lag=c(0:7),
                        rr=lag_est_mean_no_temp,rr.ll=lag_est_uncertainty_no_temp[,1],rr.ul=lag_est_uncertainty_no_temp[,2],
                        rr.ll.bfc=lag_est_uncertainty_bf_corrected_no_temp[,1],rr.ul.bfc=lag_est_uncertainty_bf_corrected_no_temp[,2])

dat.results_no_temp$cause = causes_group
rownames(dat.results_no_temp) = seq(nrow(dat.results_no_temp))
names(dat.results_no_temp)[2] = 'rr'

# output directory
dir.output.plots = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/model_run/wind_events/frequentist/unconstrained_dlm/')
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dir.output.model.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/output/')

ifelse(!dir.exists(dir.output.plots), dir.create(dir.output.plots, recursive=TRUE), FALSE)
ifelse(!dir.exists(dir.output.model.summary), dir.create(dir.output.model.summary, recursive=TRUE), FALSE)
ifelse(!dir.exists(dir.output.model.output), dir.create(dir.output.model.output, recursive=TRUE), FALSE)

# save summary of model results in a way which can be compiled later
saveRDS(mod_dlm_unconstrained, paste0(dir.output.model.output,'medicare_',causes_group,'_model_summary_update_',years[1],'_',years[length(years)],'.rds'))

# save w temperature model summary
write.csv(dat.results, paste0(dir.output.model.summary,'medicare_',causes_group,'_model_summary_update_',years[1],'_',years[length(years)],'.csv'))

# save wo temperature model summary
write.csv(dat.results_no_temp, paste0(dir.output.model.summary,'medicare_',causes_group,'_model_summary_update_no_temp_',years[1],'_',years[length(years)],'.csv'))

library(ggplot2)

pdf(paste0(dir.output.plots,'medicare_rr_values_model_summary_unconstained_dlm_update_',causes_group,'_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',height=0,width=0)
print(
    ggplot(subset(dat.results)) +
    geom_point(aes(x=lag,y=rr)) +
    geom_errorbar(aes(x=lag,ymin=rr.ll.bfc,ymax=rr.ul.bfc)) +
    geom_hline(yintercept=1,linetype=2) +
    ggtitle(paste0(gsub("_", " ", causes_group),' (unconstrained dlm)')) +
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

dev.off()
