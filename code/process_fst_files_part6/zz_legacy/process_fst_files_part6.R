# this code will take processed merged cases and denom files
# attach them to weather data

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# expand grid of stuff temporary
years = c(1999:2014)

# load processed admissions file
dir.input.local = paste0('~/data/morbidity/US/medicare/processed/')
dat.admissions = readRDS(paste0(dir.input.local,'medicare_rates_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.admissions)=1:nrow(dat.admissions)

# load processed wind file
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
counties.wind = readRDS(paste0(dir.input,'wind_data_',years[1],'_',years[length(years)],'.rds'))

library(plyr)
# temporary exploration of wind data
# test = ddply(counties.wind,.(fips,closest_date),nrow)

# expand grid to include all day-month-year-fips-ccs combinations
# TO DO

# fix ccs column name (why do I always do stupid stuff like this?)
names(dat.admissions)[names(dat.admissions)=='css_category']='ccs_category'

# isolate a particular cause of hospitalisations
ccs_codes = sort(unique(dat.admissions$ccs_category))

# css names
ccs.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('css_category','full_name')
ccs.names$full_name = as.character(ccs.names$full_name)

# output directory
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/matched_wind_data/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# CYCLE THROUGH EACH CCS TO CHECK NUMBER OF MATCHED EVENTS
dat.matched.events = data.frame()
for(ccs_code in ccs_codes){

    # print full name
    full_name = as.character(subset(ccs.names,css_category==ccs_code)[2])
    full_name = trimws(full_name)
    full_name = gsub('/',' ',full_name)
    print(paste0(ccs_code,': ',full_name))

    # filter out single ccs category
    dat.admissions.single = subset(dat.admissions,ccs_category==ccs_code)

    # match admissions and storm data by fips and date
    dat.admissions.single$month = as.numeric(dat.admissions.single$month)
    dat.admissions.single$year = as.numeric(dat.admissions.single$year)
    dat.admissions.single$day = as.numeric(dat.admissions.single$day)
    counties.wind$fips = as.numeric(counties.wind$fips)

    dat.merged.single = merge(dat.admissions.single,counties.wind,by.x=c('year','month','day','fipscounty'),
                                                by.y=c('year','month','day','fips'),all.x=TRUE)
    saveRDS(dat.merged.single, paste0(dir.output,'medicare_',gsub(" ", "_", full_name),'_rates_matched_wind_events_',years[1],'_',years[length(years)],'.rds'))

    dat.matched.event.single = data.frame(ccs_code=ccs_code,full_name=full_name,matches=nrow(na.omit(dat.merged.single)),total_events=nrow(dat.merged.single))
    dat.matched.events=rbind(dat.matched.events,dat.matched.event.single)
    print(paste0(nrow(na.omit(dat.merged.single)),' matched events from ', nrow(dat.merged.single)))

}

# add percentage of records matched
dat.matched.events$percentage_matched = with(dat.matched.events,round(100*(matches/total_events),4))

# save records as csv
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/matched_wind_data/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
write.csv(dat.matched.events, paste0(dir.output,'medicare_rates_matched_wind_events_summary_',years[1],'_',years[length(years)],'.csv'), row.names=FALSE)
