# this code will take processed merged cases and denom files
# make a complete grid for each file

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_part6.submit

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

# fix ccs column name (why do I always do stupid stuff like this?)
names(dat.admissions)[names(dat.admissions)=='css_category']='ccs_category'

# isolate a particular cause of hospitalisations
ccs_codes = sort(unique(dat.admissions$ccs_category))

# css names
ccs.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('css_category','full_name')
ccs.names$full_name = as.character(ccs.names$full_name)

# output directory
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/expanded_grid_hospitalisations/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# fully expand dates, fips then match to loaded admissions data
dates = seq(as.Date("1999-01-01"), as.Date("2014-12-31"), by="days")
fipscounty = sort(unique(dat.admissions$fipscounty))
complete.grid = expand.grid(date=dates,fipscounty=fipscounty)
complete.grid$year = format(complete.grid$date, '%Y')
complete.grid$month = as.numeric(format(complete.grid$date, '%m'))
complete.grid$day = as.numeric(format(complete.grid$date, '%d'))
complete.grid$date = NULL

# PREPARE DENOM FILES

# load processed denom file
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/medicare_denom_processing/')
dat.denom = readRDS(paste0(dir.input,'medicare_denom_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.denom)=1:nrow(dat.denom)
dat.denom$fips = paste0('0',dat.denom$fips)
dat.denom$fips = substr(dat.denom$fips, nchar(dat.denom$fips)-5+1, nchar(dat.denom$fips))
dat.denom$state.fips = substr(dat.denom$fips, 1,2)

# attach state names
fips.lookup <- read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/fips_lookup/name_fips_lookup.csv')
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(2,15)),]
fips.lookup = fips.lookup[,c(1:2)]
dat.denom$state.fips = as.numeric(dat.denom$state.fips)
dat.denom = merge(dat.denom,fips.lookup,by.x='state.fips',by.y='fips',all.x=TRUE)
dat.denom$full_name = as.character(dat.denom$full_name)
dat.denom$full_name[is.na(dat.denom$full_name)==TRUE] <- 'Died within year'

# isolate only coastal states
state.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/name_lookup/ssa_standard_state_code_lookup.csv')
state.names = state.names[,c(2:4)]
dat.denom = merge(dat.denom,state.names,by.x='full_name',by.y='name',all.x=TRUE)
dat.denom = subset(dat.denom,coastal_storm_state==1)

# CYCLE THROUGH EACH CCS
for(ccs_code in ccs_codes){

    # print full name
    full_name = as.character(subset(ccs.names,css_category==ccs_code)[2])
    full_name = trimws(full_name)
    full_name = gsub('/',' ',full_name)
    print(paste0(ccs_code,': ',full_name))

    # filter out single ccs category
    dat.admissions.single = subset(dat.admissions,ccs_category==ccs_code)
    dat.admissions.single$month = as.numeric(dat.admissions.single$month)
    dat.admissions.single$day = as.numeric(dat.admissions.single$day)

    # merge deaths counts with complete grid to ensure there are rows with zero deaths
    dat.complete = merge(complete.grid,dat.admissions.single,by=c('fipscounty','year','month','day'),all.x='TRUE') # WHY IS THIS LOSING CASES???

    # assign missing cases to have value 0
    dat.complete$cases = ifelse(is.na(dat.complete$cases)==TRUE,0,dat.complete$cases)

    # match admissions and storm data by fips and date
    dat.complete$month = as.numeric(dat.complete$month)
    dat.complete$year = as.numeric(dat.complete$year)
    dat.complete$day = as.numeric(dat.complete$day)

    # MERGE CASES AND DENOM FILES
    dat.denom$fips = as.numeric(dat.denom$fips)
    dat.denom = dat.denom[,c('year','fips','population')]
    dat.merged = merge(dat.complete[,c('year','fipscounty','month','day','cases')], dat.denom[,c('year','fips','population')], by.x=c('year','fipscounty'),by.y=c('year','fips'),all.x=TRUE)

    # only take years from beginning of actual intended dataset
    dat.merged = subset(dat.merged,year%in%years)

    # exclude those weird georgia counties
    fips_to_exclude = c(13007, 13037, 13061, 13087, 13099, 13131, 13201, 13239, 13243, 13253, 13273)
    dat.merged = subset(dat.merged,!(year%in%c(1999:2001)&fipscounty%in%c(fips_to_exclude)))

    # calculate rates
    dat.merged$rates = with(dat.merged,cases/population)

    # check missing values
    dat.merged.na = dat.merged[rowSums(is.na(dat.merged)) > 0,]

    print(paste0('Total number of cases = ',sum(dat.merged$cases)))
    print(paste0('Unmatched cases = ',sum(dat.merged.na$cases)))

    saveRDS(dat.merged, paste0(dir.output,'medicare_',gsub(" ", "_", full_name),'_rates_expanded_grid_hospitalisations_',years[1],'_',years[length(years)],'.rds'))
}