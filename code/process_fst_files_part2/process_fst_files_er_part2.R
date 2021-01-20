# this code will take all processed years and load them
# then process them together
# because some admissions are in the previous year

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_2.submit

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# years of data
years = c(1999:2014)

# codes from SSA_STATE_CD info here https://www.resdac.org/cms-data/variables/medpar-beneficiary-residence-ssa-standard-state-code

# location of files
# dir.input = paste0('~/shared_space/ci3_analysis/rmparks_coastal_storms_Jan_2020/data/medicare_admissions_processing/')
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/medicare_admissions_processing/')

# loop through and load processed admissions file for next stage of processing
dat.all = data.frame()
for(year in years){
# for(year in c(1999:2004)){
    print(paste0('loading ',year))

    # load current year's medicare data
    dat.current = readRDS(paste0(dir.input,'medicare_admissions_er_processing_',year,'.rds'))

    # fix date categories
    dat.current$date = format(as.Date(dat.current$ADATE, "%d%B%Y"))
    dat.current$day = format(as.Date(dat.current$date), "%d")
    dat.current$month = format(as.Date(dat.current$date), "%m")
    dat.current$year = format(as.Date(dat.current$date), "%Y")

    # fix css category
    dat.current$css_category = as.numeric(dat.current$css_category)

    # summarising by date of admission (which isn't necessarily in the file for the year on record)
    print('summarising file...')
    library(plyr)
    dat.current = ddply(dat.current,.(SSA_STATE_CD,SSA_CNTY_CD,css_category,emergency,day,month,year),summarise,cases=sum(cases))

    dat.all=rbind(dat.all,dat.current)
}

# only take emergency values
dat.all.emergency = subset(dat.all,emergency==1)
dat.all.non.emergency = subset(dat.all,emergency==0)

# summarise one final time to get over entire multi-year period
dat_admissions_emergency_sum_total = ddply(dat.all.emergency,.(SSA_STATE_CD, SSA_CNTY_CD,css_category,day,month,year),summarise,cases=sum(cases))
dat_admissions_nonemergency_sum_total = ddply(dat.all.non.emergency,.(SSA_STATE_CD, SSA_CNTY_CD,css_category,day,month,year),summarise,cases=sum(cases))

#reorder
dat_admissions_sum_total = dat_admissions_emergency_sum_total[order(dat_admissions_emergency_sum_total$SSA_STATE_CD,dat_admissions_emergency_sum_total$SSA_CNTY_CD,dat_admissions_emergency_sum_total$css_category,dat_admissions_emergency_sum_total$year,dat_admissions_emergency_sum_total$month,dat_admissions_emergency_sum_total$day),]
dat_admissions_nonemergency_sum_total = dat_admissions_nonemergency_sum_total[order(dat_admissions_nonemergency_sum_total$SSA_STATE_CD,dat_admissions_nonemergency_sum_total$SSA_CNTY_CD,dat_admissions_nonemergency_sum_total$css_category,dat_admissions_nonemergency_sum_total$year,dat_admissions_nonemergency_sum_total$month,dat_admissions_nonemergency_sum_total$day),]

# save processed admissions file
# dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/medicare_admissions_processing/')
dir.output.local = paste0('~/data/morbidity/US/medicare/processed/')
ifelse(!dir.exists(dir.output.local), dir.create(dir.output.local, recursive=TRUE), FALSE)
saveRDS(dat_admissions_sum_total, paste0(dir.output.local,'medicare_admissions_er_',years[1],'_',years[length(years)],'.rds'))
saveRDS(dat_admissions_nonemergency_sum_total, paste0(dir.output.local,'medicare_admissions_non_er_',years[1],'_',years[length(years)],'.rds'))

