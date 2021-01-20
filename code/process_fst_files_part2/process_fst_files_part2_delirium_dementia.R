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
    dat.current = readRDS(paste0(dir.input,'medicare_admissions_dementia_delirium_processing_',year,'.rds'))

    # summarising by date of admission (which isn't necessarily in the file for the year on record)
    print('summarising file...')
    library(plyr)
    dat.current = ddply(dat.current,.(css_category_2),summarise,cases=sum(cases))

    dat.all=rbind(dat.all,dat.current)
}

# summarise one final time to get over entire multi-year period
dat_admissions_sum_total = ddply(dat.all,.(css_category_2),summarise,cases=sum(cases))

# save processed admissions file
# dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/medicare_admissions_processing/')
dir.output.local = paste0('~/data/morbidity/US/medicare/processed/')
ifelse(!dir.exists(dir.output.local), dir.create(dir.output.local, recursive=TRUE), FALSE)
saveRDS(dat_admissions_sum_total, paste0(dir.output.local,'medicare_admissions_dementia_delirium_processing_',years[1],'_',years[length(years)],'.rds'))

