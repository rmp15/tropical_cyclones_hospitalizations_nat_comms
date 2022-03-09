# this code will take all processed years of denominators and load them
# then process them together

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_part5.submit

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# years of data
years = c(1999:2014)

# codes from SSA_STATE_CD info here https://www.resdac.org/cms-data/variables/medpar-beneficiary-residence-ssa-standard-state-code

# location of files
# dir.input = paste0('~/shared_space/ci3_analysis/rmparks_coastal_storms_Jan_2020/data/medicare_admissions_processing/')
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/medicare_denom_processing/')

# loop through and load processed admissions file for next stage of processing
dat.all = data.frame()
for(year in years){
# for(year in c(1999:2004)){
    print(paste0('loading ',year))

    # load current year's medicare data
    dat.current = readRDS(paste0(dir.input,'medicare_denom_processing_',year,'.rds'))
    names(dat.current)[2] = 'fips'

    dat.all=rbind(dat.all,dat.current)
}

# summarise one final time to get over entire multi-year period
library(plyr)
dat_denom_sum_total = ddply(dat.all,.(year,fips),summarise,population=sum(population))

# reorder
dat_denom_sum_total = dat_denom_sum_total[order(dat_denom_sum_total$year,dat_denom_sum_total$fips),]

# save processed denom file
# dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/medicare_admissions_processing/')
ifelse(!dir.exists(dir.input), dir.create(dir.input, recursive=TRUE), FALSE)
saveRDS(dat_denom_sum_total, paste0(dir.input,'medicare_denom_',years[1],'_',years[length(years)],'.rds'))

