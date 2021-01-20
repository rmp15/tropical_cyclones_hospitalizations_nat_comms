# this code will take a selected year's denominator files and load it
# then batch process it in intervals of 50,000

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_part3.submit

# NEEDS TO CHANGE location to
# ci3_health_data/medicare/mortality/1999_2016/wu/cache_data/merged_by_year_v2

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
seedVal = as.numeric(args[1]) + 1

# expand grid of stuff
years = c(1999:2020)
year = years[seedVal]

print(year)

# BATCH PROCESS THE DENOM FILES

# set location for loading and processing admissions files
library(fst)
setwd('~/shared_space/ci3_health_data/medicare/gen_admission/1999_2016/targeted_conditions/cache_data')

# load denom file with year of interest and figure out number of rows in file
file_denom = paste0('health_by_year/health_',year,'.fst')
metadata = fst.metadata(file_denom)
num_rows = metadata$nrOfRows
intervals = seq(from=1, to = num_rows, by=50000)
intervals = c(intervals,num_rows)

# loop through batches of 50,000 of denom file, summarise each batch, then add to a larger file
# necessary due to restrictions on memory on Harvard RCE

library(plyr)

dat_denom_sum_total = data.frame()
dat_denom_sum_total_na = data.frame()

for(i in seq((length(intervals)-1))){
#for(i in c(1:2)){

    print(paste0('Processing batch ',i,' of ',length(intervals)-1))

    dat_denom = read_fst(file_denom, from=intervals[i], to=(intervals[i+1]-1))

    # don't filter out dead people
    # dat_denom = subset(dat_denom,dead==0)

    # summarise by state, county, year, date of admission, CSS code
    dat_denom_sum = ddply(dat_denom,.(year,FIPS),nrow)
    names(dat_denom_sum)[names(dat_denom_sum) == "V1"] <- "population"

    # add na files to total na file to check missing codes
    dat_denom_sum_na = dat_denom_sum[rowSums(is.na(dat_denom_sum)) > 0,]
    dat_denom_sum_total_na = rbind(dat_denom_sum_total_na, dat_denom_sum_na)

    print(paste0('Number of complete rows = ',dim(dat_denom_sum)[1]))
    print(paste0('Number of total rows = ',dim(na.omit(dat_denom_sum))[1]))

    # bind to total file
    dat_denom_sum_total = rbind(dat_denom_sum_total, dat_denom_sum)

}

# summarise one final time to get over entire dataset
dat_denom_sum_total = ddply(dat_denom_sum_total,.(year,FIPS),summarise,population=sum(population))

# check to see if total number of original rows (i.e., cases) is total number of final cases in file
print(year)
print(paste0('Original number of total rows: ',num_rows,'. Number of total rows in final file: ',sum(dat_denom_sum_total$population)))

# save processed denom file for next stage of processing
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/medicare_denom_processing/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
saveRDS(dat_denom_sum_total, paste0(dir.output,'medicare_denom_processing_',year,'.rds'))

# save missing icd9 records too
# dat_denom_sum_total_na = dat_denom_sum_total_na[,1]
dat_denom_sum_total_na = ddply(dat_denom_sum_total_na,.(year,FIPS),summarise,population=sum(population))
write.csv(dat_denom_sum_total_na, paste0(dir.output,'NA_medicare_denom_processing_',year,'.csv'))

