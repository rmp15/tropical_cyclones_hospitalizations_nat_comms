# this code will take a selected year and load it
# then bactch process delerium and dementia in intervals of 50,000

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files.submit

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
seedVal = as.numeric(args[1]) + 1

# expand grid of stuff
years = c(1999:2020)
year = years[seedVal]

print(year)

# 1a Declare root directory
project.folder = paste0(print(here::here()),'/')

# 1. SET UP CCS LOOKUP
if(year%in%c(1999:2014)){

    # load the Bobb coding file and match up the codes (from paper https://www.ncbi.nlm.nih.gov/pubmed/25536257)
    # downloaded from https://www.hcup-us.ahrq.gov/toolssoftware/ccs/ccs.jsp#examples
    setwd(paste0(project.folder,'data/Single_Level_CCS_2015/'))
    code.lookup = read.csv('$dxref 2015.csv',skip=1)
    names(code.lookup) = c('icd9','css_category','css_category_description','icd9_code_description')
    code.lookup$icd9 = trimws(code.lookup$icd9) ; code.lookup$icd9 = trimws(code.lookup$icd9)

    # get rid of all the weird quotation marks in look-up file
    for(i in seq(dim(code.lookup)[2])){
        code.lookup[,i] = gsub("'","",code.lookup[,i])
        code.lookup[,i] = as.character(code.lookup[,i])
    }

    code.lookup$icd9 = trimws(code.lookup$icd9) ; code.lookup$icd9 = trimws(code.lookup$icd9)

}

if(year%in%c(2015:2020)){
    # load crosswalk
    code.lookup = readRDS(paste0(project.folder,'data/ccs_icd_crosswalk/ccs_icd_crosswalk.rds'))
}

# 2. BATCH PROCESS THE ADMISSIONS FILES

# set location for loading and processing admissions files
library(fst)
setwd('~/shared_space/ci3_health_data/medicare/gen_admission/1999_2016/targeted_conditions/cache_data')

# load admissions file with year of interest and figure out number of rows in file
file_admissions = paste0('admissions_by_year/admissions_',year,'.fst')
metadata = fst.metadata(file_admissions)
num_rows = metadata$nrOfRows
intervals = seq(from=1, to = num_rows, by=50000)
intervals = c(intervals,num_rows)

# loop through batches of 50,000 of admissions file, summarise each batch, then add to a larger file
# necessary due to restrictions on memory on Harvard RCE

library(plyr)

dat_admissions_sum_total = data.frame()
dat_admissions_sum_total_na = data.frame()

for(i in seq((length(intervals)-1))){
# for(i in c(1:2)){

    print(paste0('Processing batch ',i,' of ',length(intervals)-1))

    dat_admissions = read_fst(file_admissions, from=intervals[i], to=(intervals[i+1]-1))

    # code for emergency or non-emergency via
    ## https://med.noridianmedicare.com/web/jea/topics/claim-submission/type-of-admission-or-visit-codes
    dat_admissions$emergency = ifelse(dat_admissions$ADM_TYPE%in%c('1','2','5'),1,0)

    # summarise by state, county, year, date of admission, CSS code
    dat_admissions_sum = ddply(dat_admissions,.(SSA_STATE_CD,SSA_CNTY_CD,YEAR,ADATE,DIAG1,emergency),nrow)
    names(dat_admissions_sum)[names(dat_admissions_sum) == "V1"] <- "cases"

    # match up the cause of hospitalization groupings with the DIAG1 coding
    dat_admissions_sum = merge(dat_admissions_sum,code.lookup,by.x=c('DIAG1'),by.y=c('icd9'),all.x=TRUE)
    dat_admissions_sum$css_category = trimws(dat_admissions_sum$css_category)

    # add na files to total na file to check missing codes
    dat_admissions_sum_na = dat_admissions_sum[rowSums(is.na(dat_admissions_sum)) > 0,]
    dat_admissions_sum_total_na = rbind(dat_admissions_sum_total_na, dat_admissions_sum_na)

    # summarise again by new groupings
    dat_admissions_sum = ddply(dat_admissions_sum,.(SSA_STATE_CD,SSA_CNTY_CD,YEAR,ADATE,css_category,emergency),summarise,cases=sum(cases))

    print(paste0('Number of complete rows = ',dim(dat_admissions_sum)[1]))
    print(paste0('Number of total rows = ',dim(na.omit(dat_admissions_sum))[1]))

    # bind to total file
    dat_admissions_sum_total = rbind(dat_admissions_sum_total, dat_admissions_sum)

}

# summarise one final time to get over entire
dat_admissions_sum_total = ddply(dat_admissions_sum_total,.(SSA_STATE_CD,SSA_CNTY_CD,YEAR,ADATE,css_category,emergency),summarise,cases=sum(cases))

# check to see if total number of original rows (i.e., cases) is total number of final cases in file
print(year)
print(paste0('Original number of total rows: ',num_rows,'. Number of total rows in final file: ',sum(dat_admissions_sum_total$cases)))

# save processed admissions file for next stage of processing
dir.output = paste0('~/shared_space/ci3_analysis/rmparks_coastal_storms_Jan_2020/data/medicare_admissions_processing/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
saveRDS(dat_admissions_sum_total, paste0(dir.output,'medicare_admissions_er_processing_',year,'.rds'))

# save missing icd9 records too
dat_admissions_sum_total_na = dat_admissions_sum_total_na[,1]
dat_admissions_sum_total_na = unique(dat_admissions_sum_total_na)
# saveRDS(dat_admissions_sum_total_na, paste0(dir.output,'NA_medicare_admissions_processing_',year,'.rds'))
write.csv(dat_admissions_sum_total_na, paste0(dir.output,'NA_medicare_admissions_er_processing_',year,'.csv'))