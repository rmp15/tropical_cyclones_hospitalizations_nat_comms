# this code will take all processed years and load them
# calculate rates then compare how to match up SSA codings with FIPS

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_part5.submit

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# expand grid of stuff temporary
years = c(1999:2014)

# PREPARE ADMISSIONS FILES

# er or non-er label
labels = c('er','non_er')

# loop through er and non-er
for(label in labels){

  # load processed admissions file
  dir.input.local = paste0('~/data/morbidity/US/medicare/processed/')
  dat.admissions = readRDS(paste0(dir.input.local,'medicare_admissions_',label,'_',years[1],'_',years[length(years)],'.rds'))
  rownames(dat.admissions)=1:nrow(dat.admissions)

  print(paste0('Total number of cases is currently ',sum(dat.admissions$cases)))

  # attach state names
  state.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/name_lookup/ssa_standard_state_code_lookup.csv')
  state.names = state.names[,c(1:2,4)]
  dat.admissions = merge(dat.admissions,state.names,by.x='SSA_STATE_CD',by.y='code',all.x=TRUE)

  print(paste0('Total number of cases is currently ',sum(dat.admissions$cases)))

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

  # 1. REMOVE NON-COASTAL STATES CASES

  # only have coastal states
  dat.admissions = subset(dat.admissions,coastal_storm_state==1)

  print(paste0('Total number of cases is currently ',sum(dat.admissions$cases)))

  # 2. REMOVE SSA CODES CASES NOT IN FIPS DIRECTORY

  # make all SSD_CNTY_CD three characters long, adding zeroes at the beginning of values that aren't three characters long
  dat.admissions$SSA_CNTY_CD = paste0('00',dat.admissions$SSA_CNTY_CD)
  dat.admissions$SSA_CNTY_CD = substr(dat.admissions$SSA_CNTY_CD, nchar(dat.admissions$SSA_CNTY_CD)-3+1, nchar(dat.admissions$SSA_CNTY_CD))

  # make unique ssa state county code to merge with fips lookup
  dat.admissions$ssacounty = with(dat.admissions,paste0(SSA_STATE_CD,SSA_CNTY_CD))
  dat.admissions$ssacounty = as.numeric(dat.admissions$ssacounty)

  # fips ssa lookup
  fips.ssa.lookup = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/ssa_fips_state_county2017/ssa_fips_state_county2017.csv')
  fips.ssa.lookup = fips.ssa.lookup[,c(1:4)]

  # merge admissions file with ssa lookup file
  dat.admissions = merge(dat.admissions,fips.ssa.lookup,by.x='ssacounty',by.y='ssacounty',all.x=TRUE)

  # remove cases with SSA codes which are not in the FIPS directory
  dat.admissions = na.omit(dat.admissions)

  print(paste0('Total number of cases is currently ',sum(dat.admissions$cases)))

  # 3. REMOVE 990 FIPS CODES
  dat.admissions = subset(dat.admissions,SSA_CNTY_CD!=999)

  print(paste0('Total number of cases is currently ',sum(dat.admissions$cases)))

  # 4. MERGE CASES AND DENOM FILES

  dat.denom$fips = as.numeric(dat.denom$fips)
  dat.denom = dat.denom[,c('year','fips','population')]
  dat.merged = merge(dat.admissions, dat.denom[,c('year','fips','population')], by.x=c('year','fipscounty'),by.y=c('year','fips'),all.x=TRUE)

  # only take years from beginning of actual intended dataset
  dat.merged = subset(dat.merged,year%in%years)

  print(paste0('Total number of cases is currently ',sum(dat.merged$cases)))

  # Look at rows which are matched and which ones are missing and why
  dat.merged.not.na = dat.merged[rowSums(is.na(dat.merged))==0,]
  dat.merged.na = dat.merged[rowSums(is.na(dat.merged))>0,]

  dat.merged.na$fipscounty = paste0('0',dat.merged.na$fipscounty)
  dat.merged.na$fipscounty = substr(dat.merged.na$fipscounty, nchar(dat.merged.na$fipscounty)-5+1, nchar(dat.merged.na$fipscounty))
  dat.merged.na$state.fips = substr(dat.merged.na$fips, 1,2)

  print(paste0('Total number of cases is currently ',sum(dat.merged.not.na$cases)))

  # summary of all and missing values
  dat.merged.summary = ddply(dat.merged,.(year,fipscounty),summarise,cases=sum(cases))
  dat.merged.na.summary = ddply(dat.merged.na,.(year,fipscounty),summarise,cases=sum(cases))

  # 5. EXCLUDE REMAINING (HOPEFULLY TEMPORARY) POST-MERGE PROBLEMS (CURRENTLY IN GEORGIA AND VIRGINIA)

  # examine the problematic Virginia counties in isolation (FIPS 51595 and 51081)
  virginia_excluded_counties = c(51595,51081)
  dat.merged.summary.virginia = subset(dat.merged.summary,fipscounty%in%virginia_excluded_counties)
  dat.denom.virginia = subset(dat.denom,fips%in%virginia_excluded_counties)
  dat.denom.virginia = dat.denom.virginia[order(dat.denom.virginia$year),]

  # summary of count of denom files per year and per FIPS
  dat.denom.summary = ddply(dat.denom,.(fips),nrow)
  print('Following FIPS codes have less than complete records')
  print(subset(dat.denom.summary,V1<length(years)))

  # temporarily get rid of Georgia and Virginia FIPS codes which are missing in denom file for 1999-2001
  # also eventually fix {{SSA 49270}} (FIPS 51595) and make the cases go into {{SSA 49400}} (FIPS 51081)
  # as per https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013710
  dat.merged = subset(dat.merged,!(fipscounty %in% virginia_excluded_counties))

  georgia_excluded_counties = sort(unique(dat.merged.na.summary$fipscounty)) ;
  georgia_excluded_counties = subset(georgia_excluded_counties,substr(georgia_excluded_counties,1,2)==13)
  dat.merged = subset(dat.merged,!(fipscounty %in% georgia_excluded_counties & year %in% c(1999:2001)))

  # are any of the prevalences greater than 1?
  dat.merged$rate = with(dat.merged,cases/population)
  dat.merged.not.na$rate = with(dat.merged.not.na,cases/population)

  print(paste0('Total number of cases is currently ',sum(dat.merged$cases)))

  # save processed rates file
  dir.output.local = paste0('~/data/morbidity/US/medicare/processed/')
  ifelse(!dir.exists(dir.output.local), dir.create(dir.output.local, recursive=TRUE), FALSE)
  saveRDS(dat.merged, paste0(dir.output.local,'medicare_rates_',label,'_',years[1],'_',years[length(years)],'.rds'))

}