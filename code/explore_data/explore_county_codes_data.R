# this code will take all processed years and load them
# then compare how to match up SSA codings with FIPS (why on earth is this the case?!)

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# expand grid of stuff temporary
years = c(1999:2014)

# ADMISSIONS

# load processed admissions file
dir.input.local = paste0('~/data/morbidity/US/medicare/processed/')
dat.admissions = readRDS(paste0(dir.input.local,'medicare_admissions_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.admissions)=1:nrow(dat.admissions)

# attach state names
state.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/name_lookup/ssa_standard_state_code_lookup.csv')
state.names = state.names[,c(1:4)]
dat.admissions = merge(dat.admissions,state.names,by.x='SSA_STATE_CD',by.y='code',all.x=TRUE)

# only have coastal states
dat.admissions = subset(dat.admissions,coastal_storm_state==1)

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

# find combination of unique SSA state and county codes which are in the look-up
ssa.not.missing = unique(na.omit(dat.admissions)[,c('SSA_STATE_CD','SSA_CNTY_CD','fipscounty')])
ssa.not.missing.summary = ddply(ssa.not.missing,.(SSA_STATE_CD),nrow)

# examine na values for admissions file
dat.admissions.na=dat.admissions[rowSums(is.na(dat.admissions))>0,]

# find combination of unique SSA state and county codes which are NOT in the look-up
ssa.missing = unique(dat.admissions.na[,c('SSA_STATE_CD','SSA_CNTY_CD','fipscounty')])
ssa.missing.summary = ddply(ssa.missing,.(SSA_STATE_CD),nrow)

# total percentage of cases which are missing
missing.percentage = 100*sum(dat.admissions.na$cases)/sum(dat.admissions$cases)
print(paste0('Missing percentage of cases is currently ',round(missing.percentage,2),'% (',sum(dat.admissions.na$cases),' out of ',sum(dat.admissions$cases),')'))

# output directory
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_county_codes_data/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# save records of non-missing and missing SSA codes
saveRDS(ssa.not.missing, paste0(dir.output,'medicare_admissions_not_missing_ssa_codes_',years[1],'_',years[length(years)],'.rds'))
saveRDS(ssa.missing, paste0(dir.output,'medicare_admissions_missing_ssa_codes_',years[1],'_',years[length(years)],'.rds'))

write.csv(ssa.not.missing, paste0(dir.output,'medicare_admissions_not_missing_ssa_codes_',years[1],'_',years[length(years)],'.csv'))
write.csv(ssa.missing, paste0(dir.output,'medicare_admissions_missing_ssa_codes_',years[1],'_',years[length(years)],'.csv'))

# DENOM FILES

# load processed denom file
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/medicare_denom_processing/')
dat.denom = readRDS(paste0(dir.input,'medicare_denom_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.denom)=1:nrow(dat.denom)
dat.denom$fips = paste0('0',dat.denom$fips)
dat.denom$fips = substr(dat.denom$fips, nchar(dat.denom$fips)-5+1, nchar(dat.denom$fips))
dat.denom$state.fips = substr(dat.denom$fips, 1,2)

# attach state names
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
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

# find unique values of FIPS in denom files
fips.denom = unique(na.omit(dat.denom)[,c('state.fips','fips')])
fips.denom = subset(fips.denom,state.fips!='0N')
fips.denom.summary = ddply(fips.denom,.(state.fips),nrow)

# save records of non-missing fips codes from denom files
saveRDS(fips.denom, paste0(dir.output,'medicare_denom_not_fips_codes_',years[1],'_',years[length(years)],'.rds'))
write.csv(fips.denom, paste0(dir.output,'medicare_denom_fips_codes_',years[1],'_',years[length(years)],'.csv'))

# merge admissions file with denom file
fips.denom$fips = as.numeric(fips.denom$fips)
admissions.denom.fips.merge = merge(ssa.not.missing,fips.denom,by.x='fipscounty',by.y='fips',all.x=TRUE)

# examine na values for merged file
admissions.denom.fips.merge.matched = admissions.denom.fips.merge[rowSums(is.na(admissions.denom.fips.merge))==0,]

# examine na values for merged file i.e., these counties are in the admissions files but not in the denominator files
admissions.denom.fips.merge.unmatched = admissions.denom.fips.merge[rowSums(is.na(admissions.denom.fips.merge))>0,]

# save records of non-missing and missing SSA codes
saveRDS(admissions.denom.fips.merge.matched, paste0(dir.output,'medicare_admissions_and_denom_matching_fips_',years[1],'_',years[length(years)],'.rds'))
saveRDS(admissions.denom.fips.merge.unmatched, paste0(dir.output,'medicare_admissions_and_denom_not_matching_fips_',years[1],'_',years[length(years)],'.rds'))

write.csv(admissions.denom.fips.merge.matched, paste0(dir.output,'medicare_admissions_and_denom_matching_fips_',years[1],'_',years[length(years)],'.csv'))
write.csv(admissions.denom.fips.merge.unmatched, paste0(dir.output,'medicare_admissions_and_denom_not_matching_fips_',years[1],'_',years[length(years)],'.csv'))

# summary of fips codes which do not match by matching the codes we are going to get rid of in the original admissions files
dat.admissions.999 = subset(dat.admissions,SSA_CNTY_CD==999)
missing.percentage.999 = 100*sum(dat.admissions.999$cases)/sum(dat.admissions$cases)
print(paste0('Missing percentage of cases is currently ',round(missing.percentage.999,2),'% (',sum(dat.admissions.999$cases),' out of ',sum(dat.admissions$cases),')'))