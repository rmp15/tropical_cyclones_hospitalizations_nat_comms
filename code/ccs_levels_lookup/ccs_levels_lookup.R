# this code will take ccs documents
# make a match and then output a lookup across ccs levels

rm(list=ls())

# load the Bobb coding file and match up the codes (from paper https://www.ncbi.nlm.nih.gov/pubmed/25536257)
# downloaded from https://www.hcup-us.ahrq.gov/toolssoftware/ccs/ccs.jsp#examples

# 1. load and process CCS level 3

setwd('~/git/rmparks_coastal_storms_Jan_2020/data/Single_Level_CCS_2015/')

code.lookup = read.csv('$dxref 2015.csv',skip=1)[,c(1,2)]
code.lookup = code.lookup[c(2:nrow(code.lookup)),]
names(code.lookup) = c('icd9','ccs_level_3')

# get rid of all the weird quotation marks in look-up file
for(i in seq(dim(code.lookup)[2])){
    code.lookup[,i] = gsub("'","",code.lookup[,i])
    code.lookup[,i] = as.character(code.lookup[,i])
}

code.lookup$icd9 = trimws(code.lookup$icd9) ; code.lookup$icd9 = trimws(code.lookup$icd9)
code.lookup$ccs_level_3 = trimws(code.lookup$ccs_level_3) ; code.lookup$ccs_level_3 = trimws(code.lookup$ccs_level_3)

# full level 3 names to add to lookup
ccs.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('ccs_level_3','ccs_level_3_description')
ccs.names = ccs.names[c(4:nrow(ccs.names)),]
ccs.names$ccs_level_3 = as.character(ccs.names$ccs_level_3)

# add full level 3 names to ccs level 3 lookup
code.lookup = plyr::join(code.lookup,ccs.names)

# 2. load and process CCS level 1,2,3

setwd('~/git/rmparks_coastal_storms_Jan_2020/data/Multi_Level_CCS_2015/')

code.lookup.2 = read.csv('ccs_multi_dx_tool_2015.csv')[,c(1:3)]
names(code.lookup.2) = c('icd9','ccs_level_1','ccs_level_1_description')

# get rid of all the weird quotation marks in look-up file
for(i in seq(dim(code.lookup.2)[2])){
    code.lookup.2[,i] = gsub("'","",code.lookup.2[,i])
    code.lookup.2[,i] = as.character(code.lookup.2[,i])
}

code.lookup.2$icd9 = trimws(code.lookup.2$icd9) ; code.lookup.2$icd9 = trimws(code.lookup.2$icd9)
code.lookup.2$ccs_level_1 = trimws(code.lookup.2$ccs_level_1) ; code.lookup.2$ccs_level_1 = trimws(code.lookup.2$ccs_level_1)

# 3. match up the icd codings for two levels of lookup
code.lookup.merged = plyr::join(code.lookup,code.lookup.2)
code.lookup.merged = na.omit(code.lookup.merged)

# fix the mental illness typo
code.lookup.merged$ccs_level_1_description = ifelse(code.lookup.merged$ccs_level_1_description=='Mental Illness',
                                                'Mental illness',code.lookup.merged$ccs_level_1_description)

# save merged ccs file for next stage of processing
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
write.csv(code.lookup.merged, paste0(dir.output,'CCS_lookup_2015.csv'))

# 4. unique combinations of ccs level 1 and 3
code.lookup.unique = unique(code.lookup.merged[,c('ccs_level_1','ccs_level_1_description','ccs_level_3','ccs_level_3_description')])
write.csv(code.lookup.unique, paste0(dir.output,'CCS_lookup_2015_unique.csv'))