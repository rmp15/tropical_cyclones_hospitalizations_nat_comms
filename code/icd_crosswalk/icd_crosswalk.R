# this code will take the icd cm lookups
# and create a lookup file for ccs across icd9/10

rm(list=ls())

# load packages
library(NSAPHutils)
library(NSAPHplatform)
#set_threads()
library(fst)
library(data.table)
library(lubridate)
library(icd)
library(here)

# if icd doesn't exist
#icd_package_loc = paste0(project.folder,'icd_package/')
#install.packages(paste0(icd_package_loc,'icd_4.0.9.tar.gz'), repos = NULL, type ="source")

# Declare root directory (replace with commonly sourced file locations later)
project.folder = paste0(print(here::here()),'/')

# 1. load existing CCS look-up file for ICD 9-CM
code.lookup = read.csv(paste0(project.folder,'data/Single_Level_CCS_2015/$dxref 2015.csv'),skip=1)
names(code.lookup) = c('icd9','css_category','css_category_description','icd9_code_description')
code.lookup$icd9 = trimws(code.lookup$icd9) ; code.lookup$icd9 = trimws(code.lookup$icd9)

# get rid of all the weird quotation marks in look-up file
for(i in seq(dim(code.lookup)[2])){
    code.lookup[,i] = gsub("'","",code.lookup[,i])
    code.lookup[,i] = as.character(code.lookup[,i])
}
code.lookup$icd9 = trimws(code.lookup$icd9) ; code.lookup$icd9 = trimws(code.lookup$icd9)
code.lookup$css_category = trimws(code.lookup$css_category) ; code.lookup$icd9 = trimws(code.lookup$css_category)
all_ccs_codes = sort(unique(as.character(code.lookup$css_category)))

# follow
# https://github.com/NSAPH/data_requests/blob/master/request_projects/jun2020_heatwave_hospitalizations/code/2_classify_hosps.R

## Create icd code lists
ccs_codes <- list()

## Load maps from the ICD package
data("icd10_map_ccs")
data("icd9_map_single_ccs")

## Harmonize structures/names for icd9/icd10
icd10_map_ccs <- icd10_map_ccs$single
names(icd10_map_ccs) <- paste0(names(icd10_map_ccs))
names(icd9_map_single_ccs) <- paste0(names(icd9_map_single_ccs))

# map each ccs to each ICD 9 and 10
selected_codes <- paste0(all_ccs_codes)
for (code in selected_codes) {
  ccs_codes[[code]][["icd10"]] <- icd10_map_ccs[[code]]
  ccs_codes[[code]][["icd9"]] <- icd9_map_single_ccs[[code]]
}

# save file
dir.output = paste0(paste0(project.folder,'data/ccs_icd_crosswalk/'))
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
saveRDS(ccs_codes, paste0(dir.output,'ccs_icd_crosswalk.rds'))