## Code that creates files from the admission and denominator files that
## comply with the CMS cell suppression policy (no values calculated on 10 or
## fewer individuals). 
##
## Author: Ben Sabath
## Date: 3/3/2020

library(data.table)

for (year in 1999:2014) {
  file_name <- paste0("../data/medicare_admissions_processing/medicare_admissions_processing_",
                      year,
                      ".rds")
  x <- as.data.table(readRDS(file_name))
  x <- x[cases > 10]
  saveRDS(x, paste0("../data/medicare_admissions_processing/downloadable/medicare_admissions_processing_",
                   year,
                   ".rds"))
}

x <- as.data.table(readRDS("../data/medicare_denom_processing/medicare_denom_1999_2014.rds"))
x <- x[population > 10]
saveRDS(x, "../data/medicare_denom_processing/downloadable/medicare_denom_1999_2014.rds")