# list of packages to use
list.of.packages = c('dplyr','ggplot2','grid','gridExtra','hurricaneexposure','hurricaneexposuredata','lubridate',
                      'maptools','mapproj','plyr','raster','RColorBrewer','rgdal','rgeos','scales','sp','tidyverse'
                      )

# check if list of packages is installed. If not, it will install ones not yet installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages
lapply(list.of.packages, require, character.only = TRUE)