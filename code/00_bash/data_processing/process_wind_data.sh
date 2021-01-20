#!/bin/bash

# this script
# processes tropical cyclone data for the study

clear

declare -i start=1999
declare -i end=2016

#################################################
# 1. PROCESS STORM DATA
#################################################

echo "Processing tropical storm files";

echo "Processing wind data files";
Rscript /Users/rmiparks/git/rmparks_coastal_storms_Jan_2020/code/process_storm_data/process_wind_data.R $start $end