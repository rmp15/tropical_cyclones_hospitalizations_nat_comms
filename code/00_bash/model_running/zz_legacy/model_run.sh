#!/bin/bash

# this script
# processes Medicare admissions and denominator files

clear

declare -a seeds=($(seq 0 19))
declare -a lags=($(seq 0 0))

#################################################
# 1. SUMMARISE ORIGINAL MEDICARE FILES
#################################################

echo "Processing medicare files";

for seed in "${seeds[@]}"; do
for lag in "${lags[@]}"; do

# processes initial files by county, date, css death coding
Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/model_running/model_run.R $year $lag &

done; done;