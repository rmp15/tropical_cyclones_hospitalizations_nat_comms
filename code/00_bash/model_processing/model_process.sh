#!/bin/bash

# this script
# processes model outputs for plotting

clear

#################################################
# 1. BONFERRONI CORRECT MODEL RESULTS
#################################################

# CCS LEVEL 1
declare -a seeds=($(seq 0 15))
echo "Processing CCS Level 1 model files for integrated lag";

for seed in "${seeds[@]}"; do

# processes CCS Level 1 model files by seed value
#Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/model_analysis/integrated_lag_uncertainty_level1_bonferroni_corrected.R $seed

done;

# CCS LEVEL 3
declare -a seeds=($(seq 0 150))
echo "Processing CCS Level 3 model files for integrated lag";

for seed in "${seeds[@]}"; do

# processes CCS Level 3 model files by seed value
#Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/model_analysis/integrated_lag_uncertainty_level3_bonferroni_corrected.R

done;