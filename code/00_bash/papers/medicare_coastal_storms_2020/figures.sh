#!/bin/bash

# this script
# runs necessary figure R scripts for tropical cyclone Medicare paper

clear

declare -i start=1999
declare -i end=2016

#################################################
# Figure 1 (extreme wind data summary)
#################################################

echo "plotting figure 1 for tropical cyclone paper $start - $end";

#Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/explore_data/explore_wind_data.R $start $end

#################################################
# Figure 2 (Medicare data summary)
#################################################

echo "plotting figure 2 for tropical cyclone paper $start - $end";
#Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/explore_data/explore_medicare_data.R $start 2014

#################################################
# Figure 3 (forest plot of CCS level 1)
#################################################

echo "plotting figure 3 for tropical cyclone paper $start - $end";
#Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/model_analysis/explore_unconstrained_dlm_model_level1_data.R $start 2014

#################################################
# Figure 4 (ER/non-ER)
#################################################

echo "plotting figure 4 for tropical cyclone paper $start - $end";
#Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/model_analysis/explore_unconstrained_dlm_model_level1_data.R $start 2014

#################################################
# Figure 5 (complete forest plot of CCS level 3 with CCS level 1)
#################################################

echo "plotting figure 5 for tropical cyclone paper $start - $end";
Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/model_analysis/explore_unconstrained_dlm_model_level1_er_non_er.R $start 2014

#################################################
# Figure 6 (effect modification by tropical cyclone or hurricane)
#################################################

echo "plotting figure 6 for tropical cyclone paper $start - $end";
#Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/model_analysis/explore_unconstrained_dlm_model_level1_hurricane_separate_data.R $start 2014

#################################################
# Figure 7 (excess hopsitalizations of CCS level 1)
#################################################

echo "plotting figure 7 for tropical cyclone paper $start - $end";
#Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/excess_hospitalizations/excess_hospitalizations.R $start 2014
#Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/excess_hospitalizations/excess_hospitalizations_update.R $start 2014

#################################################
# Supplementary Figure 3 (comparison of models)
#################################################

echo "plotting Supplementary Figure 3 for tropical cyclone paper $start - $end";
#Rscript ~/git/rmparks_coastal_storms_Jan_2020/code/model_analysis/compare_models.R $start 2014
