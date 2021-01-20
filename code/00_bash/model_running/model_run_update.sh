#!/bin/bash

# this script
# runs model for associating tropical cyclone with hospitalization outcomes

clear

#################################################
# 1. RUN MODELS
#################################################

# run CCS Level 1 models
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/model_running/ccs_level_1/model_run_unconstrained_dlm_update.submit
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/model_running/ccs_level_1/model_run_unconstrained_dlm_w_hurricanes_update.submit

# run CCS Level 3 models
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/model_running/ccs_level_3/model_run_unconstrained_dlm_update.submit