#!/bin/bash

# this script
# demonstrates all the individual steps from the data processing all the way to the figures and tables for the final paper

clear

#################################################
# 1. PROCESS DATA
#################################################

# process hospitalization data
#./~/git/rmparks_coastal_storms_Jan_2020/code/00_bash/medicare_process/process_medicare.sh

# process tropical cyclone data
#./~/git/rmparks_tropical_cyclones_long_term_2020/code/00_bash/data_processing/process_wind_data.sh

# process temperature data
#./~/git/pollution/countries/USA/prog/00_bash/grid_county_intersection_raster_prism_fips.sh

#################################################
# 2. RUN MODELS
#################################################

# run models
#./~/git/rmparks_coastal_storms_Jan_2020/code/00_bash/model_running/model_run_update.sh

#################################################
# 3. PLOT FIGURES
#################################################

# plot figures
#./~/git/rmparks_coastal_storms_Jan_2020/code/00_bash/papers/medicare_coastal_storms_2020/figures.sh