#!/bin/bash

# this script
# processes Medicare admissions and denominator files

clear

##################################################################################################
# 1. SUMMARISE ORIGINAL MEDICARE FILES FOR MAIN ANALYSIS
##################################################################################################

echo "Processing medicare files for main analysis";

# batch process each individual year's admissions files 50,000 rows at a time
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part1/process_fst_files.submit

# load all years in admissions files and summarise across entire dataset
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part2/process_fst_files_part2.submit

# batch process each individual year's denominator files 50,000 rows at a time
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part3/process_fst_files_part3.submit

# load all years in denominator files and summarise across entire dataset
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part4/process_fst_files_part4.submit

# calculate admissions rates based on processed admissions and denominator files
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part5/process_fst_files_part5.submit

# create a complete grid of admissions and rates for each CCS cause separately
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part6/process_fst_files_part6.submit

##################################################################################################
# 2. SUMMARISE ORIGINAL MEDICARE FILES FOR EMERGENCY/NON-EMERGENCY ANALYSIS
##################################################################################################

# batch process each individual year's admissions files 50,000 rows at a time
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part1/process_fst_files_er.submit

# load all years in admissions files and summarise across entire dataset
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part2/process_fst_files_er_part2.submit

# PART 3 doesn't need to be repeated from above
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part4/process_fst_files_part4.submit

# PART 4 doesn't need to be repeated from above
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part5/process_fst_files_part5.submit

# calculate admissions rates based on processed admissions and denominator files
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part5/process_fst_files_er_part5.submit

# create a complete grid of admissions and rates for each CCS cause separately
#condor_submit ~/git/rmparks_coastal_storms_Jan_2020/code/process_fst_files_part6/process_fst_files_er_part6.submit