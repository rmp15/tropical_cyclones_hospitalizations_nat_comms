PATH="/nfs/tools/lib/anaconda/3/bin:$PATH"
export CONDA_PKGS_DIRS=/nfs/nsaph_ci3/scratch/.conda/pkgs
export CONDA_ENVS_PATH=/nfs/nsaph_ci3/scratch/.conda/envs

#source activate nsaph
R --no-save < process_fst_files.R

# below are attempts to get the loop of years going but they are currently commented out

##!/bin/sh
##PBS -J 1999-2016
#R --slave --args ${PBS_ARRAY_INDEX} < process_fst_files_part1.R

#declare -a years=($(seq 1999 2016))
#for year in "${years[@]}"; do
#R --slave --args $year < process_fst_files_part1.R
#done;

# to run in RCE
#condor_submit process_fst_files_part3.submit