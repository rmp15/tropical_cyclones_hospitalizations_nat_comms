# this code will take a particular cause of death (CCS level 3)
# and calculate integrated lag parameters
# NEED TO RUN ON RCE

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
seedVal = as.numeric(args[1]) + 1

# for testing
# seedVal = 1

# years of analysis
years = c(1999:2014)

library(gnm) ; library(splines) ; library(dlnm)

# output directory
dir.output.model.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/output/')

# cause groups
code.lookup.merged = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/CCS_lookup_2015.csv')
code.lookup.merged$X = NULL

# make list of broad causes of hospitalization
causes_groups_level_1 = unique(as.character(code.lookup.merged$ccs_level_1_description))

for(causes_group_level_1 in causes_groups_level_1){

    print(causes_group_level_1)

    # isolate the groups of CCS level 3 which fall into CCS level 1
    code.lookup.merged.subset = subset(code.lookup.merged, ccs_level_1_description==causes_group_level_1)
    causes_groups = unique(as.character(code.lookup.merged.subset$ccs_level_3_description))

    # fill in data frame
    dat.results = data.frame()

    # loop through all causes
    for(causes_group in causes_groups){

        causes_group = gsub(' ','_',causes_group)

        file_to_load = paste0(dir.output.model.output,'medicare_',causes_group,'_model_summary_update_',years[1],'_',years[length(years)],'.rds')

        if(file.exists(file_to_load)){

            print(causes_group)

            # load summary of model results
            mod_dlm_unconstrained = readRDS(file_to_load)

            # extract variance-covariance matrix to then use formula that
            # then take squre root and then multiply by Bonferroni-corrected z-value to get thing to add to
            vcov_matrix = vcov(mod_dlm_unconstrained)
            vcov_matrix = vcov_matrix[1:8,1:8]

            # var(x+y) = var(x) + var(y) + 2*cov(x,y) which basically translates to
            # var(a+b+c+d+...) = (sum of all diagonal elements of matrix) + (sum of all off-diagonal elements) OR
            var_matrix = sum(vcov_matrix)
            se_matrix = sqrt(var_matrix)

            z_score_corrected = 1.96 # currently uncorrected for 95% CI

            # calculate impact of all lags overall
            lag_est_mean_all_log_world = sum(mod_dlm_unconstrained$coefficients[1:8])
            lag_est_mean_all = exp(lag_est_mean_all_log_world)
            lag_est_mean_all_ll = exp(lag_est_mean_all_log_world-z_score_corrected*se_matrix)
            lag_est_mean_all_ul = exp(lag_est_mean_all_log_world+z_score_corrected*se_matrix)

            # calculate average impact of all lags together
            lag_est_mean_all_average = (exp(lag_est_mean_all_log_world)-1)/8
            lag_est_mean_all__average_ll = (exp(lag_est_mean_all_log_world-z_score_corrected*se_matrix)-1)/8
            lag_est_mean_all_average_ul = (exp(lag_est_mean_all_log_world+z_score_corrected*se_matrix)-1)/8

            dat.results.temp = data.frame(ccs_level_1 = causes_group_level_1, cause=causes_group,rr=lag_est_mean_all,rr.ll=lag_est_mean_all_ll,rr.ul=lag_est_mean_all_ul,
                                        err.mean=lag_est_mean_all_average,err.mean.ll=lag_est_mean_all__average_ll,err.mean.ul=lag_est_mean_all_average_ul)

            dat.results = rbind(dat.results,dat.results.temp)

        }

        if(!file.exists(file_to_load)){
            print(paste0(causes_group,' not found'))
        }

    }

    # save model summary
    dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
    write.csv(dat.results, paste0(dir.output.model.summary,'medicare_all_ccs_level_3_',causes_group_level_1,'_integrated_lag_model_summary_bonferroni_corrected_',years[1],'_',years[length(years)],'.csv'))

}

