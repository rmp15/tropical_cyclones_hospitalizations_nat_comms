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

# make list of broad causes of hospitalization (temporary four causes for 200GB to delete when finished)
causes_groups = unique(as.character(code.lookup.merged$ccs_level_1_description))

# bonferroni correction CI calculation (assume that we have 13 broad causes, which results in a revised 95 -> 99.6,
# corresponding to z-score of 2.8782
causes_groups_final=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
                                    'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
                                    'Infectious and parasitic diseases','Musculoskeletal and connective tissue diseases',
                                    'Nervous system diseases','Skin and subcutaneous tissue diseases')
# bonferroni correction CI calculation
alpha = 100 * (0.05/length(causes_groups_final))
corrected.ci = (100 - alpha) / 100

# fill in data frame
dat.results = data.frame()

# loop through all causes
for(causes_group in causes_groups){

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
        # var(x+y) = var(x) + var(y) + 2*cov(x,y) which basically translates to
        # var(a+b+c+d+...) = (sum of all diagonal elements of matrix) + (sum of all off-diagonal elements) OR
        var_matrix = sum(vcov_matrix)
        se_matrix = sqrt(var_matrix)

        z_score_corrected = abs(qnorm((1-corrected.ci)/2))

        # calculate impact of all lags overall
        lag_est_mean_all_log_world = sum(mod_dlm_unconstrained$coefficients[1:8])
        lag_est_mean_all = exp(lag_est_mean_all_log_world)
        lag_est_mean_all_ll = exp(lag_est_mean_all_log_world-z_score_corrected*se_matrix)
        lag_est_mean_all_ul = exp(lag_est_mean_all_log_world+z_score_corrected*se_matrix)

        # calculate average impact of all lags together
        lag_est_mean_all_average = exp(lag_est_mean_all_log_world)/8
        lag_est_mean_all__average_ll = exp(lag_est_mean_all_log_world-z_score_corrected*se_matrix)/8
        lag_est_mean_all_average_ul = exp(lag_est_mean_all_log_world+z_score_corrected*se_matrix)/8

        dat.results.temp = data.frame(cause=causes_group,rr=lag_est_mean_all,rr.ll=lag_est_mean_all_ll,rr.ul=lag_est_mean_all_ul,
                                    rr.mean=lag_est_mean_all_average,rr.mean.ll=lag_est_mean_all__average_ll,rr.mean.ul=lag_est_mean_all_average_ul)
        dat.results = rbind(dat.results,dat.results.temp)

    }

    if(!file.exists(file_to_load)){
        print(paste0(causes_group,' not found'))
    }

    # provide summary for each lag if desired
    # lag_est_mean = as.data.frame(exp(mod_dlm_unconstrained$coefficients[1:8]))
    # lag_est_uncertainty = exp(confint.default(mod_dlm_unconstrained)[c(1:8),])
    # lag_est_uncertainty_alt = exp(confint(mod_dlm_unconstrained)[c(1:8),])
    #
    # dat.results = data.frame(lag=c(0:7),rr=lag_est_mean,rr.ll=lag_est_uncertainty[,1],rr.ul=lag_est_uncertainty[,2])
    # dat.results$cause = causes_group
    # rownames(dat.results) = seq(nrow(dat.results))
    # names(dat.results)[2] = 'rr'

    # using above to check that the uncertainties basically match
    # exp(mod_dlm_unconstrained$coefficients[1]+sqrt(vcov_matrix[1,1])*1.96)

}

# save model summary
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
write.csv(dat.results, paste0(dir.output.model.summary,'medicare_all_ccs_level_1_integrated_lag_model_summary_bonferroni_corrected_',years[1],'_',years[length(years)],'.csv'))
