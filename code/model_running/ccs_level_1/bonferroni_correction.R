# this code will take a particular cause of death (CCS level 1)
# and load the chosen model for it
# then do a Bonferroni correction

# NEED TO RUN ON RCE

rm(list=ls())

# years of analysis
years = c(1999:2014)

# load ccs lookup
code.lookup.merged = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/CCS_lookup_2015.csv')
code.lookup.merged$X = NULL

# make list of broad causes of hospitalization
causes_groups = unique(as.character(code.lookup.merged$ccs_level_1_description))
causes_groups_final=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
                                    'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
                                    'Infectious and parasitic diseases','Musculoskeletal and connective tissue diseases',
                                    'Nervous system diseases','Skin and subcutaneous tissue diseases')

# bonferroni correction CI calculation
alpha = 100 * (0.05/length(causes_groups_final))
corrected.ci = round(100 - alpha,1) / 100

# load summary of model results
for(cause_group in causes_groups){

    dir.output.model.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/output/')
    current.file = paste0(dir.output.model.output,'medicare_',cause_group,'_model_summary_',years[1],'_',years[length(years)],'.rds')

    if(file.exists(current.file)){

        print(current.file)
        mod_dlm_unconstrained = readRDS(current.file)

        # provide summary for corrected and uncorrected CIs
        lag_est_mean = as.data.frame(exp(mod_dlm_unconstrained$coefficients[1:8]))
        lag_est_uncertainty = exp(confint.default(mod_dlm_unconstrained)[c(1:8),])
        lag_est_uncertainty_bf_corrected = exp(confint.default(mod_dlm_unconstrained,level=corrected.ci)[c(1:8),])

        dat.results = data.frame(lag=c(0:7),rr=lag_est_mean,
                                            rr.ll=lag_est_uncertainty[,1],rr.ul=lag_est_uncertainty[,2],
                                            rr.ll.bfc=lag_est_uncertainty_bf_corrected[,1],rr.ul.bfc=lag_est_uncertainty_bf_corrected[,2])
        dat.results$cause = cause_group
        rownames(dat.results) = seq(nrow(dat.results))
        names(dat.results)[2] = 'rr'

        # save model summary
        dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
        write.csv(dat.results, paste0(dir.output.model.summary,'medicare_',cause_group,'_model_summary_bonferroni_corrected_',years[1],'_',years[length(years)],'.csv'))
    }

}

