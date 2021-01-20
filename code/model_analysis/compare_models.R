# this code will take the versions of models, load them
# then compare outputs

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

print(args)

# years of study
start_year = as.numeric(args[1]) # 1999
end_year = as.numeric(args[2]) # 2014

years=c(start_year:end_year)

# load CCS level 1 and 3 names
code.lookup.merged = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/CCS_lookup_2015.csv')
code.lookup.merged$X = NULL
code.lookup.merged = unique(code.lookup.merged[,c('ccs_level_3','ccs_level_3_description','ccs_level_1','ccs_level_1_description')])
code.lookup.merged$ccs_level_3_description = as.character(code.lookup.merged$ccs_level_3_description)
code.lookup.merged$ccs_level_1_description = as.character(code.lookup.merged$ccs_level_1_description)
ccs_level_3 = unique(as.character(code.lookup.merged$ccs_level_3_description))
ccs_level_1 = unique(as.character(code.lookup.merged$ccs_level_1_description))

# load model summaries for CCS level 1 (new and updated in May 2020 to have day of year and explicitly take out day of week)
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dat.results = data.frame()
for(cod in ccs_level_1){
    file.current = paste0(dir.output.model.summary,'medicare_',cod,'_model_summary_update_',years[1],'_',years[length(years)],'.csv')
    if (file.exists(file.current)){
    print(cod)
    dat.results.current = read.csv(file.current)
    dat.results.current$ccs_level_3_description = dat.results.current$ccs_level_1_description = dat.results.current$cause
    dat.results.current$X = dat.results.current$cause = NULL
    dat.results = rbind(dat.results.current, dat.results)}
}
# exclude anything which is going in 'other'
dat.results = subset(dat.results,!(ccs_level_3_description%in%c('Congenital anomalies')))
# create lag factor values for ggplot
dat.results$lag.factor = factor(dat.results$lag, levels=c(7:0))

# rename causes CCS level 1
dat.results$ccs_level_1_description = gsub('Diseases of the circulatory system', 'Cardiovascular diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the respiratory system', 'Respiratory diseases', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Neoplasms', 'Cancers', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Injury and poisoning', 'Injuries', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Mental illness', 'Neuropsychiatric\ndisorders', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Diseases of the blood and blood-forming organs', 'Blood diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the digestive system', 'Digestive system\ndiseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", 'Endocrine disorders', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the genitourinary system', 'Genitourinary diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Infectious and parasitic diseases', 'Infectious and parasitic\ndiseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective\ntissue diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the nervous system and sense organs', 'Nervous system\ndiseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the skin and subcutaneous tissue', 'Skin and subcutaneous\ntissue diseases', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Congenital anomalies', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Residual codes unclassified all E codes 259 and 260', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Certain conditions originating in the perinatal period', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Complications of pregnancy childbirth and the puerperium', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Symptoms signs and ill-defined conditions and factors influencing health status', 'Other', dat.results$ccs_level_1_description)

# reorder CCS level 1 causes for plotting
dat.results$ccs_level_1_description = factor(dat.results$ccs_level_1_description,
                        levels=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric\ndisorders',
                                    'Blood diseases','Digestive system\ndiseases','Endocrine disorders','Genitourinary diseases',
                                    'Infectious and parasitic\ndiseases','Musculoskeletal and connective\ntissue diseases',
                                    'Nervous system\ndiseases','Skin and subcutaneous\ntissue diseases'))

dat.results.main = dat.results

# load model summaries for CCS level 1 (original with day of week in strata)
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dat.results = data.frame()
for(cod in ccs_level_1){
    file.current = paste0(dir.output.model.summary,'medicare_',cod,'_model_summary_',years[1],'_',years[length(years)],'.csv')
    if (file.exists(file.current)){
    print(cod)
    dat.results.current = read.csv(file.current)
    dat.results.current$ccs_level_3_description = dat.results.current$ccs_level_1_description = dat.results.current$cause
    dat.results.current$X = dat.results.current$cause = NULL
    dat.results = rbind(dat.results.current, dat.results)}
}
# exclude anything which is going in 'other'
dat.results = subset(dat.results,!(ccs_level_3_description%in%c('Congenital anomalies')))
# create lag factor values for ggplot
dat.results$lag.factor = factor(dat.results$lag, levels=c(7:0))

# rename causes CCS level 1
dat.results$ccs_level_1_description = gsub('Diseases of the circulatory system', 'Cardiovascular diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the respiratory system', 'Respiratory diseases', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Neoplasms', 'Cancers', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Injury and poisoning', 'Injuries', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Mental illness', 'Neuropsychiatric\ndisorders', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Diseases of the blood and blood-forming organs', 'Blood diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the digestive system', 'Digestive system\ndiseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", 'Endocrine disorders', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the genitourinary system', 'Genitourinary diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Infectious and parasitic diseases', 'Infectious and parasitic\ndiseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective\ntissue diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the nervous system and sense organs', 'Nervous system\ndiseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the skin and subcutaneous tissue', 'Skin and subcutaneous\ntissue diseases', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Congenital anomalies', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Residual codes unclassified all E codes 259 and 260', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Certain conditions originating in the perinatal period', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Complications of pregnancy childbirth and the puerperium', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Symptoms signs and ill-defined conditions and factors influencing health status', 'Other', dat.results$ccs_level_1_description)

# reorder CCS level 1 causes for plotting
dat.results$ccs_level_1_description = factor(dat.results$ccs_level_1_description,
                        levels=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric\ndisorders',
                                    'Blood diseases','Digestive system\ndiseases','Endocrine disorders','Genitourinary diseases',
                                    'Infectious and parasitic\ndiseases','Musculoskeletal and connective\ntissue diseases',
                                    'Nervous system\ndiseases','Skin and subcutaneous\ntissue diseases'))

dat.results.old = dat.results

# load model summaries for CCS level 1 (main but with no temperature)
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dat.results = data.frame()
for(cod in ccs_level_1){
    file.current = paste0(dir.output.model.summary,'medicare_',cod,'_model_summary_update_no_temp_',years[1],'_',years[length(years)],'.csv')
    if (file.exists(file.current)){
    print(cod)
    dat.results.current = read.csv(file.current)
    dat.results.current$ccs_level_3_description = dat.results.current$ccs_level_1_description = dat.results.current$cause
    dat.results.current$X = dat.results.current$cause = NULL
    dat.results = rbind(dat.results.current, dat.results)}
}
# exclude anything which is going in 'other'
dat.results = subset(dat.results,!(ccs_level_3_description%in%c('Congenital anomalies')))
# create lag factor values for ggplot
dat.results$lag.factor = factor(dat.results$lag, levels=c(7:0))

# rename causes CCS level 1
dat.results$ccs_level_1_description = gsub('Diseases of the circulatory system', 'Cardiovascular diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the respiratory system', 'Respiratory diseases', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Neoplasms', 'Cancers', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Injury and poisoning', 'Injuries', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Mental illness', 'Neuropsychiatric\ndisorders', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Diseases of the blood and blood-forming organs', 'Blood diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the digestive system', 'Digestive system\ndiseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", 'Endocrine disorders', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the genitourinary system', 'Genitourinary diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Infectious and parasitic diseases', 'Infectious and parasitic\ndiseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective\ntissue diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the nervous system and sense organs', 'Nervous system\ndiseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the skin and subcutaneous tissue', 'Skin and subcutaneous\ntissue diseases', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Congenital anomalies', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Residual codes unclassified all E codes 259 and 260', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Certain conditions originating in the perinatal period', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Complications of pregnancy childbirth and the puerperium', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Symptoms signs and ill-defined conditions and factors influencing health status', 'Other', dat.results$ccs_level_1_description)

# reorder CCS level 1 causes for plotting
dat.results$ccs_level_1_description = factor(dat.results$ccs_level_1_description,
                        levels=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric\ndisorders',
                                    'Blood diseases','Digestive system\ndiseases','Endocrine disorders','Genitourinary diseases',
                                    'Infectious and parasitic\ndiseases','Musculoskeletal and connective\ntissue diseases',
                                    'Nervous system\ndiseases','Skin and subcutaneous\ntissue diseases'))

dat.results.no.temp = dat.results

# create a single dataframe for plotting
dat.results.main = dat.results.main[,c(1:4,7)] ; names(dat.results.main)[c(2:4)] = c('rr.main','rr.ll.main','rr.ul.main')
dat.results.old = dat.results.old[,c(2:4)] ; names(dat.results.old)[c(1:3)] = c('rr.old','rr.ll.old','rr.ul.old')
dat.results.no.temp = dat.results.no.temp[,c(2:4)] ; names(dat.results.no.temp)[c(1:3)] = c('rr.no.temp','rr.ll.no.temp','rr.ul.no.temp')

dat.results.merged = cbind(dat.results.main, dat.results.old, dat.results.no.temp)

# source variables for colorinng etc.
source('~/git/rmparks_coastal_storms_Jan_2020/data/objects/objects.R')

# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_unconstrained_dlm_model_data/',start_year,'_',end_year,'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

library(ggplot2)

# plot all together in class forest plot style
pdf(paste0(dir.output,'compare_models_main_vs_old_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=subset(dat.results.merged), aes(x=rr.main-1,y=rr.old-1)) +
    geom_point(size=3,shape=16) +
    xlab('Coefficients from main model') + ylab('Coefficients from old model') +
    geom_errorbar(aes(ymin=rr.ll.main-1,ymax=rr.ul.main-1),alpha=0.2) +
    geom_errorbarh(aes(xmin=rr.ll.old-1,xmax=rr.ul.old-1),alpha=0.2) +
    geom_abline(a=0,b=1) +
    # facet_wrap(vars(ccs_level_1_description)) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
    coord_equal() +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(color=guide_legend(title="",nrow=1)) +
    guides(color=FALSE) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 9),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=9),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# plot all together in class forest plot style
pdf(paste0(dir.output,'compare_models_main_vs_old_facet_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=subset(dat.results.merged), aes(x=rr.main-1,y=rr.old-1)) +
    geom_point(size=1,shape=16) +
    xlab('Coefficients from main model') + ylab('Coefficients from old model') +
    geom_errorbar(aes(ymin=rr.ll.main-1,ymax=rr.ul.main-1),alpha=0.2) +
    geom_errorbarh(aes(xmin=rr.ll.old-1,xmax=rr.ul.old-1),alpha=0.2) +
    geom_abline(a=0,b=1) +
    facet_wrap(vars(ccs_level_1_description)) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
    coord_equal() +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(color=guide_legend(title="",nrow=1)) +
    guides(color=FALSE) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 9),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=9),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# plot all together in class forest plot style
pdf(paste0(dir.output,'compare_models_main_vs_notemp_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=subset(dat.results.merged), aes(x=rr.main-1,y=rr.no.temp-1)) +
    geom_point(size=3,shape=16) +
    xlab('Coefficients from main model') + ylab('Coefficients from no temp model') +
    geom_errorbar(aes(ymin=rr.ll.main-1,ymax=rr.ul.main-1),alpha=0.2) +
    geom_errorbarh(aes(xmin=rr.ll.no.temp-1,xmax=rr.ul.no.temp-1),alpha=0.2) +
    geom_abline(a=0,b=1) +
    # facet_wrap(vars(ccs_level_1_description)) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
    coord_equal() +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(color=guide_legend(title="",nrow=1)) +
    guides(color=FALSE) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 9),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=9),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# plot all together in class forest plot style
pdf(paste0(dir.output,'compare_models_main_vs_notemp_facet_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=subset(dat.results.merged), aes(x=rr.main-1,y=rr.no.temp-1)) +
    geom_point(size=1,shape=16) +
    xlab('Coefficients from main model') + ylab('Coefficients from no temp model') +
    geom_errorbar(aes(ymin=rr.ll.main-1,ymax=rr.ul.main-1),alpha=0.2) +
    geom_errorbarh(aes(xmin=rr.ll.no.temp-1,xmax=rr.ul.no.temp-1),alpha=0.2) +
    geom_abline(a=0,b=1) +
    facet_wrap(vars(ccs_level_1_description)) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
    coord_equal() +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(color=guide_legend(title="",nrow=1)) +
    guides(color=FALSE) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 9),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=9),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()
