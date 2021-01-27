# this code will take all processed wind years and load them
# then explore with figure output

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

print(args)

# years of study
start_year = as.numeric(args[1]) # 1999
end_year = as.numeric(args[2]) # 2014
# lag_chosen = as.numeric(args[3]) # 1

# for test runs
#start_year=1999 ; end_year=2014

years=c(start_year:end_year)

# load CCS level 1 and 3 names
code.lookup.merged = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/CCS_lookup_2015.csv')
code.lookup.merged$X = NULL
code.lookup.merged = unique(code.lookup.merged[,c('ccs_level_3','ccs_level_3_description','ccs_level_1','ccs_level_1_description')])
code.lookup.merged$ccs_level_3_description = as.character(code.lookup.merged$ccs_level_3_description)
code.lookup.merged$ccs_level_1_description = as.character(code.lookup.merged$ccs_level_1_description)
ccs_level_3 = unique(as.character(code.lookup.merged$ccs_level_3_description))
ccs_level_1 = unique(as.character(code.lookup.merged$ccs_level_1_description))
ccs_level_1 = ccs_level_1[-c(10,13,14,16,17)]

# for(lag_chosen in c(0:7)){

# load model summaries for CCS level 3 grouped by CCS level 1
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dat.results = data.frame()
for(causes_group_level_1 in ccs_level_1){
    file.current = paste0(dir.output.model.summary,'medicare_all_ccs_level_3_',causes_group_level_1,'_integrated_lag_model_summary_bonferroni_corrected_',years[1],'_',years[length(years)],'.csv')
    if (file.exists(file.current)){
    print(causes_group_level_1)
    dat.results.current = read.csv(file.current)
    dat.results.current$X = NULL
    dat.results = rbind(dat.results.current, dat.results)}
}

# rename cause level 3 to match rest of code
names(dat.results)[1] = 'ccs_level_1_description'
names(dat.results)[2] = 'ccs_level_3_description'

# filter by appropriate lag
# dat.results = subset(dat.results,lag==lag_chosen)
rownames(dat.results) = seq(1:nrow(dat.results))

dat.results.level.3 = dat.results
dat.results.level.3$top = 0

dat.results.level.3$ccs_level_3 = NULL

# get rid of hyphens
dat.results.level.3$ccs_level_3_description = gsub('_', ' ', dat.results.level.3$ccs_level_3_description)

dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_medicare_data/',years[1],'_',years[length(years)],'/')
num_hosps = read.csv(paste0(dir.output,'table_by_ccs_level_3_',years[1],'_',years[length(years)],'.csv'))
num_hosps$ccs_level_3_description = gsub(';', '', num_hosps$ccs_level_3_description)

dat.results.level.3 = merge(dat.results.level.3,num_hosps,by=c('ccs_level_3_description'),all.X=TRUE)

# get rid of unnecessary column
dat.results.level.3$ccs_level_3 = NULL

# load model summaries for CCS level 1
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dat.results = read.csv(paste0(dir.output.model.summary,'medicare_all_ccs_level_1_integrated_lag_model_summary_bonferroni_corrected_',years[1],'_',years[length(years)],'.csv'))

# filter by appropriate lag
# dat.results = subset(dat.results,lag==lag_chosen)
rownames(dat.results) = seq(1:nrow(dat.results))
dat.results$X = NULL

dat.results.level.1 = dat.results
dat.results.level.1$top = 1
dat.results.level.1$cases = 999999

# fix names
names(dat.results.level.1)[1] = 'ccs_level_1_description'
dat.results.level.1$ccs_level_3_description = dat.results.level.1$ccs_level_1_description

# add average effects
dat.results.level.1$err.mean = with(dat.results.level.1, (rr-1)/8) ; dat.results.level.1$rr.mean = NULL
dat.results.level.1$err.mean.ll = with(dat.results.level.1, (rr.ll-1)/8) ; dat.results.level.1$rr.mean.ll = NULL
dat.results.level.1$err.mean.ul = with(dat.results.level.1, (rr.ul-1)/8) ; dat.results.level.1$rr.mean.ul = NULL

# reorder to match level 1
dat.results.level.3 = dat.results.level.3[,names(dat.results.level.1)]

# combine level 3 and 1 levels
dat.results.total = rbind(dat.results.level.1,dat.results.level.3)

# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_unconstrained_dlm_model_data/',start_year,'_',end_year,'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

library(RColorBrewer)
library(gridExtra)
library(scales)
library(plyr)
library(ggplot2)

# source variables for colorinng etc.
source('~/git/rmparks_coastal_storms_Jan_2020/data/objects/objects.R')

# inclusion criteria by total number of cases in sub-group
include_limits = c(0,50000,100000,200000,500000)

# MANUALLY EXCLUDE REALLY WEIRD CCS LEVEL 1s TO FINISH
causes_level_3_to_exclude = c('Influenza')
causes_level_1_to_exclude = c('Congenital anomalies','Symptoms; signs; and ill-defined conditions and factors influencing health status')
dat.results.total = subset(dat.results.total,!(ccs_level_3_description%in%causes_level_3_to_exclude))
dat.results.total = subset(dat.results.total,!(ccs_level_1_description%in%causes_level_1_to_exclude))

# change names of CCS level 1 and 3 titles to match other figures (make into function to generalise when have time)
dat.results.total$ccs_level_1_description = as.character(dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description <- gsub(';', '', dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description <- gsub('\\.', '', dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description <- gsub('\\[', '', dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description <- gsub('\\]', '', dat.results.total$ccs_level_1_description)

dat.results.total$ccs_level_1_description = gsub('Diseases of the circulatory system', 'Cardiovascular diseases',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Diseases of the respiratory system', 'Respiratory diseases',  dat.results.total$ccs_level_1_description)

dat.results.total$ccs_level_1_description = gsub('Neoplasms', 'Cancers',  dat.results.total$ccs_level_1_description)

dat.results.total$ccs_level_1_description = gsub('Injury and poisoning', 'Injuries',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Mental illness', 'Neuropsychiatric disorders',  dat.results.total$ccs_level_1_description)

dat.results.total$ccs_level_1_description = gsub('Diseases of the blood and blood-forming organs', 'Blood diseases',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Diseases of the digestive system', 'Digestive system diseases',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub("Endocrine nutritional and metabolic diseases and immunity disorders", 'Endocrine disorders',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Diseases of the genitourinary system', 'Genitourinary diseases',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Infectious and parasitic diseases', 'Infectious and parasitic diseases',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective tissue diseases',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Musculoskeletal and connective tissue diseases', 'Musculoskeletal and\nconnective tissue diseases',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Diseases of the nervous system and sense organs', 'Nervous system diseases',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub("Diseases of the skin and subcutaneous tissue", 'Skin and subcutaneous tissue diseases',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub("Skin and subcutaneous tissue diseases", 'Skin and subcutaneous\ntissue diseases',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Congenital anomalies', 'Other',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Residual codes unclassified all E codes 259 and 260', 'Other',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Certain conditions originating in the perinatal period', 'Other',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Complications of pregnancy childbirth and the puerperium', 'Other',  dat.results.total$ccs_level_1_description)
dat.results.total$ccs_level_1_description = gsub('Symptoms signs and ill-defined conditions and factors influencing health status', 'Other',  dat.results.total$ccs_level_1_description)

# reorder CCS level 1 causes for plotting
dat.results.total$ccs_level_1_description = factor(dat.results.total$ccs_level_1_description,
                        levels=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
                                    'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
                                    'Infectious and parasitic diseases','Musculoskeletal and\nconnective tissue diseases',
                                    'Nervous system diseases','Skin and subcutaneous\ntissue diseases','Other'))

dat.results.total$ccs_level_3_description = as.character(dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description <- gsub(';', '', dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description <- gsub('\\.', '', dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description <- gsub('\\[', '', dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description <- gsub('\\]', '', dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description <- gsub('\\(', '', dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description <- gsub('\\)', '', dat.results.total$ccs_level_3_description)


dat.results.total$ccs_level_3_description = gsub('Diseases of the circulatory system', 'Cardiovascular diseases',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Diseases of the respiratory system', 'Respiratory diseases',  dat.results.total$ccs_level_3_description)

dat.results.total$ccs_level_3_description = gsub('Neoplasms', 'Cancers',  dat.results.total$ccs_level_3_description)

dat.results.total$ccs_level_3_description = gsub('Injury and poisoning', 'Injuries',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Mental illness', 'Neuropsychiatric disorders',  dat.results.total$ccs_level_3_description)

dat.results.total$ccs_level_3_description = gsub('Diseases of the blood and blood-forming organs', 'Blood diseases',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Diseases of the digestive system', 'Digestive system diseases',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub("Endocrine nutritional and metabolic diseases and immunity disorders", 'Endocrine disorders',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Diseases of the genitourinary system', 'Genitourinary diseases',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Infectious and parasitic diseases', 'Infectious and parasitic diseases',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective tissue diseases',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Musculoskeletal and connective tissue diseases', 'Musculoskeletal and\nconnective tissue diseases',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Diseases of the nervous system and sense organs', 'Nervous system diseases',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub("Diseases of the skin and subcutaneous tissue", 'Skin and subcutaneous tissue diseases',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub("Skin and subcutaneous tissue diseases", 'Skin and subcutaneous\ntissue diseases',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Congenital anomalies', 'Other',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Residual codes unclassified all E codes 259 and 260', 'Other',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Certain conditions originating in the perinatal period', 'Other',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Complications of pregnancy childbirth and the puerperium', 'Other',  dat.results.total$ccs_level_3_description)
dat.results.total$ccs_level_3_description = gsub('Symptoms signs and ill-defined conditions and factors influencing health status', 'Other',  dat.results.total$ccs_level_3_description)

# some actual level 3 names are too long
code.lookup.shorter = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/CCS_lookup_2015_unique_edit.csv')
code.lookup.shorter = code.lookup.shorter[,c(4:5)]

dat.results.total = merge(dat.results.total,code.lookup.shorter,by='ccs_level_3_description',all.x=TRUE)
dat.results.total$ccs_level_3_description_alt = ifelse(dat.results.total$ccs_level_3_description==dat.results.total$ccs_level_1_description, as.character(dat.results.total$ccs_level_1_description),as.character(dat.results.total$ccs_level_3_description_alt))

dat.results.total$ccs_level_3_description_alt = ifelse(is.na(dat.results.total$ccs_level_3_description_alt)==TRUE, as.character(dat.results.total$ccs_level_3_description),as.character(dat.results.total$ccs_level_3_description_alt))
dat.results.total$ccs_level_3_description_alt = as.factor(dat.results.total$ccs_level_3_description_alt)

dat.results.total$ccs_level_3_description_alt = gsub('Infective arthritis and osteomyelitis except that caused by tuberculosis or sexually transmitted disease', 'Infective arthritis and osteomyelitis',  dat.results.total$ccs_level_3_description_alt)
dat.results.total$ccs_level_3_description_alt = gsub('Encephalitis except that caused by tuberculosis or sexually transmitted disease', 'Encephalitis',  dat.results.total$ccs_level_3_description_alt)
dat.results.total$ccs_level_3_description_alt = gsub('Meningitis except that caused by tuberculosis or sexually transmitted disease', 'Meningitis',  dat.results.total$ccs_level_3_description_alt)
dat.results.total$ccs_level_3_description_alt = gsub('Pneumonia except that caused by tuberculosis or sexually transmitted disease', 'Pneumonia',  dat.results.total$ccs_level_3_description_alt)

# get rid of \n as it screws up plot
dat.results.total$ccs_level_3_description_alt <- gsub('\n', ' ', dat.results.total$ccs_level_3_description_alt)

# reorder by CCS level 1, top and then alphabet to get order for plotting
# add running number which is row number
# refactor ccs level 3 by running number
dat.results.total = dat.results.total[with(dat.results.total, order(ccs_level_1_description, -top, -rr)), ]
dat.results.total$order = 1:nrow(dat.results.total)
dat.results.total$ccs_level_3_description <- factor(dat.results.total$ccs_level_3_description, levels=dat.results.total$ccs_level_3_description[order(rev(dat.results.total$order))], ordered=TRUE)
dat.results.total$ccs_level_3_description_alt <- factor(dat.results.total$ccs_level_3_description_alt, levels=dat.results.total$ccs_level_3_description_alt[order(rev(dat.results.total$order))], ordered=TRUE)

# average RR and panel each broad level 1 cause (actualy Figure we use is currently this with 50,000 limit)
for(include_limit in include_limits[1:5]){

    print(include_limit)

    pdf(paste0(dir.output,'forest_plot_average_lag_all_together_panelled_include_limit_',include_limit,'_',start_year,'_',end_year,'.pdf'),paper='a4',width=0,height=0)

    print(
    ggplot() +
    geom_point(data=subset(dat.results.total,cases>include_limit),aes(x=ccs_level_3_description_alt,y=(err.mean),color=ccs_level_1_description),size=2,shape=16) +
    geom_errorbar(data=subset(dat.results.total,cases>include_limit),aes(x=ccs_level_3_description_alt,ymax=(err.mean.ul),ymin=(err.mean.ll),color=ccs_level_1_description),width=0,size=0.5) +
    geom_point(data=subset(dat.results.total,cases>include_limit&top==1),aes(x=ccs_level_3_description_alt,y=(err.mean)),color='black',size=3,shape=16) +
    geom_point(data=subset(dat.results.total,cases>include_limit),aes(x=ccs_level_3_description_alt,y=(err.mean),color=ccs_level_1_description),size=2,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Cause of hospitalization') + ylab('Percentage change in hospitalization rates\nassociated with tropical cyclone exposure') +
    # ylim(ylims) +
    scale_y_continuous(labels=scales::percent) +
    scale_colour_manual(values=colors.ccs.level.1) +
    coord_flip(ylim=c(-0.2, 0.5)) +
    facet_grid(ccs_level_1_description~.,scales="free_y", space = "free_y") +
    theme_bw() + theme(text = element_text(size = 8.5),
    panel.grid.major = element_blank(),axis.text.y = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'none',legend.justification='center', strip.text.y = element_blank(),
    legend.background = element_rect(fill="white", size=.5, linetype="dotted")))

dev.off()

    write.csv(subset(dat.results.total,cases>include_limit), paste0(dir.output,'ccs_included_',include_limit,'_',start_year,'_',end_year,'.csv'))

}

# average RR for single chosen level 1 cause separately
# plot only one cause in class forest plot style
causes = c('Cardiovascular diseases', 'Respiratory diseases','Cancers','Injuries',
          'Neuropsychiatric disorders','Blood diseases','Digestive system diseases',
          'Endocrine disorders','Genitourinary diseases','Infectious and parasitic diseases',
          'Musculoskeletal and\nconnective tissue diseases','Nervous system diseases',
          'Skin and subcutaneous\ntissue diseases')

for(cause in causes){
for(include_limit in include_limits[1:5]){
    print(include_limit)

    pdf(paste0(dir.output,'forest_plot_average_lag_',cause,'_panelled_include_limit_',include_limit,'_',start_year,'_',end_year,'.pdf'),paper='a4r',width=0,height=0)

    print(
    ggplot() +
    geom_point(data=subset(dat.results.total,cases>include_limit&ccs_level_1_description==cause),aes(x=ccs_level_3_description_alt,y=(err.mean)),color='black',size=2,shape=16) +
    geom_errorbar(data=subset(dat.results.total,cases>include_limit&ccs_level_1_description==cause),aes(x=ccs_level_3_description_alt,ymax=(err.mean.ul),ymin=(err.mean.ll)),color='black',width=0,size=0.5) +
    geom_point(data=subset(dat.results.total,cases>include_limit&top==1&ccs_level_1_description==cause),aes(x=ccs_level_3_description_alt,y=(err.mean)),color='black',size=3,shape=16) +
    geom_point(data=subset(dat.results.total,cases>include_limit&ccs_level_1_description==cause),aes(x=ccs_level_3_description_alt,y=(err.mean)),color='black',size=2,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Cause of hospitalization') + ylab('Percentage change in hospitalization rates\nassociated with tropical cyclone exposure') +
    ggtitle(cause) +
    scale_y_continuous(labels=scales::percent) +
    scale_colour_manual(values=colors.ccs.level.1) +
    coord_flip(ylim=c(-0.2, 0.5)) +
    facet_grid(ccs_level_1_description~.,scales="free_y", space = "free_y") +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.y = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'none',legend.justification='center', strip.text.y = element_blank(),
    legend.background = element_rect(fill="white", size=.5, linetype="dotted")))

dev.off()
}}

# save CSV for quoting from easily
write.csv(dat.results.total,paste0(dir.output,'medicare_all_ccs_level_3_all_ccs_level_1_integrated_lag_model_summary_bonferroni_corrected_',years[1],'_',years[length(years)],'.csv'))

# unique ccs level 1
#ccs_level_1_descriptions = unique(dat.results.total$ccs_level_1_description)

# OLD UNUSED CODE

#print(ccs_level_1_descriptions)
#
#for(ccs_level_1 in ccs_level_1_descriptions[1]){
#    for(include_limit in include_limits){
#
#    print(ccs_level_1)
#
#        pdf(paste0(dir.output,'forest_plot_integrated_lag_',ccs_level_1,'_include_limit_',include_limit,'_',start_year,'_',end_year,'.pdf'),paper='a4r',width=0,height=0)
#
#        print(
#        ggplot() +
#        geom_errorbar(data=subset(dat.results.total,top==0&ccs_level_1_description==ccs_level_1&cases>include_limit),aes(x=ccs_level_3_description,ymax=(rr.ul-1),ymin=(rr.ll-1),color=ccs_level_1_description),width=0,size=0.5) +
#        geom_point(data=subset(dat.results.total,top==0&ccs_level_1_description==ccs_level_1&cases>include_limit),aes(x=ccs_level_3_description,y=(rr-1),color=ccs_level_1_description),size=3,shape=16) +
#        geom_errorbar(data=subset(dat.results.total,top==1&ccs_level_1_description==ccs_level_1&cases>include_limit),aes(x=ccs_level_3_description,ymax=(rr.ul-1),ymin=(rr.ll-1)),width=0,size=0.5) +
#        geom_point(data=subset(dat.results.total,top==1&ccs_level_1_description==ccs_level_1&cases>include_limit),aes(x=ccs_level_3_description,y=(rr-1)),size=5,shape=16) +
#        geom_hline(yintercept=0,linetype='dotted') +
#        xlab('Lag (days after event)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
#        # ylim(ylims) +
#        scale_y_continuous(labels=scales::percent) +
#        ggtitle(paste0(ccs_level_1,': include limit=',include_limit)) +
#        coord_flip() +
#        theme_bw() + theme(text = element_text(size = 10),
#        panel.grid.major = element_blank(),axis.text.y = element_text(angle=0),
#        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#        legend.position = 'bottom',legend.justification='center',
#        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
#
#        dev.off()
#
#}}

## cumulative RR
#for(include_limit in include_limits[2:5]){
#
#    print(include_limit)
#
#    pdf(paste0(dir.output,'forest_plot_integrated_lag_all_together_include_limit_',include_limit,'_',start_year,'_',end_year,'.pdf'),paper='a4',width=0,height=0)
#
#    print(
#    ggplot() +
#    geom_point(data=subset(dat.results.total,cases>include_limit),aes(x=ccs_level_3_description,y=(rr-1),color=ccs_level_1_description),size=3,shape=16) +
#    geom_errorbar(data=subset(dat.results.total,cases>include_limit),aes(x=ccs_level_3_description,ymax=(rr.ul-1),ymin=(rr.ll-1),color=ccs_level_1_description),width=0,size=0.5) +
#    geom_point(data=subset(dat.results.total,cases>include_limit&top==1),aes(x=ccs_level_3_description,y=(rr-1)),color='black',size=2,shape=16) +
#
#    geom_hline(yintercept=0,linetype='dotted') +
#    xlab('Cause of hospitalization') + ylab('Percentage change in hospitalization rates\nassociated with tropical cyclone exposure') +
#    # ylim(ylims) +
#    scale_y_continuous(labels=scales::percent) +
#    # ggtitle(paste0('Include limit=',include_limit)) +
#    coord_flip() +
#    # facet_wrap(~ccs_level_1_description) +
#    theme_bw() + theme(text = element_text(size = 10),
#    panel.grid.major = element_blank(),axis.text.y = element_text(angle=0),
#    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#    legend.position = 'none',legend.justification='center',
#    legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
#
#dev.off()
#
#}
#
## average RR
#for(include_limit in include_limits[2:5]){
#
#    print(include_limit)
#
#    pdf(paste0(dir.output,'forest_plot_average_lag_all_together_include_limit_',include_limit,'_',start_year,'_',end_year,'.pdf'),paper='a4',width=0,height=0)
#
#    print(
#    ggplot() +
#    geom_point(data=subset(dat.results.total,cases>include_limit),aes(x=ccs_level_3_description,y=(err.mean),color=ccs_level_1_description),size=2,shape=16) +
#    geom_errorbar(data=subset(dat.results.total,cases>include_limit),aes(x=ccs_level_3_description,ymax=(err.mean.ul),ymin=(err.mean.ll),color=ccs_level_1_description),width=0,size=0.5) +
#    geom_point(data=subset(dat.results.total,cases>include_limit&top==1),aes(x=ccs_level_3_description,y=(err.mean)),color='black',size=3,shape=16) +
#    geom_point(data=subset(dat.results.total,cases>include_limit),aes(x=ccs_level_3_description,y=(err.mean),color=ccs_level_1_description),size=2,shape=16) +
#    geom_hline(yintercept=0,linetype='dotted') +
#    xlab('Cause of hospitalization') + ylab('Percentage change in hospitalization rates\nassociated with tropical cyclone exposure') +
#    # ylim(ylims) +
#    scale_y_continuous(labels=scales::percent) +
#    scale_colour_manual(values=colors.ccs.level.1) +
#    coord_flip(ylim=c(-0.2, 0.5)) +
#    # facet_wrap(~ccs_level_1_description) +
#    theme_bw() + theme(text = element_text(size = 10),
#    panel.grid.major = element_blank(),axis.text.y = element_text(angle=0),
#    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#    legend.position = 'none',legend.justification='center',
#    legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
#
#dev.off()
#
#}
