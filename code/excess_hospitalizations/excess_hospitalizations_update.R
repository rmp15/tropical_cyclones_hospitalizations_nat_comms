# this code will take all model results
# then explore with figure output

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

# export combined causes of hospitalisation results for separate lags
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dat.results = read.csv(paste0(dir.output.model.summary,'medicare_all_ccs_level_1_model_summary_bonferroni_corrected_',years[1],'_',years[length(years)],'.csv'))

# create lag factor values for ggplot
dat.results$lag.factor = factor(dat.results$lag, levels=c(7:0))

# rename causes CCS level 1
rename_function = function(dat.admissions){

    dat.admissions$ccs_level_1_description = gsub('Neoplasms', 'Cancers',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Injury and poisoning', 'Injuries',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Mental illness', 'Neuropsychiatric\ndisorders',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and\nconnective tissue diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Diseases of the skin and subcutaneous tissue", 'Skin and subcutaneous\ntissue diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Diseases of the genitourinary system", 'Genitourinary\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Diseases of the digestive system", 'Digestive system\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Diseases of the respiratory system", 'Respiratory\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Diseases of the circulatory system", 'Cardiovascular\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Diseases of the nervous system and sense organs", 'Nervous system\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Diseases of the blood and blood-forming organs", 'Blood\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", 'Endocrine\ndisorders',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Infectious and parasitic diseases", 'Infectious and\nparasitic diseases',  dat.admissions$ccs_level_1_description)

    # reorder CCS level 1 causes for plotting
    dat.admissions$ccs_level_1_description = factor(dat.admissions$ccs_level_1_description,
                        levels=c('Cardiovascular\ndiseases','Respiratory\ndiseases','Cancers','Injuries','Neuropsychiatric\ndisorders',
                                    'Blood\ndiseases','Digestive system\ndiseases','Endocrine\ndisorders','Genitourinary\ndiseases',
                                    'Infectious and\nparasitic diseases','Musculoskeletal and\nconnective tissue diseases',
                                    'Nervous system\ndiseases','Skin and subcutaneous\ntissue diseases'))

    return(dat.admissions)
}

dat.results = rename_function(dat.results)

# get rid of pointless columns
dat.results$X = NULL ; dat.results$ccs_level_3_description = NULL

# also integrated lag
library(plyr)
dat.results.total = read.csv(paste0(dir.output.model.summary,'medicare_all_ccs_level_1_integrated_lag_model_summary_bonferroni_corrected_',years[1],'_',years[length(years)],'.csv'))
dat.results.total$X = NULL

dat.results.total$ccs_level_1_description = dat.results.total$cause ; dat.results.total$cause = NULL

dat.results.total = rename_function(dat.results.total)

# source variables for coloring etc.
#source('~/git/rmparks_coastal_storms_Jan_2020/data/objects/objects.R')

# if data already processed this is a necessary step
skip = 1

# this is initial preparation which needs to be skipped because it takes ages to process
if(skip==0){
# load hospitalizations for last year of study and in hurricane season (I have to cycle through lags to subset also!!!)
dir.input.local = paste0('~/data/morbidity/US/medicare/processed/')
dat.admissions = readRDS(paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_no_statewide_matched_wind_events_',years[1],'_',years[length(years)],'.rds'))
dat.admissions$month = as.numeric(dat.admissions$month)
dat.admissions = subset(dat.admissions, month%in%c(5:10))

# take average number of hospitalizations for all hurricane season (May to October?) for non-storm events
library(plyr)
#dat.admissions.ccs.level.1 = ddply(dat.admissions,.(fipscounty,day,month,year,ccs_level_1_description),summarize,cases=sum(cases))
dat.admissions.ccs.level.1 = ddply(dat.admissions,.(fipscounty,day,month,year,ccs_level_1),summarize,cases=sum(cases))

# set up some sort of it file exists string here to save time next time
saveRDS(dat.admissions.ccs.level.1,paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_no_statewide_matched_wind_events_summarised_',years[1],'_',years[length(years)],'.rds'))

# rename causes CCS level 1
rename_function_2 = function(dat.admissions){

    dat.admissions$ccs_level_1_description = gsub('Cancers', 'Cancers',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Injuries', 'Injuries',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Neuropsychiatric disorders', 'Neuropsychiatric\ndisorders',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Musculoskeletal and\nconnective tissue diseases', 'Musculoskeletal and\nconnective tissue diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Skin and subcutaneous\ntissue diseases", 'Skin and subcutaneous\ntissue diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Genitourinary diseases", 'Genitourinary\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Digestive system diseases", 'Digestive system\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Respiratory diseases", 'Respiratory\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Cardiovascular diseases", 'Cardiovascular\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Nervous system diseases", 'Nervous system\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Blood diseases", 'Blood\ndiseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Endocrine disorders", 'Endocrine\ndisorders',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Infectious and parasitic diseases", 'Infectious and\nparasitic diseases',  dat.admissions$ccs_level_1_description)

    # reorder CCS level 1 causes for plotting
    dat.admissions$ccs_level_1_description = factor(dat.admissions$ccs_level_1_description,
                        levels=c('Cardiovascular\ndiseases','Respiratory\ndiseases','Cancers','Injuries','Neuropsychiatric\ndisorders',
                                    'Blood\ndiseases','Digestive system\ndiseases','Endocrine\ndisorders','Genitourinary\ndiseases',
                                    'Infectious and\nparasitic diseases','Musculoskeletal and\nconnective tissue diseases',
                                    'Nervous system\ndiseases','Skin and subcutaneous\ntissue diseases','Other'))

    return(dat.admissions)
}

dat.admissions.ccs.level.1 = rename_function_2(dat.admissions.ccs.level.1)

# set up some sort of it file exists string here to save time next time
saveRDS(dat.admissions.ccs.level.1,paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_no_statewide_matched_wind_events_summarised_new_names_',years[1],'_',years[length(years)],'.rds'))

# fully expand dates, fips then match to loaded admissions data
dates = seq(as.Date("1999-01-01"), as.Date("2014-12-31"), by="days")
fipscounty = sort(unique(dat.admissions.ccs.level.1$fipscounty))
ccs_level_1_description_list = sort(unique(dat.admissions.ccs.level.1$ccs_level_1_description))
complete.grid = expand.grid(fipscounty=fipscounty,date=dates,ccs_level_1_description=ccs_level_1_description_list)
complete.grid$year = format(complete.grid$date, '%Y') ; complete.grid$year = as.numeric(complete.grid$year)
complete.grid$month = as.numeric(format(complete.grid$date, '%m'))
complete.grid$day = as.numeric(format(complete.grid$date, '%d'))
complete.grid$date = NULL

# rename causes CCS level 1
# complete.grid$ccs_level_1_description = gsub('\n', ' ', complete.grid$ccs_level_1_description)
# dat.admissions.ccs.level.1$ccs_level_1_description = gsub('\n', ' ', dat.admissions.ccs.level.1$ccs_level_1_description)

dat.complete = merge(complete.grid,dat.admissions.ccs.level.1,by=c('fipscounty','year','month','day','ccs_level_1_description'),all.x='TRUE')

# set up some sort of it file exists string here to save time next time
saveRDS(dat.complete,paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_no_statewide_matched_wind_events_summarised_new_names_complete_grid_',years[1],'_',years[length(years)],'.rds'))

}

dat.complete = readRDS(paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_no_statewide_matched_wind_events_summarised_new_names_complete_grid_',years[1],'_',years[length(years)],'.rds'))

# assign missing cases to have value 0
dat.complete$cases = ifelse(is.na(dat.complete$cases)==TRUE,0,dat.complete$cases)
dat.complete.subset = subset(dat.complete, month%in%c(5:10))

# find average number of hospitalizations for the entire hurricane (May - October) season (could do loop but lazy)
dat.admissons.css.level.1.mean = ddply(dat.complete.subset,.(fipscounty,ccs_level_1_description),summarize,cases.mean=mean(cases))
dat.admissons.css.level.1.mean.7 = dat.admissons.css.level.1.mean.6 = dat.admissons.css.level.1.mean.5 = dat.admissons.css.level.1.mean
dat.admissons.css.level.1.mean.5 = dat.admissons.css.level.1.mean.4 = dat.admissons.css.level.1.mean.3 = dat.admissons.css.level.1.mean
dat.admissons.css.level.1.mean.2 = dat.admissons.css.level.1.mean.1 = dat.admissons.css.level.1.mean.0 = dat.admissons.css.level.1.mean

# add lags to each hospitalization record
dat.admissons.css.level.1.mean.0$lag = 0 ; dat.admissons.css.level.1.mean.1$lag = 1 ; dat.admissons.css.level.1.mean.2$lag = 2
dat.admissons.css.level.1.mean.3$lag = 3 ; dat.admissons.css.level.1.mean.4$lag = 4 ; dat.admissons.css.level.1.mean.5$lag = 5
dat.admissons.css.level.1.mean.6$lag = 6 ; dat.admissons.css.level.1.mean.7$lag = 7

dat.admissions.ccs.level.1.mean.total = rbind(dat.admissons.css.level.1.mean.0,dat.admissons.css.level.1.mean.1,dat.admissons.css.level.1.mean.2,
                                        dat.admissons.css.level.1.mean.3,dat.admissons.css.level.1.mean.4,dat.admissons.css.level.1.mean.5,
                                        dat.admissons.css.level.1.mean.6,dat.admissons.css.level.1.mean.7)

dat.admissions.ccs.level.1.mean.total = merge(dat.admissions.ccs.level.1.mean.total, dat.results, by=c('lag','ccs_level_1_description'), all.X=TRUE)

# load and attach the mean number of tropical cyclones per decade
dir.input.cyclones = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_wind_data/',start_year,'_2016/')
dat.event.sum = read.csv(paste0(dir.input.cyclones,'events_by_county_',start_year,'_2016.csv'))
dat.event.sum$X = NULL ; dat.event.sum$cases = dat.event.sum$cases.per.year = NULL

# merge average number with the main admissions file
dat.admissions.ccs.level.1.mean.total = merge(dat.admissions.ccs.level.1.mean.total,dat.event.sum, by.x=c('fipscounty'), by.y=('fips'), all.X=TRUE)

dat.admissions.ccs.level.1.mean.total$cases.additional = with(dat.admissions.ccs.level.1.mean.total, cases.mean * ((rr)^cases.per.decade-1))

# rename and refactor ccs level 1

# summaries for graphs
dat.admissions.ccs.level.1.mean.total.cause.lag.summary = ddply(dat.admissions.ccs.level.1.mean.total,.(ccs_level_1_description,lag),summarize,cases.additional=sum(cases.additional))
dat.admissions.ccs.level.1.mean.total.lag.summary = ddply(dat.admissions.ccs.level.1.mean.total,.(lag),summarize,cases.additional=sum(cases.additional))
dat.admissions.ccs.level.1.mean.total.cause.summary = ddply(dat.admissions.ccs.level.1.mean.total,.(ccs_level_1_description),summarize,cases.additional=sum(cases.additional))

# establish excess hospitalizations by lag and by ccs category
dat.excess = ddply(dat.admissions.ccs.level.1.mean.total,.(ccs_level_1_description,lag),summarize,cases.additional=sum(cases.additional))

# integrated lags values too
dat.admissons.css.level.1.mean.int.lag = dat.admissons.css.level.1.mean
dat.admissons.css.level.1.mean.int.lag$cases.mean = dat.admissons.css.level.1.mean.int.lag$cases.mean * 8 # because 8 is currently the number of lag days (0 to 7)

dat.admissons.css.level.1.mean.int.lag = merge(dat.admissons.css.level.1.mean.int.lag, dat.results.total, by=c('ccs_level_1_description'), all.X=TRUE)

# merge average number of cyclones with the main admissions file
dat.admissons.css.level.1.mean.int.lag = merge(dat.admissons.css.level.1.mean.int.lag,dat.event.sum, by.x=c('fipscounty'), by.y=('fips'), all.X=TRUE)

dat.admissons.css.level.1.mean.int.lag$cases.additional = with(dat.admissons.css.level.1.mean.int.lag, cases.mean * ((rr)^(cases.per.decade/10)-1))
dat.admissons.css.level.1.mean.int.lag$cases.additional.ll = with(dat.admissons.css.level.1.mean.int.lag, cases.mean * ((rr.ll)^(cases.per.decade/10)-1))
dat.admissons.css.level.1.mean.int.lag$cases.additional.ul = with(dat.admissons.css.level.1.mean.int.lag, cases.mean * ((rr.ul)^(cases.per.decade/10)-1))

dat.admissions.ccs.level.1.mean.total.cause.summary.int.lag = ddply(dat.admissons.css.level.1.mean.int.lag,.(ccs_level_1_description),summarize,
                                                            cases.additional=10*sum(cases.additional),cases.additional.ll=10*sum(cases.additional.ll),
                                                            cases.additional.ul=10*sum(cases.additional.ul))

# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/excess_hospitalizations_update/',start_year,'_',end_year,'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# save summaries as csvs
# export combined causes of hospitalisation results
write.csv(dat.admissions.ccs.level.1.mean.total.lag.summary,paste0(dir.output,'medicare_all_ccs_level_1_model_summary_excess_hospitalizations_by_lag_',years[1],'_',years[length(years)],'.csv'))
write.csv(dat.admissions.ccs.level.1.mean.total.cause.lag.summary,paste0(dir.output,'medicare_all_ccs_level_1_model_summary_excess_hospitalizations_by_lag_and_cause',years[1],'_',years[length(years)],'.csv'))
write.csv(dat.admissions.ccs.level.1.mean.total.cause.summary.int.lag,paste0(dir.output,'medicare_all_ccs_level_1_model_summary_excess_hospitalizations_int_lag_',years[1],'_',years[length(years)],'.csv'))

# create lag factor values for ggplot
dat.admissions.ccs.level.1.mean.total.cause.lag.summary$lag.factor = factor(dat.admissions.ccs.level.1.mean.total.cause.lag.summary$lag, levels=c(0:7))

library(ggplot2)
library(gridExtra)
library(scales)

# old layered by lag across causes
p3 = ggplot() +
  geom_bar(data=subset(dat.admissions.ccs.level.1.mean.total.cause.lag.summary), aes(x=as.factor(ccs_level_1_description),y=cases.additional,fill=lag.factor), stat='identity') +
    # geom_point(data=subset(dat.admissions.ccs.level.1.mean.total.cause.summary),aes(x=as.factor(ccs_level_1_description),y=cases.additional),shape=16,color='black') +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Cause') +  ylab('') +
    # ylim(c(min.plot,max.plot)) +
    # facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.lags) +
    guides(fill=guide_legend(title="Lag (days after exposure)", nrow=1)) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(fill=guide_legend(title="Subcategory of intentional injury")) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 10),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=20, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# bar across causes
p3.new = ggplot() +
  geom_bar(data=subset(dat.admissions.ccs.level.1.mean.total.cause.summary.int.lag), aes(x=as.factor(ccs_level_1_description),y=cases.additional,fill=ccs_level_1_description), stat='identity', inherit.aes = FALSE ) +
    # geom_point(data=subset(dat.admissions.ccs.level.1.mean.total.cause.summary.int.lag),aes(x=as.factor(ccs_level_1_description),y=cases.additional),shape=16,color='black') +
    geom_errorbar(data=subset(dat.admissions.ccs.level.1.mean.total.cause.summary.int.lag),aes(x=as.factor(ccs_level_1_description),ymin=cases.additional.ll,ymax=cases.additional.ul),width=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Cause') +  ylab('') +
    # ylim(c(min.plot,max.plot)) +
    # facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.ccs.level.1) +
    guides(fill=guide_legend(title="Lag (days after exposure)", nrow=1)) +
   scale_y_continuous(labels = comma) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(fill=guide_legend(title="Subcategory of intentional injury")) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 10),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=20, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'none',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# layered by cause across lags
p4 = ggplot() +
    geom_bar(data=subset(dat.admissions.ccs.level.1.mean.total.cause.lag.summary), aes(x=as.factor(lag),y=cases.additional,fill=ccs_level_1_description), stat='identity') +
    # geom_point(data=subset(dat.admissions.ccs.level.1.mean.total.lag.summary),aes(x=as.factor(lag),y=cases.additional),shape=16,color='black') +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Lag (days after event)') + ylab('') +
    # ylim(c(min.plot,max.plot)) +
    # facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.ccs.level.1) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="", nrow=2)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 10), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# plot all together in class forest plot style
pdf(paste0(dir.output,'excess_hospitalizations_ccs_level_1_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)

grid.arrange(p3.new,p4,nrow=2,left=paste("Additional hospitalization associated with\n average decadal tropical cyclone exposure"))

dev.off()


