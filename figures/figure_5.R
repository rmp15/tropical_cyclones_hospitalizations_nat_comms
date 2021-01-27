# Figure 5. Percentage change in hospitalization rates with tropical cyclone exposure by cause of hospitalization,
# intensity of local wind exposure and lag time. Lag time is measured in days after tropical cyclone exposure.
# Dots show the point estimates and error bars represent Bonferroni-corrected 95% confidence intervals.

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

print(args)

# years of study
start_year = as.numeric(args[1]) # 1999
end_year = as.numeric(args[2]) # 2014
lag_chosen = as.numeric(args[3]) # 1

years=c(start_year:end_year)

# load CCS level 1 and 3 names
code.lookup.merged = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/CCS_lookup_2015.csv')
code.lookup.merged$X = NULL
code.lookup.merged = unique(code.lookup.merged[,c('ccs_level_3','ccs_level_3_description','ccs_level_1','ccs_level_1_description')])
code.lookup.merged$ccs_level_3_description = as.character(code.lookup.merged$ccs_level_3_description)
code.lookup.merged$ccs_level_1_description = as.character(code.lookup.merged$ccs_level_1_description)
ccs_level_3 = unique(as.character(code.lookup.merged$ccs_level_3_description))
ccs_level_1 = unique(as.character(code.lookup.merged$ccs_level_1_description))

# load model summaries for CCS level 1
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dat.results = data.frame()
for(cod in ccs_level_1){
    file.current = paste0(dir.output.model.summary,'medicare_',cod,'_model_summary_bonferroni_corrected_hurricane_separate_update_',years[1],'_',years[length(years)],'.csv')
    if (file.exists(file.current)){
    print(cod)
    dat.results.current = read.csv(file.current)
    dat.results.current$ccs_level_3_description = dat.results.current$ccs_level_1_description = dat.results.current$cause
    dat.results.current$X = dat.results.current$cause = NULL
    dat.results = rbind(dat.results.current, dat.results)}
}

# exclude anything which is going in 'other'
dat.results = subset(dat.results,!(ccs_level_3_description%in%c('Congenital anomalies')))

# export combined causes of hospitalisation results
write.csv(dat.results,paste0(dir.output.model.summary,'medicare_all_ccs_level_1_model_summary_bonferroni_corrected_hurricane_separate_',years[1],'_',years[length(years)],'.csv'))

# create lag factor values for ggplot
dat.results$lag.factor = factor(dat.results$lag, levels=c(7:0))

# rename causes CCS level 1
dat.results$ccs_level_1_description = gsub('Diseases of the circulatory system', 'Cardiovascular diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the respiratory system', 'Respiratory diseases', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Neoplasms', 'Cancers', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Injury and poisoning', 'Injuries', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Mental illness', 'Neuropsychiatric disorders', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Diseases of the blood and blood-forming organs', 'Blood diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the digestive system', 'Digestive system diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub("Endocrine; nutritional; and metabolic diseases and immunity disorders", 'Endocrine disorders', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the genitourinary system', 'Genitourinary diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Infectious and parasitic diseases', 'Infectious and parasitic diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective tissue diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the nervous system and sense organs', 'Nervous system diseases', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Diseases of the skin and subcutaneous tissue', 'Skin and subcutaneous tissue diseases', dat.results$ccs_level_1_description)

dat.results$ccs_level_1_description = gsub('Congenital anomalies', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Residual codes unclassified all E codes 259 and 260', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Certain conditions originating in the perinatal period', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Complications of pregnancy childbirth and the puerperium', 'Other', dat.results$ccs_level_1_description)
dat.results$ccs_level_1_description = gsub('Symptoms signs and ill-defined conditions and factors influencing health status', 'Other', dat.results$ccs_level_1_description)

# reorder CCS level 1 causes for plotting
dat.results$ccs_level_1_description = factor(dat.results$ccs_level_1_description,
                        levels=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
                                    'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
                                    'Infectious and parasitic diseases','Musculoskeletal and connective tissue diseases',
                                    'Nervous system diseases','Skin and subcutaneous tissue diseases'))

# rename storm categories
dat.results$event = gsub('Coastal storm', 'Gale to violent storm', dat.results$event)
dat.results$event = gsub('Hurricane', 'Hurricane', dat.results$event)


# source variables for colorinng etc.
source('~/git/rmparks_coastal_storms_Jan_2020/data/objects/objects.R')

# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_unconstrained_dlm_model_data/',start_year,'_',end_year,'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

library(ggplot2)

# ONLY UNCORRECTED

# # plot all together in class forest plot style
# pdf(paste0(dir.output,'forest_plot_all_ccs_level_1_hurricane_separate_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(dat.results),aes(x=lag.factor,ymax=rr.ll-1,ymin=rr.ul-1),width=.2,size=0.5) +
#     geom_point(data=subset(dat.results,event=='Coastal storm'), aes(x=lag.factor,y=rr-1),size=3,shape=16) +
#     geom_point(data=subset(dat.results,event=='Coastal storm'), aes(x=lag.factor,y=rr-1,color=ccs_level_1_description),size=2,shape=16) +
#     geom_point(data=subset(dat.results,event=='Hurricane'), aes(x=lag.factor,y=rr-1),size=3,shape=17) +
#     geom_point(data=subset(dat.results,event=='Hurricane'), aes(x=lag.factor,y=rr-1,color=ccs_level_1_description),size=2,shape=17) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
#     xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
#     facet_wrap(vars(ccs_level_1_description),ncol=2) +
#     scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
#     scale_color_manual(values=colors.ccs.level.1) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     # guides(color=guide_legend(title="",nrow=1)) +
#     guides(color=FALSE) +
#     coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 12),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()

# # plot all together in class forest plot style only lag 2 and above
# pdf(paste0(dir.output,'forest_plot_all_ccs_level_1_lag2_above_only_hurricane_separate_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(dat.results, lag%in%c(2:7)),aes(x=lag.factor,ymax=rr.ll-1,ymin=rr.ul-1),width=.2,size=0.5) +
#     geom_point(data=subset(dat.results, lag%in%c(2:7)), aes(x=lag.factor,y=rr-1),size=3,shape=16) +
#     geom_point(data=subset(dat.results, lag%in%c(2:7)), aes(x=lag.factor,y=rr-1,color=ccs_level_1_description),size=2,shape=16) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
#     xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
#     facet_wrap(vars(ccs_level_1_description),ncol=2) +
#     scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
#     scale_color_manual(values=colors.ccs.level.1) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     # guides(color=guide_legend(title="",nrow=1)) +
#     guides(color=FALSE) +
#     coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 12),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()
#
# # plot all together in class forest plot style
# pdf(paste0(dir.output,'forest_plot_alt_all_ccs_level_1_hurricane_separate_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(dat.results),aes(x=as.factor(lag),ymax=rr.ll-1,ymin=rr.ul-1),width=.2,size=0.5) +
#     geom_point(data=subset(dat.results), aes(x=as.factor(lag),y=rr-1),size=3,shape=16) +
#     geom_point(data=subset(dat.results), aes(x=as.factor(lag),y=rr-1,color=ccs_level_1_description),size=2,shape=16) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
#     xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
#     # facet_wrap(vars(ccs_level_1_description),ncol=2) +
#     scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
#     scale_color_manual(values=colors.ccs.level.1) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     # guides(color=guide_legend(title="",nrow=1)) +
#     guides(color=FALSE) +
#     # coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 12),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()

# ONLY CORRECTED

# # plot all together in class forest plot style
# pdf(paste0(dir.output,'forest_plot_all_ccs_level_1_bonferroni_corrected_hurricane_separate_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(dat.results),aes(x=lag.factor,ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1),width=.2,size=0.5,alpha=0.4) +
#     geom_point(data=subset(dat.results,event=='Coastal storm'), aes(x=lag.factor,y=rr-1),size=3,shape=16,alpha=0.4) +
#     geom_point(data=subset(dat.results,event=='Coastal storm'), aes(x=lag.factor,y=rr-1,color=ccs_level_1_description),size=2,shape=16,alpha=0.4) +
#     geom_point(data=subset(dat.results,event=='Hurricane'), aes(x=lag.factor,y=rr-1),size=3,shape=17,alpha=0.4) +
#     geom_point(data=subset(dat.results,event=='Hurricane'), aes(x=lag.factor,y=rr-1,color=ccs_level_1_description),size=2,shape=17,alpha=0.4) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
#     xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
#     facet_wrap(vars(ccs_level_1_description),ncol=2) +
#     scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
#     scale_color_manual(values=colors.ccs.level.1) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     # guides(color=guide_legend(title="",nrow=1)) +
#     guides(color=FALSE) +
#     coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 12),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()

# plot all together in class forest plot style
pdf(paste0(dir.output,'forest_plot_all_ccs_level_1_bonferroni_corrected_hurricane_separate_',start_year,'_',end_year,'_2.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_errorbar(data=subset(dat.results),aes(x=lag.factor,ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1,color=as.factor(event)),width=0.5,size=0.5,alpha=1,position=position_dodge(width=0.7)) +
    geom_point(data=subset(dat.results), aes(x=lag.factor,y=rr-1,color=as.factor(event)),size=2.5,shape=16,position=position_dodge(width=0.7)) +
    geom_hline(yintercept=0,linetype='dotted') +
    # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
    xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
    facet_wrap(vars(ccs_level_1_description),ncol=2) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_color_manual(values=colors.storm.hurricane) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
     guides(color=guide_legend(title="",nrow=1)) +
    #guides(color=FALSE) +
    coord_flip() +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 12),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.justification='center',legend.box = "horizontal",legend.position=c(0.76, 0.07),
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# plot all together in class forest plot style
pdf(paste0(dir.output,'forest_plot_all_ccs_level_1_bonferroni_corrected_hurricane_separate_no_legend_',start_year,'_',end_year,'_2.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_errorbar(data=subset(dat.results),aes(x=lag.factor,ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1,color=as.factor(event)),width=0.5,size=0.5,alpha=1,position=position_dodge(width=0.7)) +
    geom_point(data=subset(dat.results), aes(x=lag.factor,y=rr-1,color=as.factor(event)),size=2.5,shape=16,position=position_dodge(width=0.7)) +
    geom_hline(yintercept=0,linetype='dotted') +
    # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
    xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
    facet_wrap(vars(ccs_level_1_description),ncol=2) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_color_manual(values=colors.storm.hurricane) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(color=guide_legend(title="",nrow=1)) +
    guides(color=FALSE) +
    coord_flip() +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 12),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# plot all together in class forest plot style only lag 2 and above
# pdf(paste0(dir.output,'forest_plot_all_ccs_level_1_lag2_above_only_bonferroni_corrected_hurricane_separate_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(dat.results, lag%in%c(2:7)),aes(x=lag.factor,ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1),width=.2,size=0.5) +
#     geom_point(data=subset(dat.results, lag%in%c(2:7)), aes(x=lag.factor,y=rr-1),size=3,shape=16) +
#     geom_point(data=subset(dat.results, lag%in%c(2:7)), aes(x=lag.factor,y=rr-1,color=ccs_level_1_description),size=2,shape=16) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
#     xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
#     facet_wrap(vars(ccs_level_1_description),ncol=2) +
#     scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
#     scale_color_manual(values=colors.ccs.level.1) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     # guides(color=guide_legend(title="",nrow=1)) +
#     guides(color=FALSE) +
#     coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 12),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()
#
# # plot all together in class forest plot style
# pdf(paste0(dir.output,'forest_plot_alt_all_ccs_level_1_bonferroni_corrected_hurricane_separate_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(dat.results),aes(x=as.factor(lag),ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1),width=.2,size=0.5) +
#     geom_point(data=subset(dat.results), aes(x=as.factor(lag),y=rr-1),size=3,shape=16) +
#     geom_point(data=subset(dat.results), aes(x=as.factor(lag),y=rr-1,color=ccs_level_1_description),size=2,shape=16) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
#     xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
#     # facet_wrap(vars(ccs_level_1_description),ncol=2) +
#     scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
#     scale_color_manual(values=colors.ccs.level.1) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     # guides(color=guide_legend(title="",nrow=1)) +
#     guides(color=FALSE) +
#     # coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 12),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()

# BOTH CORRECTED AND UNCORRECTED

# # plot all together in class forest plot style
# pdf(paste0(dir.output,'forest_plot_all_ccs_level_1_bonferroni_corrected_and_uncorrected_hurricane_separate_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(dat.results),aes(x=lag.factor,ymax=rr.ll-1,ymin=rr.ul-1),width=.2,size=0.5) +
#     geom_errorbar(data=subset(dat.results),aes(x=lag.factor,ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1),width=.2,size=0.5,alpha=0.5) +
#     geom_point(data=subset(dat.results), aes(x=lag.factor,y=rr-1),size=3,shape=16) +
#     geom_point(data=subset(dat.results), aes(x=lag.factor,y=rr-1,color=ccs_level_1_description),size=2,shape=16) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
#     xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
#     facet_wrap(vars(ccs_level_1_description),ncol=2) +
#     scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
#     scale_color_manual(values=colors.ccs.level.1) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     # guides(color=guide_legend(title="",nrow=1)) +
#     guides(color=FALSE) +
#     coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 12),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()
#
# # plot all together in class forest plot style only lag 2 and above
# pdf(paste0(dir.output,'forest_plot_all_ccs_level_1_lag2_above_only_bonferroni_corrected_and_uncorrected_hurricane_separate_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(dat.results, lag%in%c(2:7)),aes(x=lag.factor,ymax=rr.ll-1,ymin=rr.ul-1),width=.2,size=0.5) +
#     geom_errorbar(data=subset(dat.results, lag%in%c(2:7)),aes(x=lag.factor,ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1),width=.2,size=0.5,alpha=0.5) +
#     geom_point(data=subset(dat.results, lag%in%c(2:7)), aes(x=lag.factor,y=rr-1),size=3,shape=16) +
#     geom_point(data=subset(dat.results, lag%in%c(2:7)), aes(x=lag.factor,y=rr-1,color=ccs_level_1_description),size=2,shape=16) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
#     xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
#     facet_wrap(vars(ccs_level_1_description),ncol=2) +
#     scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
#     scale_color_manual(values=colors.ccs.level.1) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     # guides(color=guide_legend(title="",nrow=1)) +
#     guides(color=FALSE) +
#     coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 12),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()
#
# # plot all together in class forest plot style
# pdf(paste0(dir.output,'forest_plot_alt_all_ccs_level_1_bonferroni_corrected_and_uncorrected_hurricane_separate_',start_year,'_',end_year,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(dat.results),aes(x=as.factor(lag),ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1),width=.2,size=0.5) +
#     geom_point(data=subset(dat.results), aes(x=as.factor(lag),y=rr-1),size=3,shape=16) +
#     geom_point(data=subset(dat.results), aes(x=as.factor(lag),y=rr-1,color=ccs_level_1_description),size=2,shape=16) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
#     xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
#     # facet_wrap(vars(ccs_level_1_description),ncol=2) +
#     scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
#     scale_color_manual(values=colors.ccs.level.1) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     # guides(color=guide_legend(title="",nrow=1)) +
#     guides(color=FALSE) +
#     # coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 12),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()

