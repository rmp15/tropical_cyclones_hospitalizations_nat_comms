# this code will take all processed wind years and load them
# then explore with figure output

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

print(args)

# years of study
start_year = as.numeric(args[1]) # 1999
end_year = as.numeric(args[2]) # 2014
lag_chosen = as.numeric(args[3]) # 1

years=c(start_year:end_year)

# for er/non-er coding
labels=c('er', 'non_er')

# load CCS level 1 and 3 names
code.lookup.merged = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/CCS_lookup_2015.csv')
code.lookup.merged$X = NULL
code.lookup.merged = unique(code.lookup.merged[,c('ccs_level_3','ccs_level_3_description','ccs_level_1','ccs_level_1_description')])
code.lookup.merged$ccs_level_3_description = as.character(code.lookup.merged$ccs_level_3_description)
code.lookup.merged$ccs_level_1_description = as.character(code.lookup.merged$ccs_level_1_description)
ccs_level_3 = unique(as.character(code.lookup.merged$ccs_level_3_description))
ccs_level_1 = unique(as.character(code.lookup.merged$ccs_level_1_description))

# load model summaries for CCS level 1 er and non er
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dat.results = data.frame()
for(cod in ccs_level_1){
        for(label in labels){
            file.current = paste0(dir.output.model.summary,'medicare_',cod,'_',label,'_model_summary_update_',years[1],'_',years[length(years)],'.csv')
            if (file.exists(file.current)){
            print(cod)
            dat.results.current = read.csv(file.current)
            dat.results.current$ccs_level_3_description = dat.results.current$ccs_level_1_description = dat.results.current$cause
            dat.results.current$X = dat.results.current$cause = NULL
            dat.results.current$type = label
            dat.results = rbind(dat.results.current, dat.results)}
        }
}
# exclude anything which is going in 'other'
dat.results = subset(dat.results,!(ccs_level_3_description%in%c('Congenital anomalies')))

# export combined causes of hospitalisation results
write.csv(dat.results,paste0(dir.output.model.summary,'medicare_all_ccs_level_1_model_summary_bonferroni_corrected_er_non_er_',years[1],'_',years[length(years)],'.csv'))

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

# rename er categories
dat.results$type = gsub('non_er', 'Not emgcy', dat.results$type)
dat.results$type = gsub('er', 'emgcy', dat.results$type)
dat.results$type = gsub('emgcy', 'Emergency', dat.results$type)


# source variables for coloring etc.
source('~/git/rmparks_coastal_storms_Jan_2020/data/objects/objects.R')

# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_unconstrained_dlm_model_data/',start_year,'_',end_year,'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

library(ggplot2)

# ONLY CORRECTED

# plot all together in class forest plot style
pdf(paste0(dir.output,'forest_plot_all_ccs_level_1_bonferroni_corrected_er_non_er_',start_year,'_',end_year,'_2.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_errorbar(data=subset(dat.results),aes(x=lag.factor,ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1,color=as.factor(type)),width=0.5,size=0.5,alpha=1,position=position_dodge(width=0.7)) +
    geom_point(data=subset(dat.results), aes(x=lag.factor,y=rr-1,color=as.factor(type)),size=2.5,shape=16,position=position_dodge(width=0.7)) +
    geom_hline(yintercept=0,linetype='dotted') +
    # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
    xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
    facet_wrap(vars(ccs_level_1_description),ncol=2) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_color_manual(values=colors.er.noner) +
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
pdf(paste0(dir.output,'forest_plot_all_ccs_level_1_bonferroni_corrected_er_non_er_no_legend_',start_year,'_',end_year,'_2.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_errorbar(data=subset(dat.results),aes(x=lag.factor,ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1,color=as.factor(type)),width=0.5,size=0.5,alpha=1,position=position_dodge(width=0.7)) +
    geom_point(data=subset(dat.results), aes(x=lag.factor,y=rr-1,color=as.factor(type)),size=2.5,shape=16,position=position_dodge(width=0.7)) +
    geom_hline(yintercept=0,linetype='dotted') +
    # scale_x_discrete(limits = unique(rev(dat.results$lag))) +
    xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
    facet_wrap(vars(ccs_level_1_description),ncol=2) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_color_manual(values=colors.er.noner) +
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