# Figure 3. Percentage change in hospitalization rates with tropical cyclone exposure
# by cause of hospitalization and lag time. Lag time is measured in days after tropical cyclone exposure.
# Dots show the point estimates and error bars represent Bonferroni-corrected 95% confidence intervals.

rm(list=ls())

# declare directories
project.folder <- paste0(print(here::here()),'/')
data.folder <- paste0(project.folder,'data/')
packages.folder <- paste0(project.folder,'packages/')
output.folder <- paste0(project.folder,'output/')

# load necessary packages
source(paste0(packages.folder,'packages_to_load.R'))

# years of study
start_year <- 1999; end_year <- 2014

# load results
dat.results <- read.csv(paste0(data.folder,'figure_3_model_results_',start_year,'_',end_year,'.csv'))

# colors for CCS Level 1 causes of death
source(paste0(project.folder,'/colors/colors.R'))

# reorder CCS level 1 causes for plotting
dat.results$ccs_level_1_description = factor(dat.results$ccs_level_1_description,
                        levels=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
                                    'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
                                    'Infectious and parasitic diseases','Musculoskeletal and connective tissue diseases',
                                    'Nervous system diseases','Skin and subcutaneous tissue diseases'))

# ensure lags go correct order in plot
dat.results$lag.factor = factor(dat.results$lag.factor, levels=c(7:0))

# save plot output for Figure 3
pdf(paste0(output.folder,'figure_3.pdf'),paper='a4r',width=0,height=0)
ggplot() +
    geom_errorbar(data=subset(dat.results),aes(x=as.factor(lag.factor),ymax=rr.ll.bfc-1,ymin=rr.ul.bfc-1),width=.2,size=0.5) +
    geom_point(data=subset(dat.results), aes(x=as.factor(lag.factor),y=rr-1),size=3,shape=16) +
    geom_point(data=subset(dat.results), aes(x=as.factor(lag.factor),y=rr-1,color=ccs_level_1_description),size=2,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Lag (days after exposure)') + ylab('Percentage change in hospitalization rates associated with tropical cyclone exposure') +
    facet_wrap(vars(ccs_level_1_description),ncol=2) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_color_manual(values=colors.ccs.level.1) +
    guides(color=FALSE) +
    coord_flip() +
    theme_bw() + theme(text = element_text(size = 12),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=6),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()