# Figure 4. Average percentage change in hospitalization rates with tropical cyclone exposure
# by cause and sub-cause of hospitalization.
# Average percentage change in hospitalization rates is across studied lag period
# (0 to 7 days after tropical cyclone exposure).
# Dots show the point estimates and error bars represent 95% confidence intervals.

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
dat.results <- read.csv(paste0(data.folder,'figure_4_model_results_',start_year,'_',end_year,'.csv'))

# colors for CCS Level 1 causes of death
source(paste0(project.folder,'/colors/colors.R'))

# reorder CCS level 1 causes for plotting
dat.results$ccs_level_1_description = factor(dat.results$ccs_level_1_description,
                        levels=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
                                    'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
                                    'Infectious and parasitic diseases','Musculoskeletal and\nconnective tissue diseases',
                                    'Nervous system diseases','Skin and subcutaneous\ntissue diseases'))

dat.results$ccs_level_3_description <- factor(dat.results$ccs_level_3_description, levels=dat.results$ccs_level_3_description[order(rev(dat.results$order))], ordered=TRUE)

# save plot output for Figure 4
pdf(paste0(output.folder,'figure_4.pdf'),paper='a4',width=0,height=0)
ggplot() +
    geom_point(data=subset(dat.results),aes(x=ccs_level_3_description,y=(err.mean),color=ccs_level_1_description),size=2,shape=16) +
    geom_errorbar(data=subset(dat.results),aes(x=ccs_level_3_description,ymax=(err.mean.ul),ymin=(err.mean.ll),color=ccs_level_1_description),width=0,size=0.5) +
    geom_point(data=subset(dat.results,top==1),aes(x=ccs_level_3_description,y=(err.mean)),color='black',size=3,shape=16) +
    geom_point(data=subset(dat.results,),aes(x=ccs_level_3_description,y=(err.mean),color=ccs_level_1_description),size=2,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Cause of hospitalization') + ylab('Percentage change in hospitalization rates\nassociated with tropical cyclone exposure') +
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
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()