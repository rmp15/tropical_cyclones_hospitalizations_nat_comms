# Figure 7. Change in hospitalizations for the US Medicare population,
# for an expected number of tropical cyclone exposures by county over a decade.
# The top row shows the break down by cause covering the day of tropical cyclone exposure to seven days afterwards,
# with black bars representing Bonferroni-corrected 95% confidence interval.
# The bottom row shows the break down by lag days after the exposure.

# NOTE: this script will not run, because there is no data as we not allowed to share the summary file
# However, we thought it might be useful to share the color scheme

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

# colors for CCS Level 1 causes of death
source(paste0(project.folder,'/colors/colors.R'))

# bar across causes
p1 = ggplot() +
  geom_bar(data=subset(dat.admissions.ccs.level.1.mean.total.cause.summary.int.lag), aes(x=as.factor(ccs_level_1_description),y=cases.additional,fill=ccs_level_1_description), stat='identity', inherit.aes = FALSE ) +
    geom_errorbar(data=subset(dat.admissions.ccs.level.1.mean.total.cause.summary.int.lag),aes(x=as.factor(ccs_level_1_description),ymin=cases.additional.ll,ymax=cases.additional.ul),width=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Cause') +  ylab('') +
    scale_fill_manual(values=colors.ccs.level.1) +
    guides(fill=guide_legend(title="Lag (days after exposure)", nrow=1)) +
   scale_y_continuous(labels = comma) +
    theme_bw() + theme(text = element_text(size = 10),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=20, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'none',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# layered by cause across lags
p2 = ggplot() +
    geom_bar(data=subset(dat.admissions.ccs.level.1.mean.total.cause.lag.summary), aes(x=as.factor(lag),y=cases.additional,fill=ccs_level_1_description), stat='identity') +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Lag (days after event)') + ylab('') +
    scale_fill_manual(values=colors.ccs.level.1) +
    guides(fill=guide_legend(title="", nrow=2)) +
    theme_bw() + theme(text = element_text(size = 10), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# save plot output for Figure 2 (won't work as there is no dat.admissions.ccs.level.1.mean.total.cause.summary.int.lag
# or dat.admissions.ccs.level.1.mean.total.cause.lag.summary!)
pdf(paste0(output.folder,'figure_6.pdf'),paper='a4r',width=0,height=0)
grid.arrange(p1,p2,nrow=2,left=paste("Additional hospitalization associated with\n average decadal tropical cyclone exposure"))
dev.off()


