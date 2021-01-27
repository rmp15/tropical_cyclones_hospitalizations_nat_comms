# Figure 2. Annual Medicare hospitalizations by cause.
# Number of Medicare hospitalizations by year and cause of hospitalization for counties
# with at least one tropical cyclone exposure for 1999 – 2014.

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

# save plot output for Figure 2 (won't work as there is no dat.medicare!)
pdf(paste0(output.folder,'figure_2.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.medicare,aes(x=year,y=cases/1000000,fill=ccs_level_1_description)) +
    geom_col() +
    xlab('Year') + ylab('Hospitalizations (millions)') +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    scale_fill_manual(values=colors.ccs.level.1) +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.text=element_text(size=9),legend.title = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()