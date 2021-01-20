# this code will take all processed hospitalisation rates data years and load them
# then explore with figure output


rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_2.submit

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# expand grid of stuff temporary
years = c(1999:2014)

# load processed admissions file
dir.input.local = paste0('~/data/morbidity/US/medicare/processed/')
dat.admissions = readRDS(paste0(dir.input.local,'medicare_rates_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.admissions)=1:nrow(dat.admissions)

# expand grid to include all day-month-year-fips-ccs combinations
# TO DO


# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_rates_data/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

library(plyr)
library(ggplot2)

ccs_codes = sort(unique(dat.admissions$css_category))

# attach css names
ccs.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('css_category','full_name')
ccs.names$full_name = as.character(ccs.names$full_name)

# ADD NAMES

# rates overall by CCS
pdf(paste0(dir.output,'plot_by_ccs_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
for(ccs_current in ccs_codes){

    full_name = as.character(subset(ccs.names,css_category==ccs_current)[2])

    print(
    ggplot() +
    geom_histogram(data=subset(dat.admissions,css_category==ccs_current),aes(rate)) +
    xlab('Hospitalisation rate (per 100,000)') + ylab('Hospitalisation rate') + ggtitle(paste0(ccs_current,': ',full_name)) +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
    )
}
dev.off()