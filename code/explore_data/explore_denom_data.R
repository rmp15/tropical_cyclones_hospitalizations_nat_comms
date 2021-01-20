# this code will take all processed denom years and load them
# then explore with figure output

rm(list=ls())

# run on Harvard RCE by using
# condor_submit process_fst_files_2.submit

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# expand grid of stuff temporary
years = c(1999:2014)

# load processed denom file
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/medicare_denom_processing/')
dat.denom = readRDS(paste0(dir.input,'medicare_denom_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.denom)=1:nrow(dat.denom)
dat.denom$fips = paste0('0',dat.denom$fips)
dat.denom$fips = substr(dat.denom$fips, nchar(dat.denom$fips)-5+1, nchar(dat.denom$fips))
dat.denom$state.fips = substr(dat.denom$fips, 1,2)

# attach state names
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(2,15)),]
fips.lookup = fips.lookup[,c(1:2)]
dat.denom$state.fips = as.numeric(dat.denom$state.fips)
dat.denom = merge(dat.denom,fips.lookup,by.x='state.fips',by.y='fips',all.x=TRUE)
dat.denom$full_name = as.character(dat.denom$full_name)
dat.denom$full_name[is.na(dat.denom$full_name)==TRUE] <- 'Died within year'

# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_denom_data/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# isolate only coastal states
state.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/name_lookup/ssa_standard_state_code_lookup.csv')
state.names = state.names[,c(2:4)]
dat.denom = merge(dat.denom,state.names,by.x='full_name',by.y='name',all.x=TRUE)

library(plyr)
library(ggplot2)

pdf(paste0(dir.output,'plot_by_year_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=subset(dat.denom,coastal_storm_state==1|full_name%in%c('Died within year')),aes(x=year,y=population)) +
    geom_col() +
    xlab('Year') + ylab('Medicare population') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# how many hospitalisations by state in dataset?
pdf(paste0(dir.output,'plot_by_state_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=subset(dat.denom,coastal_storm_state==1|full_name%in%c('Died within year')),aes(x=year,y=population)) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Year') + ylab('Medicare population') +
    facet_wrap(~full_name,scale='free') +

    theme_bw() + theme(text = element_text(size = 10),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# how many hospitalisations by state in dataset?
pdf(paste0(dir.output,'plot_by_state_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=subset(dat.denom,coastal_storm_state==1|full_name%in%c('Died within year')),aes(x=year,y=population)) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Year') + ylab('Medicare population') +
    facet_wrap(~full_name,scale='free') +

    theme_bw() + theme(text = element_text(size = 10),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# how many hospitalisations by county in each state in dataset?
pdf(paste0(dir.output,'plot_by_state_and county_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
for(i in sort(unique(dat.denom$full_name))){
print(ggplot(data=subset(dat.denom,full_name==i),aes(x=year,y=population,group=fips)) +
    geom_line() +
    xlab('Year') + ylab('Medicare population') + ggtitle(i) +
    # facet_wrap(~fips,scale='free') +

    theme_bw() + theme(text = element_text(size = 10),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
}
dev.off()


