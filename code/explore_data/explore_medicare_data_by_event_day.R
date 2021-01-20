# this code will take all processed admissions years and load them
# then explore coastal storm matched output

# run using a bash file

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

library(plyr)
library(ggplot2)

# expand grid of stuff temporary
years = c(1999:2014)

# load processed admissions file
print('loading Medicare data')
dir.input.local = paste0('~/data/morbidity/US/medicare/processed/')

# load data (assuming explore_medicare_data.R has been run for relevant years)
dat.admissions = readRDS(paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_no_statewide_',years[1],'_',years[length(years)],'.rds'))

# convert to integers
dat.admissions$day = as.numeric(dat.admissions$day)
dat.admissions$month = as.numeric(dat.admissions$month)
dat.admissions$year = as.numeric(dat.admissions$year)

#  coastal storm event data as would need to add coastal storm events which are associated with zero cases on a fipcounty-day
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
counties.wind.edit.array = readRDS(paste0(dir.input,'wind_data_lag_array_',years[1],'_2016.rds'))
counties.wind.edit.array$event_lag07 = rowSums(counties.wind.edit.array[,c('event_lag0','event_lag1','event_lag2','event_lag3','event_lag4',
                                                                            'event_lag5','event_lag6','event_lag7')])
counties.wind.edit.array$event_lag07 = ifelse(counties.wind.edit.array$event_lag07>0,1,counties.wind.edit.array$event_lag07)

dat.merged.multiple = merge(dat.admissions,counties.wind.edit.array,by=c('year','month','day','fipscounty'),
                                                by.y=c('year','month','day','fips'),all.x=TRUE)

# save matched data
saveRDS(dat.merged.multiple,paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_no_statewide_matched_wind_events_',years[1],'_',years[length(years)],'.rds'))


dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_medicare_data/',years[1],'_',years[length(years)],'/')

# SUMMARY TABLES (lag0)
dat.css.level.1 = ddply(dat.merged.multiple,.(ccs_level_1, ccs_level_1_description,event_lag0),summarize,cases=sum(cases))
write.csv(dat.css.level.1,paste0(dir.output,'table_by_ccs_level_1_by_event_lag0_',years[1],'_',years[length(years)],'.csv'),row.names=FALSE)

dat.css.level.1 = ddply(dat.merged.multiple,.(ccs_level_1_description,event_lag0),summarize,cases=sum(cases))
write.csv(dat.css.level.1,paste0(dir.output,'table_by_ccs_level_1_other_together_by_event_lag0_',years[1],'_',years[length(years)],'.csv'),row.names=FALSE)

dat.css.level.1 = ddply(dat.merged.multiple,.(ccs_level_1_description,event_lag0),nrow)
names(dat.css.level.1)[3] = 'days'
write.csv(dat.css.level.1,paste0(dir.output,'table_by_ccs_level_1_other_together_by_event_number_days_lag0_',years[1],'_',years[length(years)],'.csv'),row.names=FALSE)

# SUMMARY TABLES (lag07)
dat.css.level.1 = ddply(dat.merged.multiple,.(ccs_level_1, ccs_level_1_description,event_lag07),summarize,cases=sum(cases))
write.csv(dat.css.level.1,paste0(dir.output,'table_by_ccs_level_1_by_event_lag07_',years[1],'_',years[length(years)],'.csv'),row.names=FALSE)

dat.css.level.1 = ddply(dat.merged.multiple,.(ccs_level_1_description,event_lag07),summarize,cases=sum(cases))
write.csv(dat.css.level.1,paste0(dir.output,'table_by_ccs_level_1_other_together_by_event_lag07_',years[1],'_',years[length(years)],'.csv'),row.names=FALSE)

dat.css.level.1 = ddply(dat.merged.multiple,.(ccs_level_1_description,event_lag07),nrow)
names(dat.css.level.1)[3] = 'days'
write.csv(dat.css.level.1,paste0(dir.output,'table_by_ccs_level_1_other_together_by_event_number_days_lag07_',years[1],'_',years[length(years)],'.csv'),row.names=FALSE)

# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_medicare_data/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

library(plyr)
library(ggplot2)

# source variables for colorinng etc.
source('~/git/rmparks_coastal_storms_Jan_2020/data/objects/objects.R')

# how many hospitalisations by year by ccs level 1 in dataset?
dat.year = ddply(dat.admissions,.(year,ccs_level_1_description),summarize,cases=sum(cases))

library(scales)

pdf(paste0(dir.output,'plot_by_year_and_ccs_level_1_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=subset(dat.year,year>=1999),aes(x=year,y=cases/1000000,fill=ccs_level_1_description)) +
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

pdf(paste0(dir.output,'plot_by_year_and_ccs_level_1_100perc_bar_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=subset(dat.year,year>=1999),aes(x=year,y=cases/1000000,fill=ccs_level_1_description)) +
    geom_col(position = "fill",stat = "identity") +
    xlab('Year') + ylab('Hospitalizations (percentage of total within year)') +
    scale_y_continuous(labels = scales::percent) +
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

# how many hospitalisations by ccs level 1 in dataset?
dat.ccs = ddply(dat.admissions,.(ccs_level_1_description),summarize,cases=sum(cases))

pdf(paste0(dir.output,'plot_by_ccs_level_1_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=subset(dat.ccs),aes(x=ccs_level_1_description,y=cases/1000000)) +
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

# how many hospitalisations by state in dataset?
dat.state = ddply(dat.admissions,.(name,ccs_level_1_description),summarize,cases=sum(cases))

pdf(paste0(dir.output,'plot_by_state_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.state,aes(x=name,y=cases/1000000,fill=ccs_level_1_description)) +
    geom_col() +
    xlab('State') + ylab('Hospitalizations (millions)') +
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

pdf(paste0(dir.output,'plot_by_state_and_ccs_level_1_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.state,aes(x=name,y=cases/1000000,fill=ccs_level_1_description)) +
    geom_col() +
    xlab('State') + ylab('Hospitalizations (millions)') +
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

pdf(paste0(dir.output,'plot_by_state_and_ccs_level_1_100perc_bar_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.state,aes(x=name,y=cases,fill=ccs_level_1_description)) +
    geom_col(position = "fill",stat = "identity") +
    xlab('State') + ylab('Hospitalizations') +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values=colors.ccs.level.1) +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.text=element_text(size=10),legend.title = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()