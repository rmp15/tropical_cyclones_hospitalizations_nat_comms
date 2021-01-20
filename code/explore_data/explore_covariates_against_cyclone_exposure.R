rm(list=ls())

library(spdep)
library(rgdal)
library(foreign)
library(dplyr)
library(reshape2)
library(ggplot2)

setwd("~/git/mortality/USA/state/data/covariates/")

################ DEFINE ######################

#id_sex <- 2 # select sex type (1=male, 2=female)
startyear <- 1999 # extracting data starting in this year
obs.Years <- 18 # ... for n years

refyr <- 2000 # reference year to which LC mort rate and income should be standardised
id_race <- 2 #1=white, 2=black, 3=native, 4=asian

################ LOAD COVARIATE DATA ######################

# DATAFRAME SAVE
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/covariates/',startyear,'_',(startyear+obs.Years-1),'/')
data_complete = read.csv(paste0(dir.input,'covariates_all_years_',startyear,'_',(startyear+obs.Years-1),'.csv'))

################ LOAD WIND DATA ######################

# load processed wind file
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
counties.wind = readRDS(paste0(dir.input,'wind_data_all_',startyear,'_',(startyear+obs.Years-1),'.rds'))

# only consider events above tropical depressions
counties.wind = subset(counties.wind,cat_sust!='tropical depression')

# how many days in total has a county been exposed?
library(plyr)
dat.event.sum = ddply(counties.wind,.(fips),nrow)
#dat.event.sum$fips = as.numeric(dat.event.sum$fips)
names(dat.event.sum)[2] = 'cases'
dat.event.sum$cases.per.year = dat.event.sum$cases / ((startyear+obs.Years-1) - startyear)
dat.event.sum$cases.per.decade = 10 * dat.event.sum$cases.per.year

################ MERGE COVARIATE AND WIND DATA ######################

data_complete_test = left_join(dat.event.sum,data_complete,by=c('fips'))

################ PLOTS ######################

# isolate one year of covariate data
dat_complete_single_year = subset(data_complete_test, year==2010)

# replace NAs with zeroes
#dat_complete_single_year[["cases.per.decade"]][is.na(dat_complete_single_year[["cases.per.decade"]])] <- 0

dat_complete_single_year_na = dat_complete_single_year[rowSums(is.na(dat_complete_single_year))>0,]

# percentage black
dat_complete_single_year$quintile_race <- with(dat_complete_single_year, cut(popraceprop,
                                breaks=quantile(popraceprop, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                include.lowest=TRUE))

# income
dat_complete_single_year$quintile_income <- with(dat_complete_single_year, cut(inc_pc_sc,
                                breaks=quantile(inc_pc_sc, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                include.lowest=TRUE))

# percentage poverty
dat_complete_single_year$quintile_poverty <- with(dat_complete_single_year, cut(poppov_sc,
                                breaks=quantile(poppov_sc, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                include.lowest=TRUE))

# percentage urban
dat_complete_single_year$quintile_urban <- with(dat_complete_single_year, cut(urban,
                                breaks=quantile(urban, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                include.lowest=TRUE))

# percentage unemployed
dat_complete_single_year$quintile_unemployed <- with(dat_complete_single_year, cut(unemp_rate,
                                breaks=quantile(unemp_rate, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                include.lowest=TRUE))

# percentage high school graduate
dat_complete_single_year$quintile_hsgrad <- with(dat_complete_single_year, cut(hsgrad,
                                breaks=quantile(hsgrad, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                include.lowest=TRUE))

dat_complete_single_year_summary_race = ddply(dat_complete_single_year,.(quintile_race),summarize,mean=mean(cases.per.decade))
dat_complete_single_year_summary_income = ddply(dat_complete_single_year,.(quintile_income),summarize,mean=mean(cases.per.decade))
dat_complete_single_year_summary_poverty = ddply(dat_complete_single_year,.(quintile_poverty),summarize,mean=mean(cases.per.decade))
dat_complete_single_year_summary_urban = ddply(dat_complete_single_year,.(quintile_urban),summarize,mean=mean(cases.per.decade))
dat_complete_single_year_summary_unemployed = ddply(dat_complete_single_year,.(quintile_unemployed),summarize,mean=mean(cases.per.decade))
dat_complete_single_year_summary_hsgrad = ddply(dat_complete_single_year,.(quintile_hsgrad),summarize,mean=mean(cases.per.decade))

# race
ggplot(dat_complete_single_year, aes(x=quintile_race, y=cases.per.decade)) +
  geom_jitter() +
  geom_violin() +
  geom_point(data=dat_complete_single_year_summary_race, aes(x=as.factor(quintile_race), y=mean),size=3,color='red') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),#text = element_text(size = 10),
        axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),legend.text=element_text(size=15),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),legend.justification='center',
        legend.position = 'bottom', legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# income
ggplot(na.omit(dat_complete_single_year), aes(x=quintile_income, y=cases.per.decade)) +
  geom_jitter() +
  geom_violin() +
  geom_point(data=na.omit(dat_complete_single_year_summary_income), aes(x=as.factor(quintile_income), y=mean),size=3,color='red') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),#text = element_text(size = 10),
        axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),legend.text=element_text(size=15),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),legend.justification='center',
        legend.position = 'bottom', legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# poverty
ggplot(na.omit(dat_complete_single_year), aes(x=quintile_poverty, y=cases.per.decade)) +
  geom_jitter() +
  geom_violin() +
  geom_point(data=na.omit(dat_complete_single_year_summary_poverty), aes(x=as.factor(quintile_poverty), y=mean),size=3,color='red') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),#text = element_text(size = 10),
        axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),legend.text=element_text(size=15),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),legend.justification='center',
        legend.position = 'bottom', legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# urban
ggplot(na.omit(dat_complete_single_year), aes(x=quintile_urban, y=cases.per.decade)) +
  geom_jitter() +
  geom_violin() +
  geom_point(data=na.omit(dat_complete_single_year_summary_urban), aes(x=as.factor(quintile_urban), y=mean),size=3,color='red') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),#text = element_text(size = 10),
        axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),legend.text=element_text(size=15),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),legend.justification='center',
        legend.position = 'bottom', legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# unemployed
ggplot(na.omit(dat_complete_single_year), aes(x=quintile_unemployed, y=cases.per.decade)) +
  geom_jitter() +
  geom_violin() +
  geom_point(data=na.omit(dat_complete_single_year_summary_unemployed), aes(x=as.factor(quintile_unemployed), y=mean),size=3,color='red') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),#text = element_text(size = 10),
        axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),legend.text=element_text(size=15),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),legend.justification='center',
        legend.position = 'bottom', legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# unemployed
ggplot(na.omit(dat_complete_single_year), aes(x=quintile_hsgrad, y=cases.per.decade)) +
  geom_jitter() +
  geom_violin() +
  geom_point(data=na.omit(dat_complete_single_year_summary_hsgrad), aes(x=as.factor(quintile_hsgrad), y=mean),size=3,color='red') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),#text = element_text(size = 10),
        axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),legend.text=element_text(size=15),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),legend.justification='center',
        legend.position = 'bottom', legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# add state names
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(2,15)),]
fips.lookup = fips.lookup[,c(1:2)]
dat_complete_single_year = merge(dat_complete_single_year,fips.lookup,by.x='STATEFP',by.y='fips',all.x=TRUE)

# population against exposure
ggplot(data=na.omit(dat_complete_single_year),aes(x=population,y=cases.per.decade)) + geom_point() + facet_wrap(~full_name) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),#text = element_text(size = 10),
        axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),legend.text=element_text(size=15),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),legend.justification='center',
        legend.position = 'bottom', legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

