# this code will take all processed admissions years and load them
# then explore with figure output

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

# only load all medicare admissions if coastal storms doesn't exist
if(!file.exists(paste0(dir.input.local,'medicare_admissions_coastal_storm_states_',years[1],'_',years[length(years)],'.rds'))){
    dat.admissions = readRDS(paste0(dir.input.local,'medicare_admissions_',years[1],'_',years[length(years)],'.rds'))

    rownames(dat.admissions)=1:nrow(dat.admissions)
    print('loaded Medicare data')

    print(paste0('Total number of hospitalizations is ', sum(dat.admissions$cases)))

    # attach state names
    state.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/name_lookup/ssa_standard_state_code_lookup.csv')
    state.names = state.names[,c(1:4)]
    dat.admissions = merge(dat.admissions,state.names,by.x='SSA_STATE_CD',by.y='code',all.x=TRUE)

    # only have coastal states
    dat.admissions = subset(dat.admissions,coastal_storm_state==1)
    names(dat.admissions)[3] = 'ccs_level_3'

    print(paste0('Total number of hospitalizations in coastal storm states is ', sum(dat.admissions$cases)))

    saveRDS(dat.admissions,paste0(dir.input.local,'medicare_admissions_coastal_storm_states_',years[1],'_',years[length(years)],'.rds'))

}

# only load all coastal storm state admissions if CCS merged doesn't exist
if(!file.exists(paste0(dir.input.local,'medicare_admissions_ccs_merged_',years[1],'_',years[length(years)],'.rds'))){

    dat.admissions = readRDS(paste0(dir.input.local,'medicare_admissions_coastal_storm_states_',years[1],'_',years[length(years)],'.rds'))

    # merge CCS level 1 names
    code.lookup.merged = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/CCS_lookup_2015.csv')
    code.lookup.merged$X = NULL
    code.lookup.merged = unique(code.lookup.merged[,c('ccs_level_3','ccs_level_3_description','ccs_level_1','ccs_level_1_description')])
    dat.admissions = merge(dat.admissions,code.lookup.merged,by=c('ccs_level_3'),all.x=TRUE)

    # get rid of CCS level 3 which are a value of 0
    dat.admissions = subset(dat.admissions,ccs_level_3!=0)

    print(paste0('Total number of hospitalizations with full level 3 CCS is ', sum(dat.admissions$cases)))

    saveRDS(dat.admissions,paste0(dir.input.local,'medicare_admissions_ccs_merged_',years[1],'_',years[length(years)],'.rds'))

}

#####################################################################
# only load all coastal storm state admissions if CCS merged doesn't exist
if(!file.exists(paste0(dir.input.local,'medicare_admissions_full_fips_',years[1],'_',years[length(years)],'.rds'))){

    # filter out counties which aren't included in the analysis in coastal states because they don't have wind events
    dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
    counties.wind.edit.array = readRDS(paste0(dir.input,'wind_data_lag_array_',years[1],'_2016.rds'))
    counties.wind.edit.array = subset(counties.wind.edit.array, year%in%years)
    dat.event.multiple.test = ddply(counties.wind.edit.array, .(fips), summarize, NumSubs = length(unique(event_lag0)))
    dat.event.multiple.test = subset(dat.event.multiple.test,NumSubs<2)
    fips_to_exclude = as.numeric(as.character(dat.event.multiple.test$fips))

    print(paste0('Total number of counties with at least one wind event is ', length(unique(counties.wind.edit.array$fips))-length(fips_to_exclude)))

    # now exclude counties which do not have at least one wind event
    # it's super annoying because medicare data use SSA whereas coastal storms uses fips

    # make all SSD_CNTY_CD three characters long, adding zeroes at the beginning of values that aren't three characters long
    dat.admissions$SSA_CNTY_CD = paste0('00',dat.admissions$SSA_CNTY_CD)
    dat.admissions$SSA_CNTY_CD = substr(dat.admissions$SSA_CNTY_CD, nchar(dat.admissions$SSA_CNTY_CD)-3+1, nchar(dat.admissions$SSA_CNTY_CD))

    # make unique ssa state county code to merge with fips lookup
    dat.admissions$ssacounty = with(dat.admissions,paste0(SSA_STATE_CD,SSA_CNTY_CD))
    dat.admissions$ssacounty = as.numeric(dat.admissions$ssacounty)

    # fips ssa lookup
    fips.ssa.lookup = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/ssa_fips_state_county2017/ssa_fips_state_county2017.csv')
    fips.ssa.lookup = fips.ssa.lookup[,c(1:4)]

    # merge admissions file with ssa lookup file
    dat.admissions = merge(dat.admissions,fips.ssa.lookup,by.x='ssacounty',by.y='ssacounty',all.x=TRUE)

    # remove cases with SSA codes which are not in the FIPS directory
    dat.admissions = na.omit(dat.admissions)

    print(paste0('Total number of hospitalizations with full FIPS records is ', sum(dat.admissions$cases)))

    saveRDS(dat.admissions,paste0(dir.input.local,'medicare_admissions_full_fips_',years[1],'_',years[length(years)],'.rds'))

}

# only load admissions with any amount of wind event if CCS merged doesn't exist
if(!file.exists(paste0(dir.input.local,'medicare_admissions_at_least_one_wind_event_',years[1],'_',years[length(years)],'.rds'))){

    # exclude fips codes which do not have at least one wind event

    dat.admissions = subset(dat.admissions,!(fipscounty%in%fips_to_exclude))

    print(paste0('Total number of hospitalizations in counties with at least one wind event is ', sum(dat.admissions$cases)))

    saveRDS(dat.admissions,paste0(dir.input.local,'medicare_admissions_at_least_one_wind_event_',years[1],'_',years[length(years)],'.rds'))

}

#####################################################################

dat.admissions = readRDS(paste0(dir.input.local,'medicare_admissions_at_least_one_wind_event_',years[1],'_',years[length(years)],'.rds'))

# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_medicare_data/',years[1],'_',years[length(years)],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

library(plyr)
library(ggplot2)

# WHEN YOU GET TO HERE AGAIN, SAVE THE FILE TO BE SURE

# fix CCS level 1 names
dat.admissions$ccs_level_1_description = as.character(dat.admissions$ccs_level_1_description)
dat.admissions$ccs_level_1_description <- gsub(';', '', dat.admissions$ccs_level_1_description)
dat.admissions$ccs_level_1_description <- gsub('\\.', '', dat.admissions$ccs_level_1_description)
dat.admissions$ccs_level_1_description <- gsub('\\[', '', dat.admissions$ccs_level_1_description)
dat.admissions$ccs_level_1_description <- gsub('\\]', '', dat.admissions$ccs_level_1_description)

# dat.admissions$ccs_level_1_description = gsub('Diseases of the genitourinary system', 'Genitourinary\ndiseases', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Diseases of the respiratory system', 'Respiratory\ndiseases', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and\nconnective tissue diseases', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Diseases of the circulatory system', 'Circulatory system\ndiseases', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Infectious and parasitic diseases', 'Infectious and parasitic\ndiseases', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Injury and poisoning', 'Injury and\npoisoning', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Mental illness', 'Mental illness', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Endocrine nutritional and metabolic diseases and immunity disorders', 'Endocrine, nutritional\nand metabolic diseases and immunity disorders', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Diseases of the digestive system', 'Digestive system\ndiseases', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Symptoms signs and ill-defined conditions and factors influencing health status', 'Other', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Neoplasms', 'Cancers', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Diseases of the blood and blood-forming organs', 'Blood\ndiseases', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Diseases of the nervous system and sense organs', 'Nervous system\ndiseases', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Congenital anomalies', 'Other', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Diseases of the skin and subcutaneous tissue', 'Skin and subcutaneous tissue\ndiseases', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Residual codes unclassified all E codes 259 and 260', 'Other', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Certain conditions originating in the perinatal period', 'Perinatal conditions', dat.admissions$ccs_level_1_description)
# dat.admissions$ccs_level_1_description = gsub('Complications of pregnancy childbirth and the puerperium', 'Maternal conditions', dat.admissions$ccs_level_1_description)

# how many hospitalisations by year in dataset?
dat.year = ddply(dat.admissions,.(year,ccs_level_1_description),summarize,cases=sum(cases))

pdf(paste0(dir.output,'plot_by_year_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=subset(dat.year,year>=1999),aes(x=year,y=cases)) +
    geom_col() +
    xlab('Year') + ylab('Hospitalisations') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

library(scales)

pdf(paste0(dir.output,'plot_by_year_and_ccs_level_1_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=subset(dat.year,year>=1999),aes(x=year,y=cases,fill=ccs_level_1_description)) +
    geom_col() +
    xlab('Year') + ylab('Hospitalisations') +
    scale_y_continuous(labels = comma) +
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

# how many hospitalisations by month in dataset?
dat.month = ddply(dat.admissions,.(month),summarize,cases=sum(cases))

pdf(paste0(dir.output,'plot_by_month_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.month,aes(x=month,y=cases)) +
    geom_col() +
    xlab('Month') + ylab('Hospitalisations') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# how many hospitalisations by ccs in dataset?
dat.ccs = ddply(dat.admissions,.(css_category),summarize,cases=sum(cases))

# attach css names
ccs.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('css_category','full_name')

dat.ccs = merge(dat.ccs,ccs.names,by='css_category',all.x=TRUE)

dat.top.10 = dplyr::top_n(dat.ccs, 10)
dat.top.10 = merge(dat.top.10,ccs.names)
dat.top.10$cases=NULL

pdf(paste0(dir.output,'plot_by_ccs_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.ccs,aes(x=css_category,y=cases)) +
    geom_col() +
    xlab('CCS') + ylab('Hospitalisations') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(dir.output,'plot_by_ccs_fullname_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.ccs,aes(x=full_name,y=cases)) +
    geom_col() +
    xlab('Name') + ylab('Hospitalisations') +
    theme_bw() + theme(text = element_text(size = 7), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# how many hospitalisations by state in dataset?
dat.state = ddply(dat.admissions,.(name),summarize,cases=sum(cases))

pdf(paste0(dir.output,'plot_by_state_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.state,aes(x=name,y=cases)) +
    geom_col() +
    xlab('State') + ylab('Hospitalisations') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# how many hospitalisations per top 10 causes of death by year
dat.year.ccs = ddply(dat.admissions,.(css_category,year),summarize,cases=sum(cases))
dat.year.ccs.test = merge(dat.year.ccs, dat.top.10,by.x='css_category',by.y='css_category',all.x=TRUE)
dat.year.ccs.test$full_name = as.character(dat.year.ccs.test$full_name)
dat.year.ccs.test[is.na(dat.year.ccs.test)] <- 'Other'
dat.year.ccs.test = ddply(dat.year.ccs.test,.(year,full_name),summarise,cases=sum(cases))

pdf(paste0(dir.output,'plot_by_year_and_ccs_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.year.ccs.test,aes(x=year,y=cases,fill=full_name)) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Year') + ylab('Hospitalisations') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(dir.output,'plot_by_year_and_ccs_points_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.year.ccs.test,aes(x=year,y=cases,color=full_name)) +
    geom_point() +
    xlab('Year') + ylab('Hospitalisations') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(dir.output,'plot_by_year_and_ccs_facetted_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.year.ccs.test,aes(x=year,y=cases,fill=full_name)) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Year') + ylab('Hospitalisations') +
    facet_wrap(~full_name,scale='free') +
    theme_bw() + theme(text = element_text(size = 10), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()