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

# only go through trying to process results if the main file used to plotting doesn't exist
if(!file.exists(paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_no_statewide_',years[1],'_',years[length(years)],'.rds'))){

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

    # fix CCS level 1 names
    dat.admissions$ccs_level_1_description = as.character(dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description <- gsub(';', '', dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description <- gsub('\\.', '', dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description <- gsub('\\[', '', dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description <- gsub('\\]', '', dat.admissions$ccs_level_1_description)

    dat.admissions$ccs_level_1_description = gsub('Diseases of the circulatory system', 'Cardiovascular diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Diseases of the respiratory system', 'Respiratory diseases',  dat.admissions$ccs_level_1_description)

    dat.admissions$ccs_level_1_description = gsub('Neoplasms', 'Cancers',  dat.admissions$ccs_level_1_description)

    dat.admissions$ccs_level_1_description = gsub('Injury and poisoning', 'Injuries',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Mental illness', 'Neuropsychiatric disorders',  dat.admissions$ccs_level_1_description)

    dat.admissions$ccs_level_1_description = gsub('Diseases of the blood and blood-forming organs', 'Blood diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Diseases of the digestive system', 'Digestive system diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Endocrine nutritional and metabolic diseases and immunity disorders", 'Endocrine disorders',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Diseases of the genitourinary system', 'Genitourinary diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Infectious and parasitic diseases', 'Infectious and parasitic diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Musculoskeletal and connective tissue diseases', 'Musculoskeletal and\nconnective tissue diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Diseases of the musculoskeletal system and connective tissue', 'Musculoskeletal and connective tissue diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Diseases of the nervous system and sense organs', 'Nervous system diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Diseases of the skin and subcutaneous tissue", 'Skin and subcutaneous tissue diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub("Skin and subcutaneous tissue diseases", 'Skin and subcutaneous\ntissue diseases',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Congenital anomalies', 'Other',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Residual codes unclassified all E codes 259 and 260', 'Other',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Certain conditions originating in the perinatal period', 'Other',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Complications of pregnancy childbirth and the puerperium', 'Other',  dat.admissions$ccs_level_1_description)
    dat.admissions$ccs_level_1_description = gsub('Symptoms signs and ill-defined conditions and factors influencing health status', 'Other',  dat.admissions$ccs_level_1_description)

    # reorder CCS level 1 causes for plotting
    dat.admissions$ccs_level_1_description = factor(dat.admissions$ccs_level_1_description,
                            levels=c('Cardiovascular diseases','Respiratory diseases','Cancers','Injuries','Neuropsychiatric disorders',
                                        'Blood diseases','Digestive system diseases','Endocrine disorders','Genitourinary diseases',
                                        'Infectious and parasitic diseases','Musculoskeletal and\nconnective tissue diseases',
                                        'Nervous system diseases','Skin and subcutaneous\ntissue diseases','Other'))

    # save for quick access if happy with name changes
    saveRDS(dat.admissions,paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_',years[1],'_',years[length(years)],'.rds'))

    # only allow from 1999 onwards
    dat.admissions = subset(dat.admissions,year>=1999)

    print(paste0('Total number of hospitalizations 1999 and after is ', sum(dat.admissions$cases)))

    # not STATEWIDE
    dat.admissions = subset(dat.admissions,county!='STATEWIDE')

    print(paste0('Total number of hospitalizations not including STATEWIDE ', sum(dat.admissions$cases)))

    # save for quick access if happy with name changes
    saveRDS(dat.admissions,paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_no_statewide_',years[1],'_',years[length(years)],'.rds'))

    # SUMMARY TABLES
    dat.css.level.1 = ddply(dat.admissions,.(ccs_level_1, ccs_level_1_description),summarize,cases=sum(cases))
    write.csv(dat.css.level.1,paste0(dir.output,'table_by_ccs_level_1_',years[1],'_',years[length(years)],'.csv'),row.names=FALSE)

    dat.css.level.1 = ddply(dat.admissions,.(ccs_level_1_description),summarize,cases=sum(cases))
    write.csv(dat.css.level.1,paste0(dir.output,'table_by_ccs_level_1_other_together_',years[1],'_',years[length(years)],'.csv'),row.names=FALSE)

    dat.css.level.3 = ddply(dat.admissions,.(ccs_level_3, ccs_level_3_description),summarize,cases=sum(cases))
    write.csv(dat.css.level.3,paste0(dir.output,'table_by_ccs_level_3_',years[1],'_',years[length(years)],'.csv'),row.names=FALSE)

    dat.css.state = ddply(dat.admissions,.(name),summarize,cases=sum(cases))
    write.csv(dat.css.state,paste0(dir.output,'table_by_state_',years[1],'_',years[length(years)],'.csv'),row.names=FALSE)
}

# load data
dat.admissions = readRDS(paste0(dir.input.local,'medicare_admissions_changed_ccs_level_1names_no_statewide_',years[1],'_',years[length(years)],'.rds'))

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