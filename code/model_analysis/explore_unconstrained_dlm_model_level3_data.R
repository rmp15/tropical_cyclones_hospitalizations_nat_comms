# this code will take all processed wind years and load them
# then explore with figure output

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

print(args)

# years of study
start_year = as.numeric(args[1]) # 1999
end_year = as.numeric(args[2]) # 2014
lag_chosen = as.numeric(args[3]) # 1

years=c(start_year:end_year)

# load CCS level 1 and 3 names
code.lookup.merged = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/CCS_lookup_2015/CCS_lookup_2015.csv')
code.lookup.merged$X = NULL
code.lookup.merged = unique(code.lookup.merged[,c('ccs_level_3','ccs_level_3_description','ccs_level_1','ccs_level_1_description')])
code.lookup.merged$ccs_level_3_description = as.character(code.lookup.merged$ccs_level_3_description)
code.lookup.merged$ccs_level_1_description = as.character(code.lookup.merged$ccs_level_1_description)
ccs_level_3 = unique(as.character(code.lookup.merged$ccs_level_3_description))
ccs_level_1 = unique(as.character(code.lookup.merged$ccs_level_1_description))

# for(lag_chosen in c(0:7)){

# load model summaries for CCS level 3
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dat.results = data.frame()
for(cod in ccs_level_3){
    file.current = paste0(dir.output.model.summary,'medicare_',gsub(" ", "_", cod),'_model_summary_',years[1],'_',years[length(years)],'.csv')
    if (file.exists(file.current)){
    print(cod)
    dat.results.current = read.csv(file.current)
    dat.results.current$ccs_level_1_description = code.lookup.merged$ccs_level_1_description[code.lookup.merged$ccs_level_3_description==cod]
    dat.results.current$ccs_level_3_description = cod
    dat.results.current$X =  dat.results.current$cause =NULL
    dat.results = rbind(dat.results.current, dat.results)}
}

# filter by appropriate lag
# dat.results = subset(dat.results,lag==lag_chosen)
rownames(dat.results) = seq(1:nrow(dat.results))

dat.results.level.3 = dat.results
dat.results.level.3$top = 0

dat.results.level.3$ccs_level_3 = NULL

dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_medicare_data/',years[1],'_',years[length(years)],'/')
num_hosps = read.csv(paste0(dir.output,'table_by_ccs_level_3_',years[1],'_',years[length(years)],'.csv'))
num_hosps$ccs_level_3_description = gsub(';', '', num_hosps$ccs_level_3_description)

dat.results.level.3 = merge(dat.results.level.3,num_hosps,by=c('ccs_level_3_description'),all.X=TRUE)

# load model summaries for CCS level 1
dir.output.model.summary = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/frequentist/unconstrained_dlm/summary/')
dat.results = data.frame()
for(cod in ccs_level_1){
    file.current = paste0(dir.output.model.summary,'medicare_',cod,'_model_summary_',years[1],'_',years[length(years)],'.csv')
    if (file.exists(file.current)){
    print(cod)
    dat.results.current = read.csv(file.current)
    dat.results.current$ccs_level_3_description = dat.results.current$ccs_level_1_description = dat.results.current$cause
    dat.results.current$X = dat.results.current$cause = NULL
    dat.results = rbind(dat.results.current, dat.results)}
}

# filter by appropriate lag
# dat.results = subset(dat.results,lag==lag_chosen)
rownames(dat.results) = seq(1:nrow(dat.results))

dat.results.level.1 = dat.results
dat.results.level.1$top = 1
dat.results.level.1$cases = 999999

# reorder to match level 1
dat.results.level.3 = dat.results.level.3[,names(dat.results.level.1)]

# combine level 3 and 1 levels
dat.results.total = rbind(dat.results.level.1,dat.results.level.3)

# get rid of influenza as really weird (need to figure out why)
# dat.results.total = subset(dat.results.total, ccs_level_3_description!='Influenza')

# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_unconstrained_dlm_model_data/',start_year,'_',end_year,'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

library(RColorBrewer)
library(gridExtra)
library(scales)
library(plyr)
library(ggplot2)

# create lag factor values for ggplot
dat.results.total$lag.factor = factor(dat.results.total$lag, levels=c(7:0))

# inclusion criteria by total number of cases in sub-group
include_limits = c(0,50000,100000,200000,500000)

# unique ccs level 1
ccs_level_1_descriptions = unique(dat.results.total$ccs_level_1_description)

for(ccs_level_1 in ccs_level_1_descriptions){
    for(include_limit in include_limits){

    print(ccs_level_1)

        pdf(paste0(dir.output,'forest_plot_all_lags_',ccs_level_1,'_include_limit_',include_limit,'_',start_year,'_',end_year,'.pdf'),paper='a4r',width=0,height=0)

        print(
        ggplot() +
        geom_errorbar(data=subset(dat.results.total,top==0&ccs_level_1_description==ccs_level_1&cases>include_limit),position=position_dodge(width=0.5),aes(x=lag.factor,ymax=(rr.ul-1),ymin=(rr.ll-1),color=ccs_level_3_description),width=.2,size=0.5) +
        geom_point(data=subset(dat.results.total,top==0&ccs_level_1_description==ccs_level_1&cases>include_limit),position=position_dodge(width=0.5), aes(x=lag.factor,y=(rr-1),color=ccs_level_3_description),size=3,shape=16) +
        geom_errorbar(data=subset(dat.results.total,top==1&ccs_level_1_description==ccs_level_1&cases>include_limit),position=position_dodge(width=0.5),aes(x=lag.factor,ymax=(rr.ul-1),ymin=(rr.ll-1)),width=0,size=0.5) +
        geom_point(data=subset(dat.results.total,top==1&ccs_level_1_description==ccs_level_1&cases>include_limit),position=position_dodge(width=0.5), aes(x=lag.factor,y=(rr-1)),size=3.5,shape=16) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Lag (days after event)') + ylab('Percentage change in hospitalization rates associated with tropical storm or hurricane exposure') +
        # ylim(ylims) +
        scale_y_continuous(labels=scales::percent) +
        ggtitle(paste0(ccs_level_1,': include limit=',include_limit)) +
        coord_flip() +
        theme_bw() + theme(text = element_text(size = 10),
        panel.grid.major = element_blank(),axis.text.y = element_text(angle=0),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))

        dev.off()

        # pdf(paste0(dir.output,'forest_plot_lag_',lag_chosen,'_',ccs_level_1,'_',start_year,'_',end_year,'.pdf'),paper='a4r',width=0,height=0)

        # print(
        # ggplot() +
        # geom_errorbar(data=subset(dat.results.total,ccs_level_1_description==ccs_level_1&cases>include_limit,color=as.factor(top)),position=position_dodge(width=0.5),aes(x=as.factor(ccs_level_3_description),ymax=(rr.ul-1),ymin=(rr.ll-1)),width=.2,size=0.5) +
        # geom_point(data=subset(dat.results.total,ccs_level_1_description==ccs_level_1&cases>include_limit,color=as.factor(top)),position=position_dodge(width=0.5), aes(x=as.factor(ccs_level_3_description),y=(rr-1)),size=3,shape=16) +
        # geom_hline(yintercept=0,linetype='dotted') +
        # xlab('Sub-cause') + ylab('Percentage change in hospitalization rates associated with tropical storm or hurricane exposure') +
        # # ylim(ylims) +
        # scale_y_continuous(labels=scales::percent) +
        # ggtitle(paste0('Lag ',lag_chosen,': ',ccs_level_1)) +
        # coord_flip() +
        # theme_bw() + theme(text = element_text(size = 10),
        # panel.grid.major = element_blank(),axis.text.y = element_text(angle=0),
        # plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        # panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        # panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        # legend.position = 'bottom',legend.justification='center',
        # legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        #
        # dev.off()
}}

# }