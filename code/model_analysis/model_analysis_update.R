# this code will take a particular cause of death
# and run the chosen model for it

rm(list=ls())

# years of analysis
years = c(1999:2014)

# ccs names (temporarily keeping just 10 causes of death which we thought might be interesting)
ccs.names = read.csv('~/git/rmparks_coastal_storms_Jan_2020/data/Single_Level_CCS_2015/dxlabel 2015.csv')
names(ccs.names) = c('css_category','full_name')
ccs.names$full_name = as.character(ccs.names$full_name)
cods = ccs.names$full_name

# create complete grid of age, sex, and cause of death values
lags=c(-3:7)

# directory for results input
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/results/model_run/wind_events/summary/')

# load model summaries
dat.results = data.frame()
for(cod.arg in cods){
    for(lag in lags){

        # print arguments
        print(paste0(cod.arg,' ',lag))

        cod.arg = gsub(" ", "_", cod.arg)

        # load model summary
        if(file.exists(paste0(dir.input,'medicare_',gsub(" ", "_", cod.arg),'_model_summary_',years[1],'_',years[length(years)],'_lag',lag,'.csv'))){
            dat.results.single = read.csv(paste0(dir.input,'medicare_',gsub(" ", "_", cod.arg),'_model_summary_',years[1],'_',years[length(years)],'_lag',lag,'.csv'))
            dat.results = rbind(dat.results,dat.results.single)
        }

    }}

# tidy up a bit
dat.results$X = NULL
dat.results$cause = gsub("_", " ", dat.results$cause)

library(ggplot2)

# directory for results input
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/model_run/wind_events/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

# RR values per hospitalisation
pdf(paste0(dir.output,'medicare_rr_values_model_summary_',years[1],'_',years[length(years)],'.pdf'),paper='a4r',height=0,width=0)
for(cod in cods){
    print(
        ggplot(subset(dat.results,cause==cod&lag>-1)) +
        geom_point(aes(x=lag,y=rr)) +
        geom_errorbar(aes(x=lag,ymin=rr.ll,ymax=rr.ul)) +
        geom_hline(yintercept=1,linetype=2) +
        ggtitle(paste0(cod.arg,' (lags run separately)')) +
        xlab('Lag') + ylab('RR') +
        ylim(c(0.8,1.3)) +
        theme_bw() + theme(text = element_text(size = 8),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
        )
}
dev.off()
