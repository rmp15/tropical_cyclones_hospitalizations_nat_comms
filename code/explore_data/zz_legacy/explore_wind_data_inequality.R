# this code will take all processed wind years and load them
# then explore with figure output

rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

print(args)

# years of study
start_year = as.numeric(args[1]) # 1999
end_year = as.numeric(args[2]) # 2016

# load processed wind file
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
counties.wind = readRDS(paste0(dir.input,'wind_data_all_',start_year,'_',end_year,'.rds'))

# only consider events above tropical depressions
counties.wind = subset(counties.wind,cat_sust!='tropical depression')

# temporary: only consider up to and including 2014
#counties.wind = subset(counties.wind,year<=2014)

# attach state names
counties.wind$state.fips = as.numeric(substr(counties.wind$fips,1,2))
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(2,15)),]
fips.lookup = fips.lookup[,c(1:2)]
counties.wind = merge(counties.wind,fips.lookup,by.x='state.fips',by.y='fips',all.x=TRUE)

# load SES measures

# merge with counties.wind exposure file

# output directory
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/explore_wind_data_inequality/',start_year,'_',end_year,'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

library(RColorBrewer)
library(gridExtra)
library(scales)
library(plyr)
library(ggplot2)

# how many events per state during period of study?
dat.state = ddply(counties.wind,.(full_name),nrow)
names(dat.state)[2] = 'cases'

# save
write.csv(dat.state,paste0(dir.output,'events_by_state_',start_year,'_',end_year,'.pdf'))

# how many events per state during period of study?
dat.county = ddply(counties.wind,.(fips),nrow)
names(dat.county)[2] = 'cases'

# PLOTS
#
#pdf(paste0(dir.output,'plot_by_state_and_category_',start_year,'_',end_year,'.pdf'),paper='a4r',width=0,height=0)
#ggplot(data=dat.state.cat,aes(x=full_name,y=cases,fill=cat_sust)) +
#    geom_bar(width = 0.9, stat = "identity") +
#    xlab('State') + ylab('Counts of wind events') +
#    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
#    axis.title.y = element_text(margin=margin(b=1000)),
#    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
#    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#    legend.position = 'bottom',legend.justification='center',
#    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
#dev.off()

# how many events during period of study by year and what kind are they?
dat.year = ddply(counties.wind,.(year,cat_sust),nrow)
names(dat.year)[3] = 'cases'

pdf(paste0(dir.output,'plot_by_year_',start_year,'_',end_year,'.pdf'),paper='a4r',width=0,height=0)
ggplot(data=dat.year,aes(x=year,y=cases,,fill=cat_sust)) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Year') + ylab('Counts of wind events') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# how many counties have ever had an event during the period of study?

###############################################################
# PREPARING MAP
###############################################################

library('hurricaneexposure'); library('hurricaneexposuredata'); library('tidyverse') ; library('lubridate')

# info for obtaining data
library(dplyr)
all_counties = county_centers %>% pull("fips")

# way of getting out all the states which are included in the dataset
counties.track = county_distance(counties=all_counties, start_year = start_year, end_year = end_year, dist_limit=1000000)
counties.track$state.fips = substr(counties.track$fips,1,2)
state.fips = sort(unique(counties.track$state.fips))

# for theme_map
#devtools::source_gist("33baa3a79c5cfef0f6df")
theme_map <- function(base_size=9, base_family=""){
    require(grid)
    theme_bw(base_size=base_size,base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.margin=unit(0,"lines"),
    plot.background=element_blank(),
    # legend.justification = c(0,0),
    legend.position = 'bottom'
    )
}

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(sp)
library(plyr)

# load county shapefile map_create

# load shapefile of entire United States county map_create
us.national <- readOGR(dsn="~/git/pollution/countries/USA/data/shapefiles/cb_2015_us_county_500k",layer="cb_2015_us_county_500k")

# get projection of shapefile
original.proj = proj4string(us.national)

# load county shapefile map_create
us.national <- spTransform(us.national, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

# remove non-mainland territories (assuming it's for entire mainland US)
us.main = us.national[us.national$STATEFP %in% state.fips,]

# also state file
us <- readOGR(dsn="~/git/mortality/USA/state/data/shapefiles",layer="states")

# fortify to prepare for ggplot
map <- fortify(us.main)

# extract data from shapefile
us.main@data$id <- rownames(us.main@data)
shapefile.data <- us.main@data

# merge selected data to map_create dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$GEOID <- as.integer(as.character(USA.df$GEOID))

# 1. has a county ever been exposed to coastal storm?

dat.event = unique(counties.wind[,c('fips','cat_sust')])
dat.event.2 = data.frame(fips=as.numeric(unique(counties.wind[,c('fips')])))
dat.event.2$exposure = 1

# merge event data with file
USA.df.test = merge(USA.df,dat.event.2,by.x='GEOID',by.y='fips',all.x=TRUE)
USA.df.test[is.na(USA.df.test)] <- 0
USA.df.test <- with(USA.df.test, USA.df.test[order(id,order),])

# convert shapefile to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# remove Alaska and Hawaii (don't put back in now)
us_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15"),]

us_aea = fortify(us_aea)

# plot
pdf(paste0(dir.output,'map_ever_exposed_',start_year,'_',end_year,'.pdf'),paper='a4r',width=0,height=0)
print(ggplot() +
geom_polygon(data=subset(USA.df.test),aes(x=long,y=lat,group=group,fill=as.factor(exposure)),color='white',size=0.01) +
geom_polygon(data=subset(us_aea),aes(x=long,y=lat,group=group),fill=NA,color='black',size=0.2) +
# scale_fill_gradientn(colors=ASDRpalette,guide_legend(title="Norm temperature (°C)")) +
guides(fill=guide_colorbar(barwidth=30)) +
coord_fixed() +
xlab('') +
ylab('') +
theme_map() +
theme(legend.title = element_blank(), text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(0.5,0.5),strip.background = element_blank(),legend.background = element_rect(fill = "white")))

dev.off()

# 2. how many days in total has a county been exposed?
dat.event.sum = ddply(counties.wind,.(fips),nrow)
dat.event.sum$fips = as.numeric(dat.event.sum$fips)
names(dat.event.sum)[2] = 'cases'
dat.event.sum$cases.per.year = dat.event.sum$cases / (2014 - start_year + 1)
dat.event.sum$cases.per.decade = 10 * dat.event.sum$cases.per.year

# save
write.csv(dat.event.sum,paste0(dir.output,'events_by_county_',start_year,'_',end_year,'.csv'))

# merge event data with file
USA.df.test = merge(USA.df,dat.event.sum,by.x='GEOID',by.y='fips',all.x=TRUE)
USA.df.test[is.na(USA.df.test)] <- 0
USA.df.test <- with(USA.df.test, USA.df.test[order(id,order),])

# convert shapefile to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# remove Alaska and Hawaii (don't put back in now)
us_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15"),]

us_aea = fortify(us_aea)

# palette for maps
colorfunc = colorRampPalette(c(brewer.pal(6 , "BrBG" )[1:3],brewer.pal(6 , "RdGy" )[4:6]))
# yearpalette = c('cornsilk',"darkseagreen2","darkseagreen3",'darkseagreen4','dodgerblue','dodgerblue1','dodgerblue2','dodgerblue3','dodgerblue4')
yearpalette = c('cornsilk',"blue","darkblue","black")

# plot
pdf(paste0(dir.output,'map_number_days_exposed_',start_year,'_',end_year,'.pdf'),paper='a4r',width=0,height=0)
print(ggplot() +
    geom_polygon(data=subset(USA.df.test),aes(x=long,y=lat,group=group,fill=cases),color='black',size=0.001) +
    geom_polygon(data=subset(us_aea),aes(x=long,y=lat,group=group),fill=NA,color='black',size=0.2) +
    guides(fill = guide_colorbar(direction = "horizontal", title.position="left",barwidth = 10, barheight = 1,title.vjust = 0.8,
            title = "Number of days of tropical cyclone exposure")) +
    coord_fixed() +
    xlab('') +
    ylab('') +
    scale_fill_gradientn(c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,1),
    colors=yearpalette,breaks=seq(0,20,5), limits = c(0,20),guide = guide_legend(nrow = 1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),#text = element_text(size = 10),
        axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),legend.text=element_text(size=15),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),legend.justification='center',
        legend.position = 'bottom', legend.background = element_rect(fill="white", size=.5, linetype="dotted")) +
    theme_map()
)
dev.off()

# 3. what percentage of days have been exposed?

# TO FINISH