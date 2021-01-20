rm(list=ls())

library(spdep)
library(rgdal)
library(foreign)
library(dplyr)
library(reshape2)
library(ggplot2)

setwd("~/git/mortality/USA/state/data/covariates/")

################ DEFINE ######################

startyear <- 1999 # extracting data starting in this year
obs.Years <- 18 # ... for n years

refyr <- 2000 # reference year to which LC mort rate and income should be standardised
id_race <- 2 #1=white, 2=black, 3=native, 4=asian

##################### INCLUDE POPULATION DATA ####################

load('nchs_raw_annotated_withag_1990_to_2016')
pop_nchs_allage <- as.data.frame(dplyr::summarise(group_by(subset(dat_nchs),year,fips),popsum=sum(popsum)))
popsum_nchs <- subset(pop_nchs_allage)
#pop_nchs_allage <- as.data.frame(summarise(group_by(subset(dat_nchs,sex==id_sex),year,fips,sex),popsum=sum(popsum)))
#popsum_nchs <- subset(pop_nchs_allage,sex==id_sex)

################## INCLUDE INCOME DATA ####################
load('income_with_sc')
income_us_sc <- subset(income_us_sc, year >= startyear & year < (startyear+obs.Years))

# remove all US and all county values as well as overseas, AK, HI
income_us_sc$STATEFP <- substr(income_us_sc$fips,1,2)
income_us_sc$COUNTYFP <- substr(income_us_sc$fips,3,5)
income_us_sc <- subset(income_us_sc,STATEFP != '00')
income_us_sc <- subset(income_us_sc,COUNTYFP != '000')
income_us_sc <- subset(income_us_sc,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79'))

# adjust for inflation 
CPIdf <- read.csv2('CPI.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)
CPI_base <- subset(CPIdf,year==refyr)$CPI
CPIdf$ratio <- as.numeric(unlist(CPI_base))/as.numeric(unlist(CPIdf$CPI))
income_us_sc <- left_join(income_us_sc,CPIdf,by='year')
income_us_sc$pc_const <- round(income_us_sc$inc_pc_sc*income_us_sc$ratio)

# fix counties
fix_counties = function(dat){
  dat[dat$fips == 12086 , "fips"] <- 12025
  return(dat)
}

income_us_sc = fix_counties(income_us_sc)

################## INCLUDE POVERTY DATA ##################
load('povperc_with_sc')
poppov_us_sc <- subset(poppov_us_sc, year >= startyear & year < (startyear+obs.Years))

# remove all US and all county figures as well as overseas, AK, HI
poppov_us_sc$STATEFP <- substr(poppov_us_sc$fips,1,2)
poppov_us_sc$COUNTYFP <- substr(poppov_us_sc$fips,3,5)
poppov_us_sc <- subset(poppov_us_sc,STATEFP != '00')
poppov_us_sc <- subset(poppov_us_sc,COUNTYFP != '000')
poppov_us_sc <- subset(poppov_us_sc,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79'))

################## INCLUDE RACE DATA ##################
# trim race data 
#pop_race <- subset(dat_nchs,race==id_race & sex==id_sex & year >= startyear & year < (startyear+obs.Years))
pop_race <- subset(dat_nchs,race==id_race & year >= startyear & year < (startyear+obs.Years))

# sum for this race for each fips-year
pop_race_allage <- data.frame(dplyr::summarise(group_by(pop_race,year,fips),popsum=sum(popsum)))

# get race perc 
poprace_us_merged <- left_join(pop_race_allage,pop_nchs_allage,by=c('year','fips'))
poprace_us_merged$popraceprop <- poprace_us_merged$popsum.x/poprace_us_merged$popsum.y

# remove AK + HI + overseas 
poprace_us_merged$STATEFP <- substr(poprace_us_merged$fips,1,2)
poprace_us_merged <- subset(poprace_us_merged,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79'))
poprace_us_merged <- poprace_us_merged[,c(1,2,4,5)]
names(poprace_us_merged)[3] = 'population'

################## INCLUDE EDU DATA ##################

load('edu_with_sc')
# cut dataset to particular years 
edu_sc <- subset(edu_sc, year >= startyear & year < (startyear+obs.Years))
# remove all US and all county figures as well as overseas, AK, HI
edu_sc$STATEFP <- substr(edu_sc$fips,1,2)
edu_sc$COUNTYFP <- substr(edu_sc$fips,3,5)
edu_sc <- subset(edu_sc,STATEFP != '00')
edu_sc <- subset(edu_sc,COUNTYFP != '000')
edu_sc <- subset(edu_sc,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79','72'))

################# INCLUDE UR DATA ######################
load('urbanpop_with_sc')

# merge by mc
daturban_mctag <- daturban
daturban_mc <- data.frame(dplyr::summarise(group_by(daturban_mctag,fips,year),totpop=sum(totpop),toturban=sum(toturban)))
daturban_mc$urban <- with(daturban_mc,toturban/totpop)

# interpolate between years 
daturbaninterp <- data.frame()
for (cty in unique(daturban_mc$fips)) {
  
  x <- c(2000,2010)
  y <- subset(daturban_mc,fips==cty)$urban
  xout <- c(2001:2009)
  if (sum(is.na(y))==0 ) {
    yinterp <- approx(x,y,xout)$y
  }
  if (sum(is.na(y))>0) {
    y[is.na(y)] <- y[is.finite(y)]
    yinterp <- approx(x,y,xout)$y
  }
  
  dattemp <- data.frame(fips=rep(cty,obs.Years),year=c(startyear:(startyear+obs.Years-1)),urban=numeric(length=obs.Years))
  dattemp[dattemp$year==1999,'urban'] <- y[1]
  dattemp[dattemp$year==2000,'urban'] <- y[1]
  dattemp[dattemp$year==2010,'urban'] <- y[2]
  dattemp[dattemp$year==2011,'urban'] <- y[2]
  dattemp[dattemp$year==2012,'urban'] <- y[2]
  dattemp[dattemp$year==2013,'urban'] <- y[2]
  dattemp[dattemp$year==2014,'urban'] <- y[2]
  dattemp[dattemp$year==2015,'urban'] <- y[2]
  dattemp[dattemp$year==2016,'urban'] <- y[2]
  dattemp[dattemp$year %in% xout,'urban'] <- yinterp
  daturbaninterp <- rbind(daturbaninterp,dattemp)
}
  
################# INCLUDE UNEMPLOYMENT DATA ######################
load('unemployment_with_sc')
dat_unemployment_sc <- subset(dat_unemployment_sc,year >= startyear & year < (startyear + obs.Years))

dat_unemployment_sc$unemp_rate <- dat_unemployment_sc$unemployed_tot/dat_unemployment_sc$labour_force_tot

################## COLLECT ALL PROCESSED COVARIATES ##################

income_us_sc = income_us_sc[,c(1,2,8)]
poppov_us_sc = poppov_us_sc[,c(1,2,3)]
poprace_us_merged = poprace_us_merged
edu_sc = edu_sc[,c(1,2,3)]
daturbaninterp = daturbaninterp
dat_unemployment_sc = dat_unemployment_sc[,c(1,2,5)]

# make expanded grid of years, fips to merge all completely to# load processed wind file
dir.input = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
counties.wind = readRDS(paste0(dir.input,'wind_data_all_',startyear,'_',as.character(startyear+obs.Years-1),'.rds'))
fips_codes = sort(unique(counties.wind$fips))
fips_codes_2 = sort(unique(income_us_sc$fips))
fips_codes = sort(unique(c(fips_codes,fips_codes_2)))
years = c(startyear:(startyear+obs.Years-1))
data_complete = expand.grid(year=years,fips=fips_codes)

# merge all above files together
data_complete = left_join(data_complete, income_us_sc,by=c('year','fips'))
data_complete = left_join(data_complete, poppov_us_sc,by=c('year','fips'))
data_complete = left_join(data_complete, poprace_us_merged,by=c('year','fips'))
data_complete = left_join(data_complete, daturbaninterp,by=c('year','fips'))
data_complete = left_join(data_complete, dat_unemployment_sc,by=c('year','fips'))
data_complete = left_join(data_complete, edu_sc,by=c('year','fips'))

# observe NA values
data_complete_na = data_complete[rowSums(is.na(data_complete))>0,]
data_complete_na = subset(data_complete_na,year<2016)

fips_missing = sort(unique(data_complete_na$fips))

# "04S01"
# "08S01"
# "12037" "12045" "12086" -> "12S01" "12S02"
# "24031" "24033" -> "24S01"
# "30S01"
# "35S01"
# "37031" "37049" -> "37S01"
# "46S01"
# "51003" "51005" "51015" "51019" "51059" "51081" "51083" "51095" "51143" "51153" "51163" "51165" "51175" "51177" "51199" "51515" "51530" "51540" "51590" "51595" "51600" "51620" "51630" "51660" "51683" "51685" "51700" "51790" "51820"
# "51830" "51S01" "51S02" "51S03" "51S04" "51S05" "51S06" "51S07" "51S08" "51S09" "51S10" "51S11" "51S12" "51S13" "51S14" "51S15"


# DATAFRAME SAVE
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/covariates/',startyear,'_',(startyear+obs.Years-1),'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

write.csv(data_complete, paste0(dir.output,'covariates_all_years_',startyear,'_',(startyear+obs.Years-1),'.csv'),row.names = FALSE)

###############################################################
# PREPARING MAP
###############################################################

library('hurricaneexposure'); library('hurricaneexposuredata'); library('tidyverse') ; library('lubridate')

# info for obtaining data
library(dplyr)
all_counties = county_centers %>% pull("fips")

# way of getting out all the states which are included in the dataset
counties.track = county_distance(counties=all_counties, start_year = startyear, end_year = (startyear+obs.Years-1), dist_limit=1000000)
counties.track$state.fips = substr(counties.track$fips,1,2)
state.fips = sort(unique(counties.track$state.fips))

# for theme_map
#devtools::source_gist("33baa3a79c5cfef0f6df")
theme_map <- function(base_size=9, base_family=""){
    require(grid)
    theme_bw(base_size=base_size,base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
    text = element_text(size = 12),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.grid.major = element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.margin=unit(0,"lines"),
    plot.background=element_blank(),
    strip.background = element_blank(),
    # legend.justification = c(0,0),
    legend.position = 'bottom'
    )
}

library(maptools)
library(mapproj)
#library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(sp)
library(plyr)
library(scales)

# load county shapefile map

# load shapefile of entire United States county map
us.national <- readOGR(dsn="~/git/pollution/countries/USA/data/shapefiles/cb_2015_us_county_500k",layer="cb_2015_us_county_500k")

# get projection of shapefile
original.proj = proj4string(us.national)

# load county shapefile map
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

# merge selected data to map dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$GEOID <- as.integer(as.character(USA.df$GEOID))

# merge covariate data with file
data_complete$fips = as.numeric(data_complete$fips)
USA.df.test = merge(USA.df,(subset(data_complete)),by.x='GEOID',by.y='fips',all.X=TRUE)
#USA.df.test[is.na(USA.df.test)] <- 0
USA.df.test <- with(USA.df.test, USA.df.test[order(year,id,order),])

# convert shapefile to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# remove Alaska and Hawaii (don't put back in now)
us_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15"),]

us_aea = fortify(us_aea)

# palette for maps
colorfunc = colorRampPalette(c(brewer.pal(6 , "BrBG" )[1:3],brewer.pal(6 , "RdGy" )[4:6]))
# yearpalette = c('cornsilk',"darkseagreen2","darkseagreen3",'darkseagreen4','dodgerblue','dodgerblue1','dodgerblue2','dodgerblue3','dodgerblue4')
yearpalette = c('cornsilk',"red","darkred","black")

# PLOTS
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/figures/covariates_mapping/',startyear,'_',(startyear+obs.Years-1),'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)

#function to plot various types of maps

plot_covariate = function(fill_covariate, title_covariate, save_covariate){

  # plot first and 2015
  pdf(paste0(dir.output,save_covariate,startyear,'_',(startyear+obs.Years-1),'.pdf'),paper='a4r',width=0,height=0)
  print(ggplot() +
      geom_polygon(data=subset(USA.df.test,year%in%c(startyear,2015)),aes(x=long,y=lat,group=group,fill=get(fill_covariate)),color='black',size=0.001) +
      #geom_polygon(data=subset(USA.df.test,year%in%c(startyear,(startyear+obs.Years-1))),aes(x=long,y=lat,group=group,fill=get(fill_covariate)),color='black',size=0.001) +
      geom_polygon(data=subset(us_aea),aes(x=long,y=lat,group=group),fill=NA,color='black',size=0.2) +
      guides(fill = guide_colorbar(direction = "horizontal", title.position="left",barwidth = 10, barheight = 1,title.vjust = 0.8,
              title = title_covariate)) +
      coord_fixed() +
      facet_wrap(~year,ncol=1) +
      xlab('') +
      ylab('') +
      scale_fill_gradientn(#c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,1),
      colors=yearpalette,limits = c(0,1),guide = guide_legend(nrow = 1),labels = percent) +
      theme_bw() +
      theme_map()
  )
  dev.off()

  # plot all years
  pdf(paste0(dir.output,save_covariate,'all_years_',startyear,'_',(startyear+obs.Years-1),'.pdf'),paper='a4r',width=0,height=0)
  print(ggplot() +
      geom_polygon(data=subset(USA.df.test),aes(x=long,y=lat,group=group,fill=get(fill_covariate)),color='black',size=0.001) +
      geom_polygon(data=subset(us_aea),aes(x=long,y=lat,group=group),fill=NA,color='black',size=0.2) +
      guides(fill = guide_colorbar(direction = "horizontal", title.position="left",barwidth = 10, barheight = 1,title.vjust = 0.8,
              title = title_covariate)) +
      coord_fixed() +
      facet_wrap(~year) +
      xlab('') +
      ylab('') +
      scale_fill_gradientn(#c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,1),
      colors=yearpalette,limits = c(0,1),guide = guide_legend(nrow = 1),labels = percent) +
      theme_bw() +
      theme_map()
  )
  dev.off()

  # plot first year
  pdf(paste0(dir.output,save_covariate,'first_year_',startyear,'_',(startyear+obs.Years-1),'.pdf'),paper='a4r',width=0,height=0)
  print(ggplot() +
      geom_polygon(data=subset(USA.df.test,year==as.numeric(startyear)),aes(x=long,y=lat,group=group,fill=get(fill_covariate)),color='black',size=0.001) +
      geom_polygon(data=subset(us_aea),aes(x=long,y=lat,group=group),fill=NA,color='black',size=0.2) +
      guides(fill = guide_colorbar(direction = "horizontal", title.position="left",barwidth = 10, barheight = 1,title.vjust = 0.8,
              title = title_covariate)) +
      coord_fixed() +
      facet_wrap(~year) +
      xlab('') +
      ylab('') +
      scale_fill_gradientn(#c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,1),
      colors=yearpalette,limits = c(0,1),guide = guide_legend(nrow = 1),labels = percent) +
      theme_bw() +
      theme_map()
  )
  dev.off()

  # plot last year
  pdf(paste0(dir.output,save_covariate,'last_year_',startyear,'_',(startyear+obs.Years-1),'.pdf'),paper='a4r',width=0,height=0)
  print(ggplot() +
      geom_polygon(data=subset(USA.df.test,year==(startyear+obs.Years-1)),aes(x=long,y=lat,group=group,fill=get(fill_covariate)),color='black',size=0.001) +
      geom_polygon(data=subset(us_aea),aes(x=long,y=lat,group=group),fill=NA,color='black',size=0.2) +
      guides(fill = guide_colorbar(direction = "horizontal", title.position="left",barwidth = 10, barheight = 1,title.vjust = 0.8,
              title = title_covariate)) +
      coord_fixed() +
      facet_wrap(~year) +
      xlab('') +
      ylab('') +
      scale_fill_gradientn(#c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,1),
      colors=yearpalette,limits = c(0,1),guide = guide_legend(nrow = 1),labels = percent) +
      theme_bw() +
      theme_map()
  )
  dev.off()

  # plot 2015
  pdf(paste0(dir.output,save_covariate,'2015_',startyear,'_',(startyear+obs.Years-1),'.pdf'),paper='a4r',width=0,height=0)
  print(ggplot() +
      geom_polygon(data=subset(USA.df.test,year==2015),aes(x=long,y=lat,group=group,fill=get(fill_covariate)),color='black',size=0.001) +
      geom_polygon(data=subset(us_aea),aes(x=long,y=lat,group=group),fill=NA,color='black',size=0.2) +
      guides(fill = guide_colorbar(direction = "horizontal", title.position="left",barwidth = 10, barheight = 1,title.vjust = 0.8,
              title = title_covariate)) +
      coord_fixed() +
      facet_wrap(~year) +
      xlab('') +
      ylab('') +
      scale_fill_gradientn(#c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,1),
      colors=yearpalette,limits = c(0,1),guide = guide_legend(nrow = 1),labels = percent) +
      theme_bw() +
      theme_map()
  )
  dev.off()
}


# plot race
plot_covariate('popraceprop', 'Percentage black','covariate_percentage_black_data_')

# plot poverty
plot_covariate('poppov_sc', 'Percentage in poverty','covariate_percentage_poverty_data_')

# plot urban
plot_covariate('urban', 'Percentage urban','covariate_percentage_urban_data_')

# plot unemployed
plot_covariate('unemp_rate', 'Percentage unemployed','covariate_percentage_unemployed_data_')

# plot unemployed
plot_covariate('hsgrad', 'Percentage graduated from at least high school','covariate_percentage_education_data_')

# plot income
#plot_covariate('hsgrad', 'Percentage graduated from at least high school','covariate_percentage_education_data_')