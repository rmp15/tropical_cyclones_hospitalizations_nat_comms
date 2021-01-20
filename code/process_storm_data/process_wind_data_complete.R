# this code will take the coastal storm data from Brooke's package
# then process it for a model input

rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
start_year <- as.numeric(args[1])
end_year = as.numeric(args[2])

# install storm data packages if necessary
library(drat)
addRepo("geanders")
# library("devtools") ; install_github("geanders/hurricaneexposure")
# install.packages('hurricaneexposure'); install.packages('hurricaneexposuredata')

# load necessary packages
library('hurricaneexposure'); library('hurricaneexposuredata'); library('tidyverse') ; library('lubridate')

# load entire wind data if desired (no date just lags so not sure how useful these are!)
data("storm_winds") ; head(storm_winds)

# info for obtaining data
all_counties = county_centers %>% pull("fips") # 3221 counties in total

# way of getting out all the states which are included in the dataset
counties.track = county_distance(counties=all_counties, start_year = start_year, end_year = end_year, dist_limit=1000000)
counties.track$state.fips = substr(counties.track$fips,1,2)
state.fips = sort(unique(counties.track$state.fips)) # 33 states and Washington DC

# WIND
counties.wind  = county_wind(counties=all_counties, start_year = start_year, end_year = end_year, wind_limit=0)

# Categorise wind by following characteristics
# Tropical depression: up to        17.4346 m/s     (38 mph)
# Tropical storm: up to             33.081 m/s      (74 mph)
# Hurricane Cat 1: up to            42.9158 m/s     (96 mph)
# Hurricane Cat 2: up to            49.6214 m/s     (111 mph)
# Hurricane Cat 3: up to            58.1152 m/s     (139 mph)
# Hurricane Cat 4: up to            70.1853 m/s     (157 mph)
# Hurricane Cat 5: above Cat 4

counties.wind$cat_sust = with(counties.wind,
                            ifelse(vmax_sust<17.4346,   'tropical depression',
                            ifelse(vmax_sust<33.081,    'coastal storm',
                            ifelse(vmax_sust<42.9158,   'hurricane cat 1',
                            ifelse(vmax_sust<49.6214, 'hurricane cat 2',
                            ifelse(vmax_sust<58.1152, 'hurricane cat 3',
                            ifelse(vmax_sust<70.1853, 'hurricane cat 4',
                            'hurricane cat 5'
                            ))))))
                            )

# function to add year, month, and day to dataset from lubridate function
add_date_info = function(dat){
    dat$year     = lubridate::year(dat$closest_date)
    dat$month    = lubridate::month(dat$closest_date)
    dat$day      = lubridate::day(dat$closest_date)

    dat = dat[
      with(dat, order(year,month,day)),
    ]

    return(dat)
}

counties.wind = add_date_info(counties.wind)

# save all county-days including just tropical depressions
counties.wind.all = counties.wind

# save only coastal storms and above
counties.wind = subset(counties.wind,cat_sust!='tropical depression')

# make fips numeric
counties.wind$fips = as.numeric(counties.wind$fips)

# TREATING EVERYTHING ABOVE TROPICAL DEPRESSION THE SAME

# add lag to the date before matching
# function to add year, month, and day to dataset from lubridate function
add_date_info = function(dat, lag_chosen){
    dat$closest_date = as.Date(dat$closest_date, format="%Y-%m-%d") + lag_chosen
    dat$year     = lubridate::year(dat$closest_date)
    dat$month    = lubridate::month(dat$closest_date)
    dat$day      = lubridate::day(dat$closest_date)

    col_name     = paste0('event_lag',lag_chosen)
    dat[col_name]= 1

    dat = dat[
      with(dat, order(year,month,day)),
    ]

    dat = dat[,c('fips','year','month','day',col_name)]

    return(dat)
}

# lags to have included
lags=c(0:7)

# calculate lag values for the unconstrained lag model
for(lag in lags){
    assign(paste0('counties.wind.edit.',lag), add_date_info(counties.wind, lag))
    print(head(get(paste0('counties.wind.edit.',lag))))

}

# add multiple lags to array for all fips codes in dataset
dates = seq(as.Date("1988-01-01"), as.Date("2018-12-31"), by="days")
fipscounty = as.numeric(all_counties)
complete.grid = expand.grid(date=dates,fips=fipscounty)
complete.grid$year = format(complete.grid$date, '%Y') ; complete.grid$year = as.numeric(complete.grid$year)
complete.grid$month = as.numeric(format(complete.grid$date, '%m'))
complete.grid$day = as.numeric(format(complete.grid$date, '%d'))
complete.grid$date = NULL
counties.wind.edit.array = complete.grid
for(lag in lags){
    print(paste0('Matching lag ',lag))
    counties.wind.edit.array = merge(counties.wind.edit.array,get(paste0('counties.wind.edit.',lag)),by=c('fips','year','month','day'), all.x=TRUE)
}
counties.wind.edit.array[is.na(counties.wind.edit.array)] <- 0

# save as rds for analysis
dir.output = paste0('~/git/rmparks_coastal_storms_Jan_2020/data/coastal_storm_data/')
ifelse(!dir.exists(dir.output), dir.create(dir.output, recursive=TRUE), FALSE)
saveRDS(counties.wind, paste0(dir.output,'wind_data_',start_year,'_',end_year,'.rds'))
saveRDS(counties.wind.all, paste0(dir.output,'wind_data_all_',start_year,'_',end_year,'.rds'))
saveRDS(counties.wind.edit.array, paste0(dir.output,'wind_data_lag_array_',start_year,'_',end_year,'.rds'))

# TREATING EVERYTHING ABOVE TROPICAL DEPRESSION AS EITHER COASTAL STORM OR HURRICANE (TO FINISH)

# add lag to the date before matching
# function to add year, month, and day to dataset from lubridate function
add_date_info = function(dat, lag_chosen){
    dat$closest_date = as.Date(dat$closest_date, format="%Y-%m-%d") + lag_chosen
    dat$year     = lubridate::year(dat$closest_date)
    dat$month    = lubridate::month(dat$closest_date)
    dat$day      = lubridate::day(dat$closest_date)

    # separate out activation for coastal storms of hurricanes
    col_name     = paste0('coastal_storm_event_lag',lag_chosen)
    dat[col_name]= ifelse(dat$cat_sust=='coastal storm',1,0)

    col_name_2     = paste0('hurricane_event_lag',lag_chosen)
    dat[col_name_2]= ifelse(dat$cat_sust%in%c("hurricane cat 1", "hurricane cat 2", "hurricane cat 3"),1,0)

    dat = dat[
      with(dat, order(year,month,day)),
    ]

    dat = dat[,c('fips','year','month','day',col_name,col_name_2)]

    return(dat)
}

# calculate lag values for the unconstrained lag model
for(lag in lags){
    assign(paste0('counties.wind.edit.separate.',lag), add_date_info(counties.wind, lag))
    print(head(get(paste0('counties.wind.edit.separate.',lag))))

}

# add multiple lags to array for all fips codes in dataset
dates = seq(as.Date("1999-01-01"), as.Date("2016-12-31"), by="days")
fipscounty = as.numeric(all_counties)
complete.grid = expand.grid(date=dates,fips=fipscounty)
complete.grid$year = format(complete.grid$date, '%Y') ; complete.grid$year = as.numeric(complete.grid$year)
complete.grid$month = as.numeric(format(complete.grid$date, '%m'))
complete.grid$day = as.numeric(format(complete.grid$date, '%d'))
complete.grid$date = NULL
counties.wind.edit.array = complete.grid
for(lag in lags){
    print(paste0('Matching lag ',lag))
    counties.wind.edit.array = merge(counties.wind.edit.array,get(paste0('counties.wind.edit.separate.',lag)),by=c('fips','year','month','day'), all.x=TRUE)
}
counties.wind.edit.array[is.na(counties.wind.edit.array)] <- 0

# save as rds for analysis
saveRDS(counties.wind.edit.array, paste0(dir.output,'wind_data_lag_array_hurricane_separate_',start_year,'_',end_year,'.rds'))
