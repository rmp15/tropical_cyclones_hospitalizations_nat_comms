# this code will take the coastal storm data from Brooke's package
# then process it according to our needs
rm(list=ls())

# install storm data packages if necessary
# library(drat)
# addRepo("geanders")
# library("devtools") ; install_github("geanders/hurricaneexposure")

# load necessary packages
library('hurricaneexposure')
library('hurricaneexposuredata')
library('tidyverse')
library(lubridate)

# info for obtaining data
all_counties = county_centers %>% pull("fips")
start_year = 1999
end_year = 2014

# way of getting out all the states which are included in the dataset
counties.track = county_distance(counties=all_counties, start_year = start_year, end_year = end_year, dist_limit=1000000)
counties.track$state.fips = substr(counties.track$fips,1,2)
state.fips = sort(unique(counties.track$state.fips))

# (problem with all methods below is the counties do not appear more than once, and once only using this method)

# 1. DISTANCE FROM BEST TRACK
counties.track = county_distance(counties=all_counties, start_year = start_year, end_year = end_year, dist_limit=20)

# 2. RAINFALL
counties.rain  = county_rain(counties=all_counties, start_year = start_year, end_year = end_year, rain_limit=0.1, dist_limit=1000000)

# 3. WIND
counties.wind  = county_wind(counties=all_counties, start_year = start_year, end_year = end_year, wind_limit=0)

# 4. EVENTS FROM NOAA (
counties.flood  = county_events(counties=all_counties, event_type='flood', start_year = start_year, end_year = end_year)
counties.tornado  = county_events(counties=all_counties, event_type='tornado', start_year = start_year, end_year = end_year)
counties.wind  = county_events(counties=all_counties, event_type='wind', start_year = start_year, end_year = end_year)

# DO THIS AS BINARY FIRST OF ALL FOR ISEE
counties.tropical.storm  = county_events(counties=all_counties, event_type="tropical_storm", start_year = start_year, end_year = end_year)

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

# add date info in separate columns and sort
# counties.track = add_date_info(counties.track)
# counties.rain = add_date_info(counties.rain)
# counties.wind = add_date_info(counties.wind)
# counties.flood = add_date_info(counties.flood)

# load entire wind and rain data if desired (no date just lags so not sure how useful these are!)
data("storm_winds") ; head(storm_winds)
data("rain") ; head(rain)

# Complete information by combining rain lags with max rain value

# 2. rainfall
counties.rain.complete = merge(counties.rain, rain, by=c('storm_id','fips'),all.x=TRUE)
counties.rain.complete$date = with(counties.rain.complete, as.Date(local_time)+lag)
counties.rain.complete$closest_time_utc = NULL
counties.rain.complete$closest_date = NULL
counties.rain.complete$storm_dist = NULL
counties.rain.complete$tot_precip = NULL
counties.rain.complete$local_time = NULL

# make complete grid of exposures







