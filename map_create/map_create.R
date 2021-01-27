# info for obtaining data on counties to map
all_counties = county_centers %>% pull("fips")

# info for obtaining data on states to map
counties.track = county_distance(counties=all_counties, start_year = start_year, end_year = end_year, dist_limit=1000000)
counties.track$state.fips = substr(counties.track$fips,1,2)
state.fips = sort(unique(counties.track$state.fips))

# for map theme to plot in ggplot
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

### 1. load and process us county shapefile for ggplot

# load shapefile of entire United States by county
us.national <- readOGR(dsn=paste0(data.folder,"shapefiles/cb_2015_us_county_500k"),layer="cb_2015_us_county_500k")

# reproject shapefile
us.national <- spTransform(us.national, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

# only keep US States which are in the analysis
us.main <- us.national[us.national$STATEFP %in% state.fips,]

# fortify to prepare for plotting in ggplot
map <- fortify(us.main)

# extract data from shapefile
us.main@data$id <- rownames(us.main@data)
shapefile.data <- us.main@data

# merge selected data to map_create dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$GEOID <- as.integer(as.character(USA.df$GEOID))

### 2. load and process us state shapefile for ggplot

# also load shapefile of entire United States by state
us <- readOGR(dsn=paste0(data.folder,"shapefiles/states"),layer="states")

# reproject shapefile
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# only keep mainland US States
us_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15"),]

us_aea = fortify(us_aea)
