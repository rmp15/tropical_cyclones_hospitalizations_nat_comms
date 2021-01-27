# Figure 1. Tropical cyclone exposure days.
# Number of days with tropical cyclone exposure by county for 1999 – 2014.

rm(list=ls())

# declare directories
project.folder <- paste0(print(here::here()),'/')
data.folder <- paste0(project.folder,'data/')
packages.folder <- paste0(project.folder,'packages/')
output.folder <- paste0(project.folder,'output/')

# load necessary packages
source(paste0(packages.folder,'packages_to_load.R'))

# years of study
start_year <- 1999; end_year <- 2014

# load processed tropical cyclone exposure file by days of exposure per county
dat.event.sum <- read.csv(paste0(data.folder,'figure_1_events_by_county_',start_year,'_',end_year,'.csv'))

# process United States map
source(paste0(project.folder,'map_create/map_create.R'))

# merge tropical cyclone exposure data with map file and prepare for plotting in ggplot
USA.df.merged <- merge(USA.df,dat.event.sum,by.x='GEOID',by.y='fips',all.x=TRUE)
USA.df.merged[is.na(USA.df.merged)] <- 0
USA.df.merged <- with(USA.df.merged, USA.df.merged[order(id,order),])

# color palette for map
colorpalette <- c('cornsilk',"blue","darkblue","black")

# save map plot output for Figure 1
pdf(paste0(output.folder,'figure_1.pdf'),paper='a4r',width=0,height=0)
print(ggplot() +
    geom_polygon(data=subset(USA.df.merged),aes(x=long,y=lat,group=group,fill=cases),color='black',size=0.001) +
    geom_polygon(data=subset(us_aea),aes(x=long,y=lat,group=group),fill=NA,color='black',size=0.2) +
    guides(fill = guide_colorbar(direction = "horizontal", title.position="left",barwidth = 10, barheight = 1,title.vjust = 0.8,
            title = "Number of days of tropical cyclone exposure")) +
    coord_fixed() +
    xlab('') +
    ylab('') +
    scale_fill_gradientn(c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.7,1),
    colors=colorpalette,breaks=seq(0,20,5), limits = c(0,20),guide = guide_legend(nrow = 1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),#text = element_text(size = 10),
        axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),legend.text=element_text(size=15),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),legend.justification='center',
        legend.position = 'bottom', legend.background = element_rect(fill="white", size=.5, linetype="dotted")) +
    theme_map()
)
dev.off()