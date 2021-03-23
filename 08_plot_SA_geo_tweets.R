library(tidyverse)
library(rgdal)
library(sf)
library(ggthemes)

#import sf objects of geo clipped points and Saudi borders
points_sf <- st_read("data/shapefiles/SAgeopoints_clipped.shp")
borders_sf <- st_read("data/shapefiles/sau_adm0/SAU_adm0.shp")

#plot
ggplot() +
  geom_sf(data = borders_sf, fill =  "#faf8f3", size = .3, alpha=.3) +
  geom_sf(data = points_sf,alpha = .3, size=2, col="darkgreen") +
  theme_tufte(base_family = "Helvetica") +
  theme(axis.text.y=element_text(size=30, face="bold"),
        axis.text.x=element_text(size=30, face="bold"))

ggsave("data/output/plots/geo_tweets.png", width=800, height = 800, dpi=100, units="mm")
