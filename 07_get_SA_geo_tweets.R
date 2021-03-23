library(tidyverse)
library(rgdal)
library(dplyr)
library(sp)

#Read in Data
geo_tweets<-read_csv("data/output/saudi_geo_2019.csv")

#Does their self-reported location suggest they're in Saudi? 
locations<-as.data.frame(table(geo_tweets$user.location))

#Code top non-Saudi locations manually
write_csv(locations, "data/output/top_locations_geo.csv")

locations_coded<-read_csv("data/output/top_locations_geo_coded.csv")

not_saudi<-subset(locations_coded, not_saudi==TRUE)

#Find not-Saudi Tweets
geo_tweets$not_saudi<-ifelse(geo_tweets$user.location %in% not_saudi$Var1, 1,0)
table(geo_tweets$not_saudi)

geo_tweets_saudi<-subset(geo_tweets, not_saudi==0)

write_csv(geo_tweets_saudi, "data/output/geo_tweets_saudi_only.csv")

#How many unique users?
users<-as.data.frame(table(geo_tweets_saudi$user.screen_name))
#75,577

#Save Workspace 
rm(geo_tweets, locations, locations_coded, not_saudi, users)

#Get points shapefile to restrict in QGIS to Saudi borders
coords <- geo_tweets_saudi %>%
  select(coordinates.coordinates.0, coordinates.coordinates.1)

userids <- geo_tweets_saudi %>%
  select(id_str)

points <- SpatialPointsDataFrame(coords = coords, data = userids,
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# generate spatial points shapefile of all geo-located users with user_id attached
writeOGR(points, dsn="data/shapefiles/", layer="SAgeopoints", driver="ESRI Shapefile")

#restrict in QGIS to IDs of tweets within SA border

#filter geo tweets by users inside Saudi borders
geo_ids_clipped <- read.csv("data/output/geo_tweets_users_clipped.csv")
geo_ids_clipped <- geo_ids_clipped[,1]
GEOusertweets19 <- geo_tweets_saudi %>%
  filter(id_str %in% geo_ids_clipped)

GEOusertweets19$date <- substr(GEOusertweets19$created_at, 5, 10)
GEOusertweets19$year <- substr(GEOusertweets19$created_at, 27, 31)
GEOusertweets19$date <- paste(GEOusertweets19$date, GEOusertweets19$year, sep=" ")
GEOusertweets19$date <- as.Date(GEOusertweets19$date, format = "%b %d %Y")
GEOusertweets19$year <- year(GEOusertweets19$date)

save(GEOusertweets19, file= "data/analysis/GEOusertweets19.RData")

#get points spatial object of geo clipped tweets
coords <- geo_tweets_clipped %>%
  select(coordinates.coordinates.0, coordinates.coordinates.1)

tweetids <- geo_tweets_clipped %>%
  select(id_str)

#get SpatialPoints df of geo clipped users
points <- SpatialPointsDataFrame(coords = coords, data = tweetids,
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#test
plot(points)
# generate spatial points shapefile of geo clipped users with user_id attached
writeOGR(points, dsn="data/shapefiles/", layer="SAgeopoints_clipped", driver="ESRI Shapefile")

#generate csv of unique geoclipped users
options(scipen = 999)
geo_users_clipped <- unique(geo_tweets_clipped$user.id_str)
#34,000 unique users
write.csv(geo_users_clipped, "data/output/geo_users_clipped.csv")
save(geo_users_clipped, file = "data/output/geo_users_clipped.RData")