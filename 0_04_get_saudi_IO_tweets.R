library(tidyverse)
library(gdata)
library(lubridate)
library(ggthemes)
library(xtable)

################################################# INGEST R2 RELEASE

rm(list = ls())
files <- list.files(path=file.path("/PATH/TO/IO_tweets_file/sa_eg_ae_022020_tweets_csv_unhashed/"),recursive=T,include.dirs=T)
files <- paste("/PATH/TO/IO_tweets_file/sa_eg_ae_022020_tweets_csv_unhashed/", files, sep="")

tweets_all <- data.frame()
for (i in seq_along(files)) {
  filename = files[[i]]
  cat("Ingesting filename:", filename, "\n")
  tweets <- read.csv(filename)
  tweets <- tweets[,c(1,5, 14)] #take just tweet ID and reported location and date in first ingest to save memory
  tweets_all <- rbind(tweets_all, tweets)
}

#GET DF OF TWEETS WITH NON-MISSING LOCATION

tweets_with_location <- tweets_all %>%
  filter(user_reported_location!= " " & 
           user_reported_location!= "" &
           user_reported_location!= ".")

# SCRUTINIZE REPORTED LOCATIONS
location <- tweets_all$user_reported_location
unique(location)
# FILTER BY SA-LOCATED TWEETS
tweet_IDs_SA <- tweets_all %>%
  filter(grepl("الظهران|Saudi|جدة|Jeddah|السعودية|K.S.A|السعوديه|Riyadh|السعود| الرياض|الريآض|مكة|جده|KSA|القصيم|الاحساء", 
               user_reported_location))

# CHECK LOCATIONS ARE ALL IN SA
location <- tweet_IDs_SA$user_reported_location
unique(location)
# GET TWEET IDS TO FILTER BY IN SECOND INGEST
filterIDs <- tweet_IDs_SA$tweetid

# STORE AGGEGRATE STATISTICS FOR FULL DATE-RANGE OF SAMPLE
aggiostats2 <- data.frame(`IO dataset` = "sa_eg_ae_022020",
                          `N tweets` = nrow(tweets_all),
                          `N tweets w/ location` = nrow(tweets_with_location),
                          `N tweets in Saudi` = nrow(tweet_IDs_SA),
                          `% with location` = (nrow(tweets_with_location)/nrow(tweets_all))*100,
                          `% with location in Saudi Arabia` = (nrow(tweet_IDs_SA)/nrow(tweets_all))*100)

write.table(aggiostats2, file = "data/output/aggiostats2.txt")
tab <- print.xtable(xtable(aggiostats2),include.rownames=F,
             sanitize.colnames.function=function(x)gsub("\\."," ",x),
             file = "data/output/aggiostats2.tex")

# SAVE AGGEGRATE STATISTICS FOR 2019

tweets_all$date <- as.Date(tweets_all$tweet_time)
tweets_all$year <- year(tweets_all$date)

tweets_all_19 <- tweets_all %>% 
  filter(year == 2019)
ntweets_all_19 <- nrow(tweets_all_19)

tweets_with_location_19 <- tweets_all_19 %>%
  filter(user_reported_location!= " " & 
           user_reported_location!= "" &
           user_reported_location!= ".")
ntweets_with_location_19 <- nrow(tweets_with_location_19)


tweets_all_SA_filtered <- data.frame()
for (i in seq_along(files)) {
  filename = files[[i]]
  cat("Ingesting filename:", filename, "\n")
  tweets <- read.csv(filename)
  tweets <- tweets %>%
    filter(tweetid %in% filterIDs)
  tweets_all_SA_filtered <- rbind(tweets_all_SA_filtered, tweets)
}

# CHECK LOCATIONS OF FILTERED TWEETS AGAIN
location <- tweets_all_SA_filtered$user_reported_location
ulocations <- unique(location)
ulocations

# SOME LOCATIONS INCORRECTLY INCLUDED DUE TO DUPLICATE TWEETIDs
wronglocations <- ulocations[c(24,28, 33, 35:37,40:42, 44:52,53,56,59,60, 63,64, 65,67:69)]
wronglocations

tweets_all_SA_filtered20 <- tweets_all_SA_filtered %>%
  filter(!user_reported_location %in% wronglocations)

# FINAL CHECK ON LOCATION FILTER
location <- tweets_all_SA_filtered20$user_reported_location
SAlocations <- unique(location)
SAlocations

# GET TWEETS JUST FOR 2019
tweets_all_SA_filtered20$date <- as.Date(tweets_all_SA_filtered20$tweet_time)
tweets_all_SA_filtered20$year <- year(tweets_all_SA_filtered20$date)

IOtweets19_r2 <- tweets_all_SA_filtered20 %>%
  filter(year==2019)

#SAVE SA TWEETS 2019 R2 RELEASE
save(IOtweets19_r2, file = "data/output/IOtweets19_r2.RData")

#SAVE SA ALL TWEETS R2 RELEASE
IOtweets_r2 <- tweets_all_SA_filtered20 %>%
  select(tweetid, userid, tweet_text, follower_count, following_count, date,
         user_mentions, user_reported_location)
save(IOtweets_r2, file = "data/output/IOtweets_r2.RData")

# STORE AGGEGRATE STATISTICS FOR 2019 R2

aggiostats219 <- data.frame(`IO dataset` = "sa_eg_ae_022020 (2019)",
                            `N tweets` = ntweets_all_19,
                            `N tweets w/ location` = nrow(tweets_with_location_19),
                            `N tweets in Saudi` = nrow(IOtweets19_r2),
                            `% with location` = (ntweets_with_location_19/ntweets_all_19)*100,
                            `% with location in Saudi Arabia` = (nrow(IOtweets19_r2)/ntweets_all_19)*100)

write.table(aggiostats219, file = "data/output/aggiostats219.txt")
tab <- print.xtable(xtable(aggiostats219),include.rownames=F,
                    sanitize.colnames.function=function(x)gsub("\\."," ",x),
                    file = "data/output/aggiostats219.tex")

################################################# INGEST R1 RELEASE

rm(list = ls())
# GET saudi_arabia_112019_tweets_csv_unhashed DATA
files <- list.files(path=file.path("/PATH/TO/IO_tweets_file/saudi_arabia_112019_tweets_csv_unhashed/"),recursive=T,include.dirs=T)
files <- paste("/PATH/TO/IO_tweets_file/saudi_arabia_112019_tweets_csv_unhashed/", files, sep="")

#GET ALL R1 TWEETS
IOtweets_r1 <- data.frame()
for (i in seq_along(files)) {
  filename = files[[i]]
  cat("Ingesting filename:", filename, "\n")
  tweets <- read.csv(filename)
  # tweets <- tweets[,c(1,2,4,5,8,9,10,13,14,24:27)] # select columns to reduce file size
  tweets$date <- as.Date(tweets$tweet_time)
  tweets$year <- year(tweets$date)
  tweets <- tweets %>%
    select(tweetid, userid, tweet_text, follower_count, following_count, date,
           user_mentions, user_reported_location)
  IOtweets_r1 <- rbind(IOtweets_r1, tweets)
}

save(IOtweets_r1, file = "data/output/IOtweets_r1.RData")
rm(tweets)

tweets_with_location <- IOtweets_r1 %>%
  filter(user_reported_location!= " " & 
           user_reported_location!= "" &
           user_reported_location!= "." & 
           user_reported_location != "،")

# STORE AGGEGRATE STATISTICS FOR R1

aggiostats1 <- data.frame(`IO dataset` = "saudi_arabia_112019",
                          `N tweets` = nrow(IOtweets_r1),
                          `N tweets w/ location`= nrow(tweets_with_location),
                          `% with location` = (nrow(tweets_with_location)/nrow(IOtweets_r1))*100)

write.table(aggiostats1, file = "data/output/aggiostats1.txt")
tab <- print.xtable(xtable(aggiostats1),include.rownames=F,
                    sanitize.colnames.function=function(x)gsub("\\."," ",x),
                    file = "data/output/aggiostats1.tex")


#LOAD R2 SA ALL TWEETS
load("data/output/IOtweets_r2.RData")

IOtweets_r1$release <- "r1"
IOtweets_r2$release <- "r2"

IOtweets_SA_all<- rbind(IOtweets_r1, IOtweets_r2)

#SAVE R1 AND R2 SA ALL TWEETS COMBINED
IOtweets_SA_all <- IOtweets_SA_all %>%
  distinct(tweetid, .keep_all = TRUE)

save(IOtweets_SA_all, file = "data/analysis/IOtweets_SA_all.RData")
  
#GET FULL TWEET SET FOR R1

rm(list = ls())
# GET saudi_arabia_112019_tweets_csv_unhashed DATA
files <- list.files(path=file.path("/PATH/TO/IO_tweets_file/saudi_arabia_112019_tweets_csv_unhashed/"),recursive=T,include.dirs=T)
files <- paste("/PATH/TO/IO_tweets_file/saudi_arabia_112019_tweets_csv_unhashed/", files, sep="")


IOtweets19_r1 <- data.frame()
for (i in seq_along(files)) {
  filename = files[[i]]
  cat("Ingesting filename:", filename, "\n")
  tweets <- read.csv(filename)
  tweets$date <- as.Date(tweets$tweet_time)
  tweets$year <- year(tweets$date)
  tweets <- tweets %>%
    filter(year==2019)
  IOtweets19_r1 <- rbind(IOtweets19_r1, tweets)
}

save(IOtweets19_r1, file = "data/output/IOtweets19_r1.RData")

# STORE AGGEGRATE STATISTICS FOR R1 2019 

tweets_with_location_19 <- IOtweets19_r1 %>%
  filter(user_reported_location!= " " & 
           user_reported_location!= "" &
           user_reported_location!= "." & 
           user_reported_location != "،")


aggiostats119 <- data.frame(`IO dataset` = "saudi_arabia_112019 (2019)",
                          `N tweets` = nrow(IOtweets19_r1),
                          `N tweets w/ location`= nrow(tweets_with_location_19),
                          `% with location` = (nrow(tweets_with_location_19)/nrow(IOtweets19_r1))*100)

write.table(aggiostats119, file = "data/output/aggiostats119.txt")
tab <- print.xtable(xtable(aggiostats119),include.rownames=F,
                    sanitize.colnames.function=function(x)gsub("\\."," ",x),
                    file = "data/output/aggiostats119.tex")


# COMBINE R1 AND R2 RELEASES AND DELETE ANY DUPLICATES
load("data/output/IOtweets19_r2.RData")

# REMOVE poll_choices COLUMN FROM R2 FOR RBIND
IOtweets19_r2 <- IOtweets19_r2[,-31]
# BIND
IOtweets19_all <- rbind(IOtweets19_r1, IOtweets19_r2)   

# REMOVE ANY DUPLICATE TWEETS
IOtweets19_all <- IOtweets19_all %>%
  distinct(tweetid, .keep_all = TRUE)

save(IOtweets19_all, file = "data/analysis/IOtweets19_all.RData")

#SAVE AGGREGATE STATISTICS FOR FINAL 2019 SAMPLE
aggiostats <- data.frame(`IO dataset` = "IO_final",
                         `N tweets` = nrow(IOtweets19_all),
                         `N accounts` = length(unique(IOtweets19_all$userid)))

write.table(aggiostats, file = "data/output/aggiostats.txt")
tab <- print.xtable(xtable(aggiostats),include.rownames=F,
                    sanitize.colnames.function=function(x)gsub("\\."," ",x),
                    file = "data/output/aggiostats.tex")

#PLOT OVER TIME
load("data/analysis/IOtweets_SA_all.RData")

users19 <- unique(IOtweets19_all$userid)

datmin <- c(as.Date("2019-01-01"))
datmax <- c(as.Date("2020-01-01"))

IOtweets_SA_all %>%
  filter(userid %in% users19) %>% #only include accounts active in 2019
  mutate(obs=1) %>%
  group_by(date) %>%
  summarise(sum_tweets = sum(obs)) %>%
  ggplot(aes(date, sum_tweets)) +
  geom_line(alpha=0.6 , size=.8, colour="black", group=1) +
  annotate("rect",fill = "#b80000", alpha = .2,
           xmin = datmin, xmax = datmax, 
           ymin = 0, ymax = 46000) +
  annotate("text", x = as.Date("2019-06-01"), 
           y = 47500, label = "2019, \n n = 9.8m", 
           size=7, fontface="bold") +
  ylab("# IO tweets") +
  xlab("Date") + 
  scale_x_date(date_labels = "%b %Y") +
  theme_tufte(base_family = "Helvetica") +
  theme(axis.text.y=element_text(size=30),
        axis.text.x=element_text(size=30),
        axis.title.x=element_text(size=30, face="bold"),
        axis.title.y=element_text(size=30, face="bold"))

ggsave("data/output/plots/iotpd.png", width=600, height = 400, dpi=200, units="mm")