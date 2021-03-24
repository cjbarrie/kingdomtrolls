library(tidyverse)
library(rtweet)

#OPTION 1: USE HYDRATOR FROM https://github.com/DocNow/hydrator
#TAKE TWEET IDs FROM data/raw/ and hydrate to get SA, GEO, NEWS, and TOP samples


#OR OPTION 2:
api_key <-"ENTER>api_key"
api_key_secret <- "ENTER>api_key_secret"
access_token <- "ENTER>access_token"
access_token_secret <- "ENTER>access_token_secret"

token <- create_token(
  app = "IO tweets",
  consumer_key = api_key,
  consumer_secret = api_key_secret,
  access_token = access_token,
  access_secret = access_token_secret
)

SAIDs <- read.table("data/replication_tweetIDs/SAusertweets19_allIDs.txt")
SAusertweets19_all <- lookup_statuses(SAIDs)

save(SAusertweets19_all, file = "data/analysis/SAusertweets19_all.RData")

#then repeat for GEO, NEWS, and TOP IDs

###########################################

#FOR IO TWEETS, TAKE IDS FROM  data/raw/IOusertweets19_allIDs.txt
#read in IO tweets from https://transparency.twitter.com/en/reports/information-operations.html for Saudi Arabia
#datasets stored as "sa_eg_ae_022020_tweets_csv_unhashed/" for Release 2 and "saudi_arabia_112019_tweets_csv_unhashed/" for Release 1
#once imported, identify relevant tweets with e.g.:

IOIDs <- read.table("data/replication_tweetIDs/IOusertweets19_allIDs.txt")

#GET ALL R1 TWEETS
files <- list.files(path=file.path("/PATH/TO/IO_tweets_file/saudi_arabia_112019_tweets_csv_unhashed/"),recursive=T,include.dirs=T)
files <- paste("/PATH/TO/IO_tweets_file/saudi_arabia_112019_tweets_csv_unhashed/", files, sep="")


IOtweets_r1 <- data.frame()
for (i in seq_along(files)) {
  filename = files[[i]]
  cat("Ingesting filename:", filename, "\n")
  tweets <- read.csv(filename)
  tweets$date <- as.Date(tweets$tweet_time)
  tweets <- tweets %>%
    filter(tweetid %in% IOIDs)
  IOtweets_r1 <- rbind(IOtweets_r1, tweets)
}


#GET ALL R2 TWEETS
files <- list.files(path=file.path("/PATH/TO/IO_tweets_file/sa_eg_ae_022020_tweets_csv_unhashed/"),recursive=T,include.dirs=T)
files <- paste("/PATH/TO/IO_tweets_file/sa_eg_ae_022020_tweets_csv_unhashed/", files, sep="")

IOtweets_r2 <- data.frame()
for (i in seq_along(files)) {
  filename = files[[i]]
  cat("Ingesting filename:", filename, "\n")
  tweets <- read.csv(filename)
  tweets$date <- as.Date(tweets$tweet_time)
  tweets <- tweets %>%
    filter(tweetid %in% IOIDs)
  IOtweets_r2 <- rbind(IOtweets_r2, tweets)
}

# REMOVE poll_choices COLUMN FROM R2 FOR RBIND
IOtweets19_r2 <- IOtweets19_r2[,-31]
# BIND
IOtweets19_all <- rbind(IOtweets19_r1, IOtweets19_r2)   

# REMOVE ANY DUPLICATE TWEETS
IOtweets19_all <- IOtweets19_all %>%
  distinct(tweetid, .keep_all = TRUE)

save(IOtweets19_all, file = "data/analysis/IOtweets19_all.RData")