library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

#Read in top_accounts
top_accounts<-read_csv("data/output/SA_top_accounts_all_tweets.csv")

#Remove empty rows 
top_accounts<-top_accounts[!is.na(top_accounts$body),]

#Fix Long Tweet Text
top_accounts$text_l<-NA
top_accounts$text_l[is.na(top_accounts$long_object.body)]<-top_accounts$body[is.na(top_accounts$long_object.body)] 
top_accounts$text_l[!is.na(top_accounts$long_object.body)]<-top_accounts$long_object.body[!is.na(top_accounts$long_object.body)] 
top_accounts$text_l[!is.na(top_accounts$object.long_object.body)]<-paste(sapply(strsplit(top_accounts$body[is.na(top_accounts$object.long_object.body)==F],":"), `[`, 1), ": ", top_accounts$object.long_object.body[is.na(top_accounts$object.long_object.body)==F], sep="")

#Fix Date
top_accounts$date<-as.Date(top_accounts$object.postedTime, format="%a %b %d %H:%M:%S %z %Y")

#Get IDs to rehydrate for accurate engagement counts

#Clean ID Variable
top_accounts$id_original<-top_accounts$id
top_accounts<-top_accounts %>% separate(id, c("id2", "id3"), sep="2005:")
top_accounts$id_fixed<-top_accounts$id3
top_accounts$id_fixed<-ifelse(is.na(top_accounts$id_fixed), top_accounts$id_original, top_accounts$id_fixed)
top_accounts$id<-top_accounts$id_fixed

write_csv(as.data.frame(top_accounts$id), "data/output/top_account_ids.csv")

#Use Hydrator (https://github.com/DocNow/hydrator) to rehydrate tweets

#Read in hydrated tweets
hydrated<-read_csv("data/output/top_saudi_accounts_rehydrated_tweets.csv")

summary(hydrated$retweet_count)
summary(hydrated$favorite_count)

#Save File for Analysis
hydrated$date <- substr(hydrated$created_at, 5, 10)
hydrated$year <- substr(hydrated$created_at, 27, 31)
hydrated$date <- paste(hydrated$date, hydrated$year, sep=" ")
hydrated$date <- as.Date(hydrated$date, format = "%b %d %Y")
hydrated$year <- year(hydrated$date)

#Split top accounts into NEWS and TOP
naccounts <- read.csv("data/output/SA_news_accounts.csv")
naccounts <- naccounts$SA_news_accounts

NEWSusertweets19_all <- hydrated %>%
  filter(year==2019,
         user_screen_name %in% naccounts)

TOPusertweets19_all <- hydrated %>%
  filter(year==2019,
         !user_screen_name %in% naccounts)
  
save(TOPusertweets19_all,file= "data/analysis/TOPusertweets19_all.RData")
save(NEWSusertweets19_all,file="data/analysis/NEWSusertweets19_all.RData")
