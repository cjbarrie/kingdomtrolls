library(tidytext)
library(dplyr)
library(stringr)
library(readr)
library(xtable)

#Load Datasets
load("data/analysis/SAusertweets19_all.RData")
load("ata/analysis/GEOusertweets19.RData")
load("data/analysis/IOtweets19_all.RData")

#Most Followed Random
unique_random<-SAusertweets19_all[!duplicated(SAusertweets19_all[,c('screen_name')]),]
#7418 unique users 
#Find top 100 unique users to code
top_random<-unique_random[order(-unique_random$followers_count),]
top_random<-top_random[c("user_id", "screen_name", "followers_count")]
top_random<-top_random[1:100,]
write.csv(top_random, "data/output/top_random_accounts.csv")
# *manually code*
top_random<-read_csv("data/analysis/top_random_accounts_coded.csv")
top_random<-top_random[c("screen_name", "followers_count", "type")]
xtable(top_random)
table(top_random$type)

#Most Followed Geo
unique_geo<-GEOusertweets19[!duplicated(GEOusertweets19[,c('user.screen_name')]),]
#35303 unique users 
#Find top 100 to code
top_geo<-unique_geo[order(-unique_geo$user.followers_count),]
top_geo<-top_geo[c("user.id_str", "user.screen_name", "user.followers_count")]
top_geo<-top_geo[1:100,]
write.csv(top_geo, "data/output/top_geo_accounts.csv")
# *manually code*
top_geo<-read_csv("data/analysis/top_geo_accounts_coded.csv")
top_geo<-top_geo[c("user.screen_name", "user.followers_count", "type")]
xtable(top_geo)
table(top_geo$type)


#Most Followed IO Accounts 
unique_io<-IOtweets19_all[!duplicated(IOtweets19_all[,c('user_screen_name')]),]
#4539 unique users 
#Find top 100 unique users to code
top_io<-unique_io[order(-unique_io$follower_count),]
top_io<-top_io[c("userid", "user_screen_name", "user_profile_description", "follower_count")]
write.csv(top_io, "data/output/top_io_accounts.csv")

#Find Top Terms in their Bios 

unigrams<-unique_io %>% 
  unnest_tokens(output = unigram, input = user_profile_description) %>% 
  count(unigram, sort = TRUE)

#Limit to unigrams that appear at least 10 times
unigrams<-subset(unigrams, n>=10)

write.csv(unigrams, "data/output/top_unigrams_io_bios.csv")

#Read in coded unigrams 

unigrams_coded<-read.csv("data/analysis/top_unigrams_io_bios_coded.csv")
table(unigrams_coded$type)

employment<-subset(unigrams_coded, type=="employment")
employment<-paste(employment$unigram, collapse="|")

kingdom<-subset(unigrams_coded, type=="king"|type=="saudi"|type=="homeland")
kingdom<-paste(kingdom$unigram, collapse="|")

student<-subset(unigrams_coded, type=="student")
student<-paste(student$unigram, collapse="|")

inspirational_religious<-subset(unigrams_coded, type=="family"|type=="inspirational"|type=="poetry")
inspirational_religious<-paste(inspirational_religious$unigram, collapse="|")

football<-subset(unigrams_coded, type=="football")
football<-paste(football$unigram, collapse="|")

social_media<-subset(unigrams_coded, type=="social_media")
social_media<-paste(social_media$unigram, collapse="|")

top_io = top_io %>% mutate(inspirational_religious = str_detect(tolower(user_profile_description), inspirational_religious))
table(top_io$inspirational_religious)
top_io = top_io %>% mutate(employment = str_detect(tolower(user_profile_description), employment))
table(top_io$employment)
top_io = top_io %>% mutate(student = str_detect(tolower(user_profile_description), student))
table(top_io$student)
top_io = top_io %>% mutate(football = str_detect(tolower(user_profile_description), football))
table(top_io$football)
top_io = top_io %>% mutate(social_media = str_detect(tolower(user_profile_description), social_media))
table(top_io$social_media)
top_io = top_io %>% mutate(kingdom = str_detect(tolower(user_profile_description), kingdom))
table(top_io$kingdom)
write.csv(top_io, "data/analysis/top_io_accounts_coded_bios.csv")

top_io<-read_csv("data/analysis/top_io_accounts_coded_bios.csv")
table(top_io$employment) #215
table(top_io$student) #206
table(top_io$football) #113
table(top_io$social_media) #181
table(top_io$kingdom) #287









