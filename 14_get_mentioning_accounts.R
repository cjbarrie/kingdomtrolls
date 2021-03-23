library(tidyverse)
library(ggthemes)
library(stringr)
library(readr)

# GET SAUDI IOs DATA 2019 r1 and r2
load("data/analysis/IOtweets19_all.RData")
# GET RANDOM SAMPLE OF SAUDI USERS DATA
load("data/analysis/SAusertweets19_all.RData")
# GET RANDOM SAMPLE OF GEO SAUDI USERS DATA
load("data/analysis/GEOusertweets19_all.RData")
# GET TOP ACCOUNTS TWEETS
load("data/analysis/TOPusertweets19_all.RData")
# GET NEWS ACCOUNTS TWEETS
load("data/analysis/NEWSusertweets19_all.RData")

##############################

#GET IO, SA, GEO , and TOP MENTIONS

#SA
SAusertweets19_all$ment_extract <- str_extract_all(SAusertweets19_all$text, "@\\S+")
SAments <- SAusertweets19_all$ment_extract
SAments <- unlist(SAments)
SAments <- as.data.frame(SAments)

SAtopments <- SAments %>%
  mutate(obs=1) %>%
  group_by(SAments) %>%
  summarise(sum_ments = sum(obs))

SA_unique_ments<- as.data.frame(unique(SAments))

#GEO RANDOM
GEOusertweets19_all$ment_extract <- str_extract_all(GEOusertweets19_all$text, "@\\S+")
GEOments <- GEOusertweets19_all$ment_extract
GEOments <- unlist(GEOments)
GEOments <- as.data.frame(GEOments)

GEOtopments <- GEOments %>%
  mutate(obs=1) %>%
  group_by(GEOments) %>%
  summarise(sum_ments = sum(obs))

GEO_unique_ments<- as.data.frame(unique(GEOments))

#TOP
TOPusertweets19_all$ment_extract <- str_extract_all(TOPusertweets19_all$text, "@\\S+")
TOPments <- TOPusertweets19_all$ment_extract
TOPments <- unlist(TOPments)
TOPments <- as.data.frame(TOPments)

TOPtopments <- TOPments %>%
  mutate(obs=1) %>%
  group_by(TOPments) %>%
  summarise(sum_ments = sum(obs))

TOP_unique_ments<- as.data.frame(unique(TOPments))

# FILTER SA, GEO MENTIONS BY IO USERS

IOusers <-  unique(IOtweets19_all$user_screen_name)
IOusers <- paste("@", IOusers, sep="")

SAIOments <- intersect(SA_unique_ments$SAments, IOusers)

GEOIOments <- intersect(GEO_unique_ments$GEOments, IOusers)

TOPIOments <- intersect(TOP_unique_ments$TOPments, IOusers)


#GET USERS WHO MENTION IO ACCOUNT
SAIOmentweets <- SAusertweets19_all %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|"))) %>%
  mutate(sample = "SA") %>%
  select(screen_name, text, date, sample)
SA_IO_mention_users <- unique(SAIOmentweets$screen_name)
SA_IO_mention_users <- as.data.frame(paste0("@", SA_IO_mention_users))
SA_IO_mention_users$sample<- "SA"
colnames(SA_IO_mention_users) <- c("screen_name", "sample")

GEOIOmentweets <- GEOusertweets19_all %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|"))) %>%
  mutate(sample = "GEO") %>%
  select(screen_name, text, date, sample)
GEO_IO_mention_users <- unique(GEOIOmentweets$screen_name)
GEO_IO_mention_users <- as.data.frame(paste0("@", GEO_IO_mention_users))
GEO_IO_mention_users$sample<- "GEO"
colnames(GEO_IO_mention_users) <- c("screen_name", "sample")

TOPIOmentweets <- TOPusertweets19_all %>%
  filter(str_detect(text, paste(TOPIOments, collapse = "|"))) %>%
  mutate(screen_name = user_screen_name,
         sample = "TOP") %>%
  select(screen_name, text, date, sample)
TOP_IO_mention_users <- unique(TOPIOmentweets$screen_name)
TOP_IO_mention_users <- as.data.frame(paste0("@", TOP_IO_mention_users))
TOP_IO_mention_users$sample<- "TOP"
colnames(TOP_IO_mention_users) <- c("screen_name", "sample")

IO_mentioning_accounts <- rbind(SA_IO_mention_users, 
                                              GEO_IO_mention_users, 
                                              TOP_IO_mention_users)

write_csv(IO_mentioning_accounts, "data/output/IO_mentioning_accounts.csv")

IO_mentioning_tweets <- rbind(SAIOmentweets, 
                                GEOIOmentweets, 
                                TOPIOmentweets)

write_csv(IO_mentioning_tweets, "data/output/IO_mentioning_tweets.csv")