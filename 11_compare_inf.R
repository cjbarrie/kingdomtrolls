library(tidyverse)
library(ggthemes)
library(patchwork)
library(stringr)
library(zoo)

# GET SAUDI IOs DATA 2019 r1 and r2
load("data/analysis/IOtweets19_all.RData")
# GET RANDOM SAMPLE OF SAUDI USERS DATA
load("data/analysis/SAusertweets19_all.RData")
# GET RANDOM SAMPLE OF GEO SAUDI USERS DATA
load("data/analysis/GEOusertweets19_all.RData")

##############################

#GET IO, SA, AND GEO MENTIONS

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

# FILTER SA, GEO MENTIONS BY IO USERS

# % of tweets

IOusers <-  unique(IOtweets19_all$user_screen_name)
IOusers <- paste("@", IOusers, sep="")

SAIOments <- intersect(SA_unique_ments$SAments, IOusers)
GEOIOments <- intersect(GEO_unique_ments$GEOments, IOusers)

SAmentsums <- SAments %>%
  mutate(obs=1) %>%
  filter(SAments %in% SAIOments) %>%
  group_by(SAments) %>%
  summarise(sum_ments = sum(obs))

GEOmentsums <- GEOments %>%
  mutate(obs=1) %>%
  filter(GEOments %in% GEOIOments) %>%
  group_by(GEOments) %>%
  summarise(sum_ments = sum(obs))

#GET USERS WHO MENTION IO ACCOUNT
SAIOmentweets <- SAusertweets19_all %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|")))
SA_IO_mention_users <- unique(SAIOmentweets$screen_name)
SA_IO_mention_users <- paste0("@", SA_IO_mention_users)

GEOIOmentweets <- GEOusertweets19_all %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|")))
GEO_IO_mention_users <- unique(GEOIOmentweets$screen_name)
SA_IO_mention_users <- paste0("@", SA_IO_mention_users)

SAtotals <- SAusertweets19_all %>%
  mutate(obs=1) %>%
  group_by(date) %>%
  summarise(sum_tweets = sum(obs))

GEOtotals <- GEOusertweets19_all %>%
  mutate(obs=1) %>%
  group_by(date) %>%
  summarise(sum_tweets = sum(obs))

#calculate % of total users interacted with IO account

(length(unique(SAIOmentweets$user_id))/length(unique(SAusertweets19_all$user_id))*100)
(length(unique(GEOIOmentweets$user_id))/length(unique(GEOusertweets19_all$user_id))*100)

g1 <- SAusertweets19_all %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|"))) %>%
  group_by(date) %>%
  summarise(sum_IOments = sum(obs))  %>%
  full_join(SAtotals, SAusertweets19_all, by="date") %>%
  mutate(sum_IOments= ifelse(is.na(sum_IOments), 0, sum_IOments),
         pct_IOment = sum_IOments/sum_tweets,
         rmean = rollmean(pct_IOment, 7, na.pad=TRUE),
         omean = nrow(SAIOmentweets)/nrow(SAusertweets19_all)) %>%
  arrange(date) %>%
  ggplot() +
  geom_line(aes(date, pct_IOment), alpha=.3) +
  geom_line(aes(date, rmean), col="#155644", size=4) +
  geom_line(aes(date, omean), col="#155644", size=2, linetype="dashed") +
  ggtitle("SA tweets") +
  xlab("Date")+ ylab("% tweets mentioning IO account") +
  scale_y_continuous(labels = scales::percent_format(accuracy=.1),
                     limits = c(0,.003)) +
  theme_tufte(base_family="Helvetica") +
  theme(axis.text.y=element_text(size=30),
        axis.text.x=element_text(size=30),
        axis.title.x=element_text(size=30, face="bold"),
        axis.title.y=element_text(size=30, face="bold"),
        plot.title = element_text(size=35, face="bold", hjust = .5))

g2 <- GEOusertweets19_all %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|"))) %>%
  group_by(date) %>%
  summarise(sum_IOments = sum(obs))  %>%
  full_join(GEOtotals, GEOusertweets19_all, by="date") %>%
  mutate(sum_IOments= ifelse(is.na(sum_IOments), 0, sum_IOments),
         pct_IOment = sum_IOments/sum_tweets,
         rmean = rollmean(pct_IOment, 7, na.pad=TRUE),
         omean =  nrow(GEOIOmentweets)/nrow(GEOusertweets19_all)) %>%
  arrange(date) %>%
  ggplot() +
  geom_line(aes(date, pct_IOment), alpha=.3) +
  geom_line(aes(date, rmean), col="#1d785f", size=4) +
  geom_line(aes(date, omean), col="#1d785f", size=2, linetype="dashed") +
  ggtitle("GEO tweets") +
  xlab("Date")+ ylab("% tweets mentioning IO account") +
  scale_y_continuous(labels = scales::percent_format(accuracy=.1),
                     limits = c(0,.003)) +
  theme_tufte(base_family="Helvetica") +
  theme(axis.text.y=element_text(size=30),
        axis.text.x=element_text(size=30),
        axis.title.x=element_text(size=30, face="bold"),
        axis.title.y=element_text(size=30, face="bold"),
        plot.title = element_text(size=35, face="bold", hjust = .5))

g1 + g2

ggsave("data/output/plots/IOmentpcts.png", width=800, height = 400, dpi=100, units="mm")

print(nrow(SAIOmentweets)/nrow(SAusertweets19_all)*100)
print(nrow(GEOIOmentweets)/nrow(GEOusertweets19_all)*100)

# % of mentions

SAmenyeartotal <- SAusertweets19_all %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  summarise(num_ments=sum(num_ments)) %>%
  pull(num_ments)

GEOmenyeartotal <- GEOusertweets19_all %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  summarise(num_ments=sum(num_ments))  %>%
  pull(num_ments)

SAIOmenyeartotal <- SAusertweets19_all %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|"))) %>%
  group_by(year) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  summarise(sum_IOments = sum(num_IOments)) %>%
  pull(sum_IOments)

GEOIOmenyeartotal <- GEOusertweets19_all %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|"))) %>%
  group_by(year) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  summarise(sum_IOments = sum(num_IOments)) %>%
  pull(sum_IOments)

SAmentotals <- SAusertweets19_all %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(num_ments=sum(num_ments))

GEOmentotals <- GEOusertweets19_all %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(num_ments=sum(num_ments))

g1 <- SAusertweets19_all %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|"))) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(sum_IOments=sum(num_IOments)) %>%
  full_join(SAmentotals, SAusertweets19_all, by="date") %>%
  mutate(sum_IOments= ifelse(is.na(sum_IOments), 0, sum_IOments),
         pct_IOment = sum_IOments/num_ments,
         rmean = rollmean(pct_IOment, 7, na.pad=TRUE),
         omean = SAIOmenyeartotal/SAmenyeartotal) %>%
  arrange(date) %>%
  ggplot() +
  geom_line(aes(date, pct_IOment), alpha=.3) +
  geom_line(aes(date, rmean), col="#155644", size=4) +
  geom_line(aes(date, omean), col="#155644", size=2, linetype="dashed") +
  ggtitle("SA tweets") +
  xlab("Date")+ ylab("% mentions mentioning IO account") +
  scale_y_continuous(labels = scales::percent_format(accuracy=.1),
                     limits = c(0,.02)) +
  theme_tufte(base_family="Helvetica") +
  theme(axis.text.y=element_text(size=30),
        axis.text.x=element_text(size=30),
        axis.title.x=element_text(size=30, face="bold"),
        axis.title.y=element_text(size=30, face="bold"),
        plot.title = element_text(size=35, face="bold", hjust = .5))

g2 <- GEOusertweets19_all %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|"))) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(sum_IOments=sum(num_IOments)) %>%
  full_join(GEOmentotals, GEOusertweets19_all, by="date") %>%
  mutate(sum_IOments= ifelse(is.na(sum_IOments), 0, sum_IOments),
         pct_IOment = sum_IOments/num_ments,
         rmean = rollmean(pct_IOment, 7, na.pad=TRUE),
         omean = GEOIOmenyeartotal/GEOmenyeartotal) %>%
  arrange(date) %>%
  ggplot() +
  geom_line(aes(date, pct_IOment), alpha=.3) +
  geom_line(aes(date, rmean), col="#1d785f", size=4) +
  geom_line(aes(date, omean), col="#1d785f", size=2, linetype="dashed") +
  ggtitle("GEO tweets") +
  xlab("Date")+ ylab("% mentions mentioning IO account") +
  scale_y_continuous(labels = scales::percent_format(accuracy=.1),
                     limits = c(0,.02)) +
  theme_tufte(base_family="Helvetica") +
  theme(axis.text.y=element_text(size=30),
        axis.text.x=element_text(size=30),
        axis.title.x=element_text(size=30, face="bold"),
        axis.title.y=element_text(size=30, face="bold"),
        plot.title = element_text(size=35, face="bold", hjust = .5))

g1 + g2

ggsave("data/output/plots/IOmentpcts2.png", width=800, height = 400, dpi=100, units="mm")

print(SAIOmenyeartotal/SAmenyeartotal*100)
print(GEOIOmenyeartotal/GEOmenyeartotal*100)
