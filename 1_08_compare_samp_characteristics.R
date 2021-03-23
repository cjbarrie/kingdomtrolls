library(tidyverse)
library(ggthemes)
library(patchwork)
library(ggbeeswarm)
library(readr)
library(zoo)

# GET SAUDI IOs DATA 2019 r1 and r2
load("data/analysis/IOtweets19_all.RData")
# GET RANDOM SAMPLE OF SAUDI USERS DATA
load("data/analysis/SAusertweets19_all.RData")
# GET RANDOM SAMPLE OF GEO SAUDI USERS DATA
load("data/analysis/GEOusertweets19_all.RData")

# COMPARE SA AND GEO SAMPLES FOR BASIC CHARACTERISTICS (TWEET FREQUENCY AND FOLLOW COUNTS)

#SUBSET DATA

SAdaycounts <- SAusertweets19_all %>%
  mutate(obs=1) %>%
  group_by(date, user_id) %>%
  summarise(tweetfreq=sum(obs)) %>%
  group_by(user_id) %>%
  summarise(tweetfreq = mean(tweetfreq))

GEOdaycounts <-GEOusertweets19_all %>%
  mutate(obs=1) %>%
  group_by(date, user_id) %>%
  summarise(tweetfreq=sum(obs)) %>%
  group_by(user_id) %>%
  summarise(tweetfreq = mean(tweetfreq))

SA <- SAusertweets19_all %>%
  group_by(user_id) %>%
  summarise(followers_count = max(followers_count)) %>%
  left_join(SAdaycounts, by = "user_id") %>%
  select(followers_count, tweetfreq) %>%
  mutate(tweets = "SA tweets",
         col= "#155644")

GEO <- GEOusertweets19_all %>%
  group_by(user_id) %>%
  summarise(followers_count = max(followers_count)) %>%
  left_join(SAdaycounts, by = "user_id") %>%
  select(followers_count, tweetfreq) %>%
  mutate(tweets = "GEO tweets",
         col= "#1d785f")

dfs <- rbind(SA, GEO)

g1 <- ggplot(dfs, aes(tweets, log10(followers_count), col=tweets)) +
  geom_quasirandom(alpha=.5) +
  geom_boxplot(alpha=.1, lwd=.2) +
  scale_color_manual(values=c("#0c3329", "#155644", 
                              "#1d785f", "#259a7a","#29ab87")) +
  theme_tufte(base_family = "Helvetica") +
  xlab("Tweets")+ ylab("Follower count (logged)") +
  theme(axis.text.y=element_text(size=20),
        axis.text.x=element_text(size=20),
        axis.title.x=element_text(size=20, face="bold"),
        axis.title.y=element_text(size=20, face="bold"), 
        legend.position = "none")

g2 <- ggplot(dfs, aes(tweets, log10(tweetfreq), col=tweets)) +
  geom_quasirandom(alpha=.5) +
  geom_boxplot(alpha=.1, lwd=.2) +
  scale_color_manual(values=c("#0c3329", "#155644", 
                              "#1d785f", "#259a7a","#29ab87")) +
  theme_tufte(base_family = "Helvetica") +
  xlab("Tweets")+ ylab("Daily tweet frequency (logged)") +
  theme(axis.text.y=element_text(size=20),
        axis.text.x=element_text(size=20),
        axis.title.x=element_text(size=20, face="bold"),
        axis.title.y=element_text(size=20, face="bold"), 
        legend.position = "none")

g1+ g2

ggsave("data/output/plots/compare_GEOSA.png", width=600, height = 250, dpi=200, units="mm")


# COMPARE SA AND GEO SAMPLES FOR POLITICAL CONTENT

nhtags <- read_csv("data/output/top_hashtags_NEWS_edited.csv")

#get hashtags for foreign and domestic political topics

fdpol <- nhtags %>%
  filter(foreign_political==1|domestic_political==1)
fdpolhtags <- fdpol$feature

SAtweets_fdpol <- SAusertweets19_all %>%
  filter(str_detect(text, paste(fdpolhtags, collapse = "|")))

GEOtweets_fdpol <- GEOusertweets19_all %>%
  filter(str_detect(text, paste(fdpolhtags, collapse = "|")))

GEOpctfdpol <- nrow(GEOtweets_fdpol)/nrow(GEOusertweets19_all)*100
SApctfdpol <- nrow(SAtweets_fdpol)/nrow(SAusertweets19_all)*100
print(GEOpctfdpol)
print(SApctfdpol)


# take top 25% of users by n of tweets about political topics
SApolusers <- SAusertweets19_all %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(fdpolhtags, collapse = "|"))) %>%
  group_by(user_id) %>%
  summarise(sum_obs=sum(obs)) %>%
  slice_max(sum_obs, prop=.25) %>%
  pull(user_id)

GEOpolusers <- GEOusertweets19_all %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(fdpolhtags, collapse = "|"))) %>%
  group_by(user_id) %>%
  summarise(sum_obs=sum(obs)) %>%
  slice_max(sum_obs, prop=.25) %>%
  pull(user_id)

# COMPARE ENGAGEMENT WITH IO ACCOUNTS FOR 'POLITICAL' USERS

#GET IO, SA, AND GEO MENTIONS

#SA
SAusertweets19_all_p <- SAusertweets19_all %>%
  filter(user_id %in% SApolusers)
SAusertweets19_all_p$ment_extract <- str_extract_all(SAusertweets19_all_p$text, "@\\S+")
SAments <- SAusertweets19_all_p$ment_extract
SAments <- unlist(SAments)
SAments <- as.data.frame(SAments)

SAtopments <- SAments %>%
  mutate(obs=1) %>%
  group_by(SAments) %>%
  summarise(sum_ments = sum(obs))

SA_unique_ments<- as.data.frame(unique(SAments))

#GEO RANDOM
GEOusertweets19_all_p <- GEOusertweets19_all %>%
  filter(user_id %in% GEOpolusers)
GEOusertweets19_all_p$ment_extract <- str_extract_all(GEOusertweets19_all_p$text, "@\\S+")
GEOments <- GEOusertweets19_all_p$ment_extract
GEOments <- unlist(GEOments)
GEOments <- as.data.frame(GEOments)

GEOtopments <- GEOments %>%
  mutate(obs=1) %>%
  group_by(GEOments) %>%
  summarise(sum_ments = sum(obs))

GEO_unique_ments<- as.data.frame(unique(GEOments))

# FILTER SA, GEO MENTIONS BY IO USERS

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
SAIOmentweets <- SAusertweets19_all_p %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|")))
SA_IO_mention_users <- unique(SAIOmentweets$screen_name)
SA_IO_mention_users <- paste0("@", SA_IO_mention_users)

GEOIOmentweets <- GEOusertweets19_all_p %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|")))
GEO_IO_mention_users <- unique(GEOIOmentweets$screen_name)
SA_IO_mention_users <- paste0("@", SA_IO_mention_users)

SAtotals <- SAusertweets19_all_p %>%
  mutate(obs=1) %>%
  group_by(date) %>%
  summarise(sum_tweets = sum(obs))

GEOtotals <- GEOusertweets19_all_p %>%
  mutate(obs=1) %>%
  group_by(date) %>%
  summarise(sum_tweets = sum(obs))


(length(unique(SAIOmentweets$user_id))/length(unique(SAusertweets19_all_p$user_id))*100) #25%
(length(unique(GEOIOmentweets$user_id))/length(unique(GEOusertweets19_all_p$user_id))*100) #16%

SApctIOmentweet <- nrow(SAIOmentweets)/nrow(SAusertweets19_all_p)*100
GEOpctIOmentweet <- nrow(GEOIOmentweets)/nrow(GEOusertweets19_all_p)*100

g1 <- SAusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|"))) %>%
  group_by(date) %>%
  summarise(sum_IOments = sum(obs))  %>%
  full_join(SAtotals, SAusertweets19_all_p, by="date") %>%
  mutate(sum_IOments= ifelse(is.na(sum_IOments), 0, sum_IOments),
         pct_IOment = sum_IOments/sum_tweets,
         rmean = rollmean(pct_IOment, 7, na.pad=TRUE),
         omean = nrow(SAIOmentweets)/nrow(SAusertweets19_all_p)) %>%
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

g2 <- GEOusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|"))) %>%
  group_by(date) %>%
  summarise(sum_IOments = sum(obs))  %>%
  full_join(GEOtotals, GEOusertweets19_all_p, by="date") %>%
  mutate(sum_IOments= ifelse(is.na(sum_IOments), 0, sum_IOments),
         pct_IOment = sum_IOments/sum_tweets,
         rmean = rollmean(pct_IOment, 7, na.pad=TRUE),
         omean =  nrow(GEOIOmentweets)/nrow(GEOusertweets19_all_p)) %>%
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

ggsave("data/output/plots/IOmentpcts_p.png", width=800, height = 400, dpi=100, units="mm")

print(nrow(SAIOmentweets)/nrow(SAusertweets19_all_p)*100)
print(nrow(GEOIOmentweets)/nrow(GEOusertweets19_all_p)*100)

SAmenyeartotal <- SAusertweets19_all_p %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  summarise(num_ments=sum(num_ments)) %>%
  pull(num_ments)

GEOmenyeartotal <- GEOusertweets19_all_p %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  summarise(num_ments=sum(num_ments))  %>%
  pull(num_ments)

SAIOmenyeartotal <- SAusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|"))) %>%
  group_by(year) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  summarise(sum_IOments = sum(num_IOments)) %>%
  pull(sum_IOments)

GEOIOmenyeartotal <- GEOusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|"))) %>%
  group_by(year) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  summarise(sum_IOments = sum(num_IOments)) %>%
  pull(sum_IOments)

SApctIOments<- SAIOmenyeartotal/SAmenyeartotal *100
GEOpctIOments <- GEOIOmenyeartotal/GEOmenyeartotal *100

SAmentotals <- SAusertweets19_all_p %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(num_ments=sum(num_ments))

GEOmentotals <- GEOusertweets19_all_p %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(num_ments=sum(num_ments))

g1 <- SAusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|"))) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(sum_IOments=sum(num_IOments)) %>%
  full_join(SAmentotals, SAusertweets19_all_p, by="date") %>%
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

g2 <- GEOusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|"))) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(sum_IOments=sum(num_IOments)) %>%
  full_join(GEOmentotals, GEOusertweets19_all_p, by="date") %>%
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

ggsave("data/output/plots/IOmentpcts2_p.png", width=800, height = 400, dpi=100, units="mm")

print(SAIOmenyeartotal/SAmenyeartotal*100)
print(GEOIOmenyeartotal/GEOmenyeartotal*100)

# take bottom 25% of users by n of tweets about political topics
SAnpolusers <- SAusertweets19_all %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(fdpolhtags, collapse = "|"))) %>%
  group_by(user_id) %>%
  summarise(sum_obs=sum(obs)) %>%
  slice_min(sum_obs, prop=.25) %>%
  pull(user_id)

GEOnpolusers <- GEOusertweets19_all %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(fdpolhtags, collapse = "|"))) %>%
  group_by(user_id) %>%
  summarise(sum_obs=sum(obs)) %>%
  slice_min(sum_obs, prop=.25) %>%
  pull(user_id)

# COMPARE ENGAGEMENT WITH IO ACCOUNTS FOR 'POLITICAL' USERS

#GET IO, SA, AND GEO MENTIONS

#SA
SAusertweets19_all_p <- SAusertweets19_all %>%
  filter(user_id %in% SAnpolusers)
SAusertweets19_all_p$ment_extract <- str_extract_all(SAusertweets19_all_p$text, "@\\S+")
SAments <- SAusertweets19_all_p$ment_extract
SAments <- unlist(SAments)
SAments <- as.data.frame(SAments)

SAtopments <- SAments %>%
  mutate(obs=1) %>%
  group_by(SAments) %>%
  summarise(sum_ments = sum(obs))

SA_unique_ments<- as.data.frame(unique(SAments))

#GEO RANDOM
GEOusertweets19_all_p <- GEOusertweets19_all %>%
  filter(user_id %in% GEOnpolusers)
GEOusertweets19_all_p$ment_extract <- str_extract_all(GEOusertweets19_all_p$text, "@\\S+")
GEOments <- GEOusertweets19_all_p$ment_extract
GEOments <- unlist(GEOments)
GEOments <- as.data.frame(GEOments)

GEOtopments <- GEOments %>%
  mutate(obs=1) %>%
  group_by(GEOments) %>%
  summarise(sum_ments = sum(obs))

GEO_unique_ments<- as.data.frame(unique(GEOments))

# FILTER SA, GEO MENTIONS BY IO USERS

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
SAIOmentweets <- SAusertweets19_all_p %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|")))
SA_IO_mention_users <- unique(SAIOmentweets$screen_name)
SA_IO_mention_users <- paste0("@", SA_IO_mention_users)

GEOIOmentweets <- GEOusertweets19_all_p %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|")))
GEO_IO_mention_users <- unique(GEOIOmentweets$screen_name)
SA_IO_mention_users <- paste0("@", SA_IO_mention_users)

SAtotals <- SAusertweets19_all_p %>%
  mutate(obs=1) %>%
  group_by(date) %>%
  summarise(sum_tweets = sum(obs))

GEOtotals <- GEOusertweets19_all_p %>%
  mutate(obs=1) %>%
  group_by(date) %>%
  summarise(sum_tweets = sum(obs))


(length(unique(SAIOmentweets$user_id))/length(unique(SAusertweets19_all_p$user_id))*100) #15%
(length(unique(GEOIOmentweets$user_id))/length(unique(GEOusertweets19_all_p$user_id))*100) #9%

SApctIOmentweet_np <- nrow(SAIOmentweets)/nrow(SAusertweets19_all_p)*100
GEOpctIOmentweet_np <- nrow(GEOIOmentweets)/nrow(GEOusertweets19_all_p)*100

g1 <- SAusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|"))) %>%
  group_by(date) %>%
  summarise(sum_IOments = sum(obs))  %>%
  full_join(SAtotals, SAusertweets19_all_p, by="date") %>%
  mutate(sum_IOments= ifelse(is.na(sum_IOments), 0, sum_IOments),
         pct_IOment = sum_IOments/sum_tweets,
         rmean = rollmean(pct_IOment, 7, na.pad=TRUE),
         omean = nrow(SAIOmentweets)/nrow(SAusertweets19_all_p)) %>%
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

g2 <- GEOusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|"))) %>%
  group_by(date) %>%
  summarise(sum_IOments = sum(obs))  %>%
  full_join(GEOtotals, GEOusertweets19_all_p, by="date") %>%
  mutate(sum_IOments= ifelse(is.na(sum_IOments), 0, sum_IOments),
         pct_IOment = sum_IOments/sum_tweets,
         rmean = rollmean(pct_IOment, 7, na.pad=TRUE),
         omean =  nrow(GEOIOmentweets)/nrow(GEOusertweets19_all_p)) %>%
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

ggsave("data/output/plots/IOmentpcts_np.png", width=800, height = 400, dpi=100, units="mm")

print(nrow(SAIOmentweets)/nrow(SAusertweets19_all_p)*100)
print(nrow(GEOIOmentweets)/nrow(GEOusertweets19_all_p)*100)


SAmenyeartotal <- SAusertweets19_all_p %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  summarise(num_ments=sum(num_ments)) %>%
  pull(num_ments)

GEOmenyeartotal <- GEOusertweets19_all_p %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  summarise(num_ments=sum(num_ments))  %>%
  pull(num_ments)

SAIOmenyeartotal <- SAusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|"))) %>%
  group_by(year) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  summarise(sum_IOments = sum(num_IOments)) %>%
  pull(sum_IOments)

GEOIOmenyeartotal <- GEOusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|"))) %>%
  group_by(year) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  summarise(sum_IOments = sum(num_IOments)) %>%
  pull(sum_IOments)

SApctIOments_np <- SAIOmenyeartotal/SAmenyeartotal *100
GEOpctIOments_np <- GEOIOmenyeartotal/GEOmenyeartotal *100

SAmentotals <- SAusertweets19_all_p %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(num_ments=sum(num_ments))

GEOmentotals <- GEOusertweets19_all_p %>%
  mutate(num_ments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(num_ments=sum(num_ments))

g1 <- SAusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(SAIOments, collapse = "|"))) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(sum_IOments=sum(num_IOments)) %>%
  full_join(SAmentotals, SAusertweets19_all_p, by="date") %>%
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

g2 <- GEOusertweets19_all_p %>%
  mutate(obs=1) %>%
  filter(str_detect(text, paste(GEOIOments, collapse = "|"))) %>%
  mutate(num_IOments = lengths(ment_extract)) %>%
  group_by(date) %>%
  summarise(sum_IOments=sum(num_IOments)) %>%
  full_join(GEOmentotals, GEOusertweets19_all_p, by="date") %>%
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

ggsave("data/output/plots/IOmentpcts2_np.png", width=800, height = 400, dpi=100, units="mm")

print(SAIOmenyeartotal/SAmenyeartotal*100)
print(GEOIOmenyeartotal/GEOmenyeartotal*100)
