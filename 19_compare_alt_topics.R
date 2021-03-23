library(tidyverse)
library(ggthemes)
library(patchwork)
library(ggbeeswarm)

options(scipen = 999)

# GET SAUDI IOs DATA 2019 r1 and r2
load("data/analysis/IOtweets19_all.RData")
# GET RANDOM SAMPLE OF SAUDI USERS DATA 2019
load("data/analysis/SAusertweets19_all.RData")
# GET GEO USERS TWEETS
load("data/analysis/GEOusertweets19_all.RData")
# GET TOP ACCOUNTS TWEETS
load("data/analysis/TOPusertweets19_all.RData")
# GET NEWS ACCOUNTS TWEETS
load("data/analysis/NEWSusertweets19_all.RData")

#SUBSET DATA

IO <- IOtweets19_all %>%
  select(retweet_count, like_count, tweet_text) %>%
  mutate(
    tweets = "IO tweets",
    col = "#0c3329",
    favorite_count = like_count,
    text = tweet_text
  ) %>%
  select(retweet_count, favorite_count, text, tweets, col)

SA <- SAusertweets19_all %>%
  select(retweet_count, favorite_count, text) %>%
  mutate(tweets = "SA tweets",
         col = "#155644")

GEO <- GEOusertweets19_all %>%
  select(retweet_count, favorite_count, text) %>%
  mutate(tweets = "GEO tweets",
         col = "#1d785f")

TOP <- TOPusertweets19_all %>%
  select(retweet_count, favorite_count, text) %>%
  mutate(tweets = "TOP tweets",
         col = "#259a7a")

NEWS <- NEWSusertweets19_all %>%
  select(retweet_count, favorite_count, text) %>%
  mutate(tweets = "NEWS tweets",
         col = "#29ab87")


dfs <- rbind(IO, SA, GEO, TOP, NEWS)
rm(list=setdiff(ls(), "dfs"))

# GET NEWS HASHTAGS
nhtags <- read_csv("data/output/top_hashtags_NEWS_edited.csv")

# EXTRACT DOMESTIC POLITICS HASHTAGS

htag_extract <- nhtags %>%
  filter(domestic_political==1) %>%
  pull(feature)

dfdom <- dfs %>%
  filter(str_detect(text, paste(htag_extract, collapse = "|")))

dfdom$tweetslev <-
  factor(
    dfdom$tweets,
    levels = c("IO tweets", "SA tweets",
               "GEO tweets", "TOP tweets",
               "NEWS tweets")
  )

ggplot(dfdom, aes(tweetslev, log10(retweet_count), col = tweetslev)) +
  geom_quasirandom(alpha = .5) +
  geom_boxplot(alpha = .1, lwd = .2) +
  scale_color_manual(values = c("#0c3329", "#155644",
                                "#1d785f", "#259a7a", "#29ab87")) +
  theme_tufte(base_family = "Helvetica") +
  xlab("Tweets") + ylab("Political (domestic) tweet retweet count (logged)") +
  theme(
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    legend.position = "none"
  )

ggsave("data/output/plots/compare_RT2_dom.png", width=600, height = 250, dpi=200, units="mm")

# EXTRACT FOREIGN POLITICS HASHTAGS

htag_extract <- nhtags %>%
  filter(foreign_political==1) %>%
  pull(feature)

dffor <- dfs %>%
  filter(str_detect(text, paste(htag_extract, collapse = "|")))

dffor$tweetslev <-
  factor(
    dffor$tweets,
    levels = c("IO tweets", "SA tweets",
               "GEO tweets", "TOP tweets",
               "NEWS tweets")
  )

ggplot(dffor, aes(tweetslev, log10(retweet_count), col = tweetslev)) +
  geom_quasirandom(alpha = .5) +
  geom_boxplot(alpha = .1, lwd = .2) +
  scale_color_manual(values = c("#0c3329", "#155644",
                                "#1d785f", "#259a7a", "#29ab87")) +
  theme_tufte(base_family = "Helvetica") +
  xlab("Tweets") + ylab("Political (foreign) tweet retweet count (logged)") +
  theme(
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    legend.position = "none"
  )

ggsave("data/output/plots/compare_RT2_for.png", width=600, height = 250, dpi=200, units="mm")

# EXTRACT YEMEN HASHTAGS

htag_extract <- nhtags %>%
  filter(yemen==1) %>%
  pull(feature)

dfyem <- dfs %>%
  filter(str_detect(text, paste(htag_extract, collapse = "|")))

dfyem$tweetslev <-
  factor(
    dfyem$tweets,
    levels = c("IO tweets", "SA tweets",
               "GEO tweets", "TOP tweets",
               "NEWS tweets")
  )

ggplot(dfyem, aes(tweetslev, log10(retweet_count), col = tweetslev)) +
  geom_quasirandom(alpha = .5) +
  geom_boxplot(alpha = .1, lwd = .2) +
  scale_color_manual(values = c("#0c3329", "#155644",
                                "#1d785f", "#259a7a", "#29ab87")) +
  theme_tufte(base_family = "Helvetica") +
  xlab("Tweets") + ylab("Yemen tweet retweet count (logged)") +
  theme(
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    legend.position = "none"
  )

ggsave("data/output/plots/compare_RT2_yem.png", width=600, height = 250, dpi=200, units="mm")

# EXTRACT IRAN HASHTAGS

htag_extract <- nhtags %>%
  filter(iran==1) %>%
  pull(feature)

dfirn <- dfs %>%
  filter(str_detect(text, paste(htag_extract, collapse = "|")))

dfirn$tweetslev <-
  factor(
    dfirn$tweets,
    levels = c("IO tweets", "SA tweets",
               "GEO tweets", "TOP tweets",
               "NEWS tweets")
  )

ggplot(dfirn, aes(tweetslev, log10(retweet_count), col = tweetslev)) +
  geom_quasirandom(alpha = .5) +
  geom_boxplot(alpha = .1, lwd = .2) +
  scale_color_manual(values = c("#0c3329", "#155644",
                                "#1d785f", "#259a7a", "#29ab87")) +
  theme_tufte(base_family = "Helvetica") +
  xlab("Tweets") + ylab("Iran tweet retweet count (logged)") +
  theme(
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    legend.position = "none"
  )

ggsave("data/output/plots/compare_RT2_irn.png", width=600, height = 250, dpi=200, units="mm")

# EXTRACT ROYAL HASHTAGS

htag_extract <- nhtags %>%
  filter(royal==1) %>%
  pull(feature)

dfroy <- dfs %>%
  filter(str_detect(text, paste(htag_extract, collapse = "|")))

dfroy$tweetslev <-
  factor(
    dfroy$tweets,
    levels = c("IO tweets", "SA tweets",
               "GEO tweets", "TOP tweets",
               "NEWS tweets")
  )

ggplot(dfroy, aes(tweetslev, log10(retweet_count), col = tweetslev)) +
  geom_quasirandom(alpha = .5) +
  geom_boxplot(alpha = .1, lwd = .2) +
  scale_color_manual(values = c("#0c3329", "#155644",
                                "#1d785f", "#259a7a", "#29ab87")) +
  theme_tufte(base_family = "Helvetica") +
  xlab("Tweets") + ylab("Royal tweet retweet count (logged)") +
  theme(
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    legend.position = "none"
  )

ggsave("data/output/plots/compare_RT2_roy.png", width=600, height = 250, dpi=200, units="mm")