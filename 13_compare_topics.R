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
  mutate(tweets = "IO tweets",
         col= "#0c3329", 
         favorite_count= like_count,
         text = tweet_text) %>%
  select(retweet_count, favorite_count, text, tweets, col)

SA <- SAusertweets19_all %>%
  select(retweet_count, favorite_count, text) %>%
  mutate(tweets = "SA tweets",
         col= "#155644")

GEO <- GEOusertweets19_all %>%
  select(retweet_count, favorite_count, text) %>%
  mutate(tweets = "GEO tweets",
         col= "#1d785f")

TOP<- TOPusertweets19_all %>%
  select(retweet_count, favorite_count, text) %>%
  mutate(tweets = "TOP tweets",
         col= "#259a7a")

NEWS <- NEWSusertweets19_all %>%
  select(retweet_count, favorite_count, text) %>%
  mutate(tweets = "NEWS tweets",
         col= "#29ab87")


dfs <- rbind(IO, SA, GEO, TOP, NEWS)

dfkhash <- dfs %>% 
  filter(grepl("خاشقجي",text))

dfkhash$tweetslev <-factor(dfkhash$tweets,levels=c("IO tweets","SA tweets",
                                                   "GEO tweets", "TOP tweets",
                                                   "NEWS tweets"))

ggplot(dfkhash, aes(tweetslev, log10(retweet_count), col=tweetslev)) +
  geom_quasirandom(alpha=.5) +
  geom_boxplot(alpha=.1, lwd=.2) +
  scale_color_manual(values=c("#0c3329", "#155644", 
                              "#1d785f", "#259a7a","#29ab87")) +
  theme_tufte(base_family = "Helvetica") +
  xlab("Tweets")+ ylab("'Khashoggi'' tweet retweet count (logged)") +
  theme(axis.text.y=element_text(size=20),
        axis.text.x=element_text(size=20),
        axis.title.x=element_text(size=20, face="bold"),
        axis.title.y=element_text(size=20, face="bold"), 
        legend.position = "none")

ggsave("data/output/plots/compare_khash2.png", width=600, height = 250, dpi=200, units="mm")

dfqat <- dfs %>% 
  filter(grepl("قطر",text)) 

dfqat$tweetslev <-factor(dfqat$tweets,levels=c("IO tweets","SA tweets",
                                                   "GEO tweets", "TOP tweets",
                                                   "NEWS tweets"))

ggplot(dfqat, aes(tweetslev, log10(retweet_count), col=tweetslev)) +
  geom_quasirandom(alpha=.5) +
  geom_boxplot(alpha=.1, lwd=.2) +
  scale_color_manual(values=c("#0c3329", "#155644", 
                              "#1d785f", "#259a7a","#29ab87")) +
  theme_tufte(base_family = "Helvetica") +
  xlab("Tweets")+ ylab("'Qatar'' tweet retweet count (logged)") +
  theme(axis.text.y=element_text(size=20),
        axis.text.x=element_text(size=20),
        axis.title.x=element_text(size=20, face="bold"),
        axis.title.y=element_text(size=20, face="bold"), 
        legend.position = "none")

ggsave("data/output/plots/compare_qat2.png", width=600, height = 250, dpi=200, units="mm")
