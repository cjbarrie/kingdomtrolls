library(tidyverse)
library(quanteda)
library(readr)
library(xtable)

options(scipen = 999)

# GET NEWS ACCOUNTS TWEETS
load("data/analysis/NEWSusertweets19_all.RData")

#GET TOP HASHTAGS
toks_tweets <- tokens(NEWSusertweets19_all$text, remove_punct = TRUE) 
dfmat_tweets <- dfm(toks_tweets, select = "#*")
tstat_freq <- textstat_frequency(dfmat_tweets)
top_hashtags<-subset(tstat_freq, frequency>500)
write_csv(top_hashtags, "data/output/top_hashtags_NEWS.csv")
# *manually code*

#################################### MANUALLY CODED:

polhtags <- read_csv("data/output/top_hashtags_NEWS_edited.csv")

head(polhtags)

polhtags_f <- polhtags %>%
  filter(domestic_political==1|foreign_political==1|iran==1|yemen==1|royal==1) %>%
  select(frequency, translation_to_use, domestic_political, foreign_political, iran, yemen, royal) %>%
  mutate(rank = dense_rank(desc(frequency))) %>%
  arrange(rank) %>%
  select(rank, frequency, translation_to_use, domestic_political, foreign_political, iran, yemen, royal)

polhtags_f$translation_to_use <- paste0("#", polhtags_f$translation_to_use)

colnames <- colnames(polhtags_f)

polhtags_f[, colnames][is.na(polhtags_f[, colnames])] <- 0

tab <- kableExtra::kable(polhtags_f, digits = 2, "latex")
tab
