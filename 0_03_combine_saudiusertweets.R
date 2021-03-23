library(tidyverse)

# LOOP THROUGH FILES OF USER TIMELINES AND BIND
files <- list.files(path=file.path("data/output/saudiusertweets/"),recursive=T,include.dirs=T)
files <- paste("data/output/saudiusertweets/", files, sep="")

df_all <- data.frame()
for (i in seq_along(files)) {
  filename = files[[i]]
  load(filename)
  cat("Ingesting filename:", filename)
  df_all <- rbind(df_all,df)
}

save(df_all, file = "data/output/saudiusertweets.RData")

# KEEP 2019 OBSERVATIONS FOR SA USER TWEETS
df_all$date <- as.Date(df_all$created_at)
df_all$year <- year(df_all$date)
SAusertweets19_all <- df_all %>%
  filter(year==2019)

save(SAusertweets19_all, file = "data/analysis/SAusertweets19_all.RData")