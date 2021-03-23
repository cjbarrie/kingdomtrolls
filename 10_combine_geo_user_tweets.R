library(tidyverse)
library(lubridate)

files <- list.files(path=file.path("data/output/saudigeousertweets/"),recursive=T,include.dirs=T)
files <- paste("data/output/saudigeousertweets/", files, sep="")

df_all <- data.frame()
for (i in seq_along(files)) {
  filename = files[[i]]
  df <- readRDS(filename)
  df_all <- rbind(df_all,df)
  if (i %% 100L == 0L) { #every 100 ingestions
    outfile.name <- paste0('data/output/saudigeousertweets_combined/df_', i,".RDS")
    saveRDS(df_all, file= outfile.name)
    df_all <- data.frame() #empty df to keep local storage in R
  }
  ## print update message
  cat("Ingesting filename: ", filename, " \n")
}

files <- list.files(path=file.path("data/output/saudigeousertweets_combined/"),recursive=T,include.dirs=T)
files <- paste("data/output/saudigeousertweets_combined/", files, sep="")

GEOusertweets19_all <- data.frame()
for (i in seq_along(files)) {
  filename = files[[i]]
  df <- readRDS(filename)
  cat("Ingesting filename:", filename, " \n")
  df <- df[,-c(2,6,9,10,16,17,18, 22, 26, 27)]
  df$date <- as.Date(df$created_at)
  df$year <- year(df$date)
  df <- subset(df, year==2019)
  GEOusertweets19_all <- rbind(GEOusertweets19_all,df)
}

save(GEOusertweets19_all, file = "data/analysis/GEOusertweets19_all.RData")