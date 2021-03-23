library(tidyverse)
##devtools::install_github("alexpghayes/socialsampler")
library(socialsampler)
register_token("ENTER>consumer_key",
               "ENTER>consumer_secret",
               "ENTER>access_token",
               "ENTER>access_token_secret")


satweets <- read.csv("data/output/sauditweets.csv")
colnames(satweets)
unique(satweets$place_country)
unique(satweets$place_country_code)

satweets <- satweets %>%
  filter(place_country_code=="SA")

options(scipen = 999)
users <- unique(satweets$user_screen_name)

set.seed(100)
users_sample <- sample(users, size=10400) # get random sample of 10400 users (200 rate limit periods)
tmls <- vector("list", length(users_sample)) #change to just "users" to get all timelines


for (i in seq_along(tmls)) {
  tmls[[i]] <- safe_get_timelines(users_sample[i], n = 3200)
  ## assuming full rate limit at start, wait for fresh reset every 52 users
  if (i %% 52L == 0L) { #change to 52L to get every 52 (rate limit)
    tmlstocsv <- do_call_rbind(tmls[(i-51):i])
    outfile.name <- paste0('data/output/saudiusertweets/tmls_', i,".RData")
    df <- tmlstocsv[,c(1:6, 13:16,64:68, 71, 73:75,78:85)]
    df$bbox_coords <- as.character(df$bbox_coords) #convert bbox list to character to avoid write.table error
    #write.csv(df, outfile.name, row.names=FALSE, sep = ",", fileEncoding = "UTF-8")
    save(df, file= outfile.name)
    # rl <- rate_limit("get_timeline") #turn on to automate sleep when rate limit reached (not needed with safe_get_timelines)
    # Sys.sleep(as.numeric(rl$reset, "secs")) #turn on to automate sleep when rate limit reached (not needed with safe_get_timelines)
    tmls <- vector("list", length(users_sample)) #empty list to save local storage in R
  }
  ## print update message
  cat("requesting user number", i, " ")
}

