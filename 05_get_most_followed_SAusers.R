library(tidyverse)
library(readxl)
library(stringr)

# GET TOP FOLLOWED SA USERS FROM SOCIALBAKERS DATA

SA_top_accounts <- read_excel("data/raw/SA_top_accounts_unformatted.xlsx")

SA_top <- unlist(str_extract_all(SA_top_accounts$SA_top_users, "\\([^()]+\\)"))
SA_top <- SA_top[-6]
SA_top <- as.data.frame(gsub("[()]", "", SA_top))
colnames(SA_top) <- "SA_top_accounts"
write.csv(SA_top, "data/output/SA_top_accounts.csv", row.names = F)
