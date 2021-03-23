library(readr)
library(igraph)
library(rtweet)
library(graphTweets)
library(dplyr)
library(sigmajs)
library(purrr)
library(tidyr)

#Read in IO Data
load("data/analysis/IOtweets19_all.RData")
#Subset to unique account names
IOusers<-unique(IOtweets19_all$user_screen_name)

#Read in Mentions Data (accounts that mentioned IO accounts)
mentions<-read_csv("data/output/IO_mentioning_tweets.csv")
accounts<-as.data.frame(table(mentions$screen_name))
mentions$id<-1:9860

#Make Mentions Network (Most mentioned IO accounts)
mentions_screen_name = 
  lapply(mentions$text, function(tx) {
    matches = gregexpr('@[^([:blank:]|[:punct:])]+', tx)[[1]]
    sapply(seq_along(matches), function(i) 
      substr(tx, matches[i] + 1, matches[i] + attr(matches, 'match.length')[i] - 1))
  })

mentions$mentions_screen_name<-mentions_screen_name
mentions<-unnest(mentions, mentions_screen_name)
#Limit to mentions of IO accounts
mentions$io<-ifelse(tolower(mentions$mentions_screen_name) %in% tolower(IOusers), 1,0)
table(mentions$io)
mentions<-subset(mentions, io==TRUE)

#get table of most mentioned IO accounts
IOmostmentab <- mentions %>%
  group_by(mentions_screen_name) %>%
  summarise(n = n()) %>%
  filter(n>=10) %>%
  arrange(desc(n)) %>%
  mutate(rank = dense_rank(desc(n))) %>%
  select(rank, mentions_screen_name, n)

# #check all unhashed by making sure only include those with >5000 followers
# 
# IOfollows <- IOtweets19_all %>%
#   group_by(user_screen_name) %>%
#   summarise(followers = max(follower_count)) %>%
#   select(user_screen_name, followers) %>%
#   filter(user_screen_name %in% IOmentusers)

tab <- kableExtra::kable(IOmostmentab, digits = 2, "latex", booktabs=T)
tab

gt<-mentions %>%
  gt_edges(screen_name, mentions_screen_name, io) %>%
  gt_nodes() %>%
  gt_add_meta(name = io, source = io, target = io)

gt$edges$id <- 1:nrow(gt$edges)
gt$nodes$id <- gt$nodes$nodes
gt$nodes$label <- gt$nodes$nodes
gt$nodes$io<-ifelse(tolower(gt$nodes$nodes) %in% tolower(IOusers), 1,0)
table(gt$nodes$io)
gt$nodes$color <- scales::col_numeric(c("blue", "red"), NULL)(gt$nodes$io)
table(gt$nodes$color)

#Plot Network Graph 
set.seed(1234567)
sigmajs() %>% 
  sg_nodes(gt$nodes, id, io, label, color) %>% 
  sg_edges(gt$edges, id, source, target) %>% 
  sg_layout(directed=TRUE, layout=igraph::layout_nicely) %>% 
  sg_settings(defaultNodeColor=gt$nodes$color,
    edgeColor = "default",
    defaultEdgeColor = "#d3d3d3"
  ) %>% 
  sg_relative_size(initial = 1)%>% 
  sg_force_stop(100000)
#Export and save as html to preserve detail 

#export to Gephi
edges <- as.data.frame(gt$edges)
nodes <- as.data.frame(unique(c(edges[,1], edges[,2])))
colnames(nodes) <- c("nodes")
g <- graph.data.frame(edges, directed = TRUE, vertices = nodes)
write.graph(g, "data/output/exIO.graphml",
            format="graphml")
#check
sources <- unique(edges$source)
targets <- unique(edges$target)