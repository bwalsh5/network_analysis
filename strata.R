library(tidyverse)
library(lubridate)

strada <- lookup_users("stradaeducation")
followers_strada <- get_followers("stradaeducation", n = strada$followers_count)
detail_followers_strada <- lookup_users(followers$user_id)
detail_followers_strada <- data.frame(lapply(detail_followers,as.character),
                             stringsAsFactors = F)

active_fol_strada <- detail_followers_strada %>% select(user_id,screen_name,created_at,followers_count,friends_count,favourites_count, text) %>%
  mutate(created_at = ymd_hms(created_at),
         followers_count = as.numeric(followers_count),
         friends_count = as.numeric(friends_count),
         favourites_count = as.numeric(favourites_count)) %>%
  filter((followers_count > 100 & followers_count < 6000), friends_count > 75, favourites_count > 10, 
         created_at > "2020-03-15") %>%
  arrange(-followers_count)

View(active_fol_strada)

### STOP HERE


regex <- "@([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"

active_fol_strada <- active_fol_strada %>% 
  # Use regular expression to identify all the usernames in a tweet
  mutate(all_mentions = str_extract_all(text, regex)) %>%
  unnest(all_mentions)


mentions_strada <-
  active_fol_strada %>%
  mutate(all_mentions = str_trim(all_mentions)) %>%
  select(sender = screen_name, all_mentions)


#edgelist <- 
 # mentions %>% 
  # remove "@" from all_mentions column
 # mutate(all_mentions = str_sub(all_mentions, start = 2)) %>% 
  # rename all_mentions to receiver
 # select(sender, receiver = all_mentions)


interactions_sent_strada <- mentions_strada %>% 
  # this counts how many times each sender appears in the data frame, effectively counting how many interactions each individual sent 
  count(sender) %>% 
  # arranges the data frame in descending order of the number of interactions sent
  arrange(desc(n))

interactions_sent_strata <- 
  interactions_sent_strada %>% 
  filter(n > 5) %>% 
  mutate(client = "Strata")

View(interactions_sent_strata)


