library(tidyverse)
library(lubridate)
pearson <- lookup_users("Pearson")
followers_pearson <- get_followers("Pearson",n = pearson$followers_count)
detail_followers_pearson <- lookup_users(followers_pearson$user_id)
detail_followers_pearson <- data.frame(lapply(detail_followers_pearson,as.character),
                               stringsAsFactors = F)

active_fol_pearson <- detail_followers_pearson %>% select(user_id,screen_name,created_at,followers_count,friends_count,favourites_count, text) %>%
  mutate(created_at = ymd_hms(created_at),
         followers_count = as.numeric(followers_count),
         friends_count = as.numeric(friends_count),
         favourites_count = as.numeric(favourites_count)) %>%
  filter((followers_count > 100 & followers_count < 6000), friends_count > 75, favourites_count > 10, 
         created_at > "2020-03-15") %>%
  arrange(-followers_count)

View(active_fol_pearson)

#### STOP HERE


regex <- "@([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"

active_fol_pearson <- active_fol_pearson %>% 
  # Use regular expression to identify all the usernames in a tweet
  mutate(all_mentions = str_extract_all(text, regex)) %>%
  unnest(all_mentions)


mentions_pearson <-
  active_fol_pearson %>%
  mutate(all_mentions = str_trim(all_mentions)) %>%
  select(sender = screen_name, all_mentions)


interactions_sent_pearson <- mentions_pearson %>% 
  # this counts how many times each sender appears in the data frame, effectively counting how many interactions each individual sent 
  count(sender) %>% 
  # arranges the data frame in descending order of the number of interactions sent
  arrange(desc(n))

interactions_sent_pearson <- 
  interactions_sent_pearson %>% 
  filter(n > 5) %>% 
  mutate(client = "Pearson")


View(interactions_sent_pearson)


