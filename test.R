
me <- lookup_users("velshnia")
followers_me <- get_followers("velshnia",n = me$followers_count)
View(followers_me)
detail_followers_me <- lookup_users(followers_me$user_id)
detail_followers_me <- data.frame(lapply(detail_followers_me,as.character),
                                   stringsAsFactors = F)
View(detail_followers_me)
active_fol_me <- detail_followers_me %>% select(user_id,screen_name,created_at,followers_count,friends_count,favourites_count, text) %>%
  mutate(created_at = ymd_hms(created_at),
         followers_count = as.numeric(followers_count),
         friends_count = as.numeric(friends_count),
         favourites_count = as.numeric(favourites_count)) %>%
  filter((followers_count > 100 & followers_count < 6000), friends_count > 75, favourites_count > 10, 
         created_at > "2020-03-15") %>%
  arrange(-followers_count)

View(active_fol_me)

active_fol_me <- active_fol_me %>% 
  # Use regular expression to identify all the usernames in a tweet
  mutate(all_mentions = str_extract_all(text, regex)) %>%
  unnest(all_mentions)




mentions_me <-
  active_fol_me %>%
  mutate(all_mentions = str_trim(all_mentions)) %>%
  select(sender = screen_name, all_mentions)

View(mentions_me)

interactions_sent_me <- mentions_me %>% 
  # this counts how many times each sender appears in the data frame, effectively counting how many interactions each individual sent 
  count(sender) %>% 
  # arranges the data frame in descending order of the number of interactions sent
  arrange(desc(n))


# interactions_sent_me <- interactions_sent_me %>% 
#  filter(n > 5) %>% 
#  mutate(client = "velshnia")


View(interactions_sent_me)


