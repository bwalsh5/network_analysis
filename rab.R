
rab <- lookup_users("saragoldrickrab")
followers_rab <- get_followers("saragoldrickrab",n = rab$followers_count,)
detail_followers_rab <- lookup_users(followers$user_id)
detail_followers_rab <- data.frame(lapply(detail_followers_rab,as.character),
                                       stringsAsFactors = F)

active_fol_rab <- detail_followers_rab %>% select(user_id,screen_name,created_at,followers_count,friends_count,favourites_count, text) %>%
  mutate(created_at = ymd_hms(created_at),
         followers_count = as.numeric(followers_count),
         friends_count = as.numeric(friends_count),
         favourites_count = as.numeric(favourites_count)) %>%
  filter((followers_count > 100 & followers_count < 6000), friends_count > 75, favourites_count > 10, 
         created_at > "2020-03-15") %>%
  arrange(-followers_count)

View(active_fol_rab)

# STOP HERE

active_fol_rab <- active_fol_rab %>% 
  # Use regular expression to identify all the usernames in a tweet
  mutate(all_mentions = str_extract_all(text, regex)) %>%
  unnest(all_mentions)


mentions_rab <-
  active_fol_rab %>%
  mutate(all_mentions = str_trim(all_mentions)) %>%
  select(sender = screen_name, all_mentions)


interactions_sent_rab<- mentions_rab %>% 
  # this counts how many times each sender appears in the data frame, effectively counting how many interactions each individual sent 
  count(sender) %>% 
  # arranges the data frame in descending order of the number of interactions sent
  arrange(desc(n))

interactions_sent_rab <- 
  interactions_sent_rab %>% 
  filter(n > 5) %>% 
  mutate(client = "rab")


View(interactions_sent_rab)


