test1


tweet.fr %>% filter(word %in% index$word) %>% 
  ggplot(aes(word, n)) + geom_col() + coord_flip() +facet_wrap(~screen_name, ncol = 2) +
  ggtitle("The most used words in the corpus by politician")


wordcloud(words=tweet.fr$word, freq=tweet.fr$n, max.words=120, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2")) 




trump.tok %>% top_n(15) %>% 
  ggplot(aes(x = n, y = reorder(word, n)))+
  geom_bar(stat = "identity")+
  ggtitle("The most used words by Donald Trump") + ylab("word")
  
  
  
  biden.tok %>% top_n(15) %>% 
  ggplot(aes(x = n, y = reorder(word, n)))+
  geom_bar(stat = "identity")+
    ggtitle("The most used words by Joe Biden") + ylab("word")
  
  library(formattable)
  
  tweet_all %>%  select(-name) %>% 
    mutate(date_created = date(created_at)) %>% 
    filter(date_created >= "2020-10-20" & date_created <= "2020-11-17") %>%
    select(screen_name, favorite_count, retweet_count, display_text_width) %>% 
    group_by(screen_name) %>% 
    summarise(mean_favorite = mean(favorite_count),
              mean_retweet = mean(retweet_count),
              mean_text_width = mean(display_text_width)) %>% 
    arrange(desc(mean_favorite)) %>% 
    mutate(mean_retweet = round(mean_retweet, 0),
           mean_favorite = round(mean_favorite, 0),
           mean_text_width = round(mean_text_width, 0)) %>% 
    kable(caption = "General statistics", col.names = c("name","avg_like", "avg_retweet", "avg_tweet_length")) %>%
    kable_styling(
      bootstrap_options = "striped",
      full_width = F,
      position = "left"
    )
  
  library(scales)
tweet_all %>%  select(-name) %>% 
  mutate(date_created = date(created_at)) %>% 
  filter(date_created >= "2020-10-20" & date_created <= "2020-11-17") %>%
  group_by(date_created, screen_name) %>% 
  count() %>% 
  ggplot(aes( x = date_created, y = n,
              colour = screen_name ))+
  geom_line()+
  scale_y_continuous(breaks = seq(0 , 70, by = 5))+
  theme(axis.text.x = element_text(angle=45))+
  scale_x_date(breaks = date_breaks("days"),
               labels = date_format("%m/%d"))+
  ggtitle("Number of daily tweet per candidate") + ylab("nb tweets")
  