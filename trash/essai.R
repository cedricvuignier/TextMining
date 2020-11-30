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


tweet.div %>% 
  ggplot(aes(x=reorder(document, I), y=I))+geom_point()+coord_flip()+
  xlab("Text") + ylab("Yule's index")+
  ggtitle("Yule's index for each document of the corpus") + xlab("corpus Doc")
  


 # check the result
textplot_xray(kwic(tweet.cp, pattern = "trump"),
              kwic(tweet.cp, pattern = "biden"))

textplot_xray(kwic(tweet.cp, pattern = "covid"),
              kwic(tweet.cp, pattern = "health"))

textplot_xray(kwic(tweet.cp, pattern = "fake"),
              kwic(tweet.cp, pattern = "fraud"))
#########keyness
tweet.key <- tokens(corpus_subset(corpus_key, screen_name %in% c("realDonaldTrump", "JoeBiden")),
                    remove_punct = TRUE,
                    remove_symbols = TRUE,
                    remove_numbers = TRUE) %>%
  tokens_replace(pattern=hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_remove(pattern = c("s", "amp", "de","la", "en", "t","re","gsgsgh", "gsfsghkmdm", stopwords("english")))

tweet.key %>% dfm(groups="screen_name") %>% textstat_keyness(target="realDonaldTrump") %>% textplot_keyness(n = 15, 
                                                                                                            color = c("red", "blue"), labelcolor = "black", 
                                                                                                            labelsize = 4, margin = 0.2)

#create the variable "party"
party <- c("republican", "republican", "democrate", "democrate" , "democrate", "democrate")
corpus_key_party <- cbind(corpus,party)
corpus_key_party <- corpus(corpus_key_party,text_field = "text")

## Compare the tweets of republican vs to the tweets of democrat in terms of keyness
tweet.key_party <- tokens(corpus_subset(corpus_key_party, party %in% c("democrate", "republican")),
                          remove_punct = TRUE,
                          remove_symbols = TRUE,
                          remove_numbers = TRUE) %>%
  tokens_replace(pattern=hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_remove(pattern = c("s", "amp", "de","la", "en", "t", "u", "re", "http", stopwords("english")))
tweet.key_party %>% dfm(groups="party") %>% textstat_keyness(target="republican") %>% textplot_keyness(n = 15, 
                                                                                                       color = c("red", "blue"), labelcolor = "black", 
                                                                                                       labelsize = 4, margin = 0.2)

## Compare the tweets of kamala vs to the tweets of vp in terms of keyness
tweet.key2 <- tokens(corpus_subset(corpus_key_party, screen_name %in% c("KamalaHarris", "VP")),
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE) %>%
  tokens_replace(pattern=hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_remove(pattern = c("s", "amp", "de","la", "en", "t", "https", stopwords("english")))
tweet.key2 %>% dfm(groups="screen_name") %>% textstat_keyness(target="KamalaHarris") %>% textplot_keyness(n = 15, 
                                                                                                     color = c("red", "blue"), labelcolor = "black", 
                                                                                                          labelsize = 4, margin = 0.2)
#before after
corpus <- read_csv("data supervised_L/corpus_final_before_after.csv")
corpus_key_party <- corpus(corpus)

tweet.key3 <- tokens(corpus_subset(corpus_key_party, when %in% c("before", "after")),
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE) %>%
  tokens_replace(pattern=hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_remove(pattern = c("s", "amp", "de","la", "en", "t", "https", "http", "et", "co", stopwords("english")))
tweet.key3 %>% dfm(groups="when") %>% textstat_keyness(target="before") %>% textplot_keyness(n = 15, 
                                                                                             color = c("black", "grey"), labelcolor = "black", 
                                                                                             labelsize = 4, margin = 0.2)
