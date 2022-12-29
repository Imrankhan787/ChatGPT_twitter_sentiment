##***************Stringr + Twitter analytics tutorial**************************##
#install.packages("rtweet")
library(rtweet)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("tidytext")
library(tidytext)
#install.packages("wordcloud")
library(wordcloud)
library(lubridate)

## First you need to have twitter account
## Secondly, then you need to have twitter developer account.
twitter_keys <- read_csv(file.choose())


token <- create_token(
  app = "ChatGPT",
  consumer_key = twitter_keys$API_key,
  consumer_secret = twitter_keys$API_secret,
  access_token = twitter_keys$Access_token,
  access_secret = twitter_keys$Access_token_secret)
token
# Bolded operators ending with a colon should be immediately followed by a word or quoted
# phrase (if appropriate)-e.g., lang:en
#********************** Keyword for queries********************************###
# "" match exact phrase
# #  hashtag
# @  mentions
# url:found in URL
# lang : language

#*****************Accounts of interest*******************###
# from: authored by
# to: sent to
# retweets_of: retweet author

#****************Tweet attributes************************###

# is:retweet ~~ only retweets
# has:mentions ~~ uses mention(s)
# has:hashtags ~~ uses hashtags(s)
# has:media ~~ includes media(s)
# has:videos ~~ includes video(s)
# has:images ~~ includes image(s)
# has:links ~~ includes URL(s)
# is:verified ~~ from verified accounts

##Explore the different arguments of search_tweets() function
ChatGPT_tweets <- search_tweets(
  q = "ChatGPT",
  n = 5000,
  include_rts = F,
  lang = "en")

ChatGPT_tweets$text ## Explore the tweets

##**************************Cleaning and Preprocessing the tweets******************************************##
##*********************************************************************************************************##

ChatGPT_tweets$text1<-str_remove_all(ChatGPT_tweets$text, pattern = "https[[\\w][\\W]&&[^\\s]]*" ) ## Remove URLs
ChatGPT_tweets$text1<-str_replace_all(ChatGPT_tweets$text1,  pattern = "\\n|@\\w*", replacement = " " )
ChatGPT_tweets$text1 <- str_replace_all(ChatGPT_tweets$text1,pattern = "\\s{2,}", replacement = " " )
ChatGPT_tweets$text1 <- str_replace_all(ChatGPT_tweets$text1,pattern = "\\d{1,}", replacement = " " )
ChatGPT_tweets$text1 <- str_to_lower(ChatGPT_tweets$text1)



##***********Text mining with R: A tidy approach*********************##
##*******************************************************************##
# Please read this book at https://www.tidytextmining.com/index.html if you are interested in text mining.

ChatGPT_tweets1 <-tibble(tweet_number = 1:length(ChatGPT_tweets$text1), 
                            tweet_text = ChatGPT_tweets$text1 )

## turns the text data into the tidy data. Now we have one word per tweet per row.
ChatGPT_tweets1  <- ChatGPT_tweets1 %>%
                    unnest_tokens(word, tweet_text) 

stop_words <- bind_rows(stop_words, tibble(word = "i.e", lexicon = "CUSTOM") ) ##added the stop word
##Removing stop words. Stop words are commonly used words that are  not helpful to draw insights from the data.
ChatGPT_tweets1 <- ChatGPT_tweets1 %>%
                   anti_join(stop_words, by = "word")

##Frequently used words in the corpus of tweets
word_frequency <- ChatGPT_tweets1 %>%
                  count(word, sort = T)

##************wordcloud***********************##
##********************************************##
ChatGPT_wordcloud <- wordcloud(word_frequency$word, 
                               word_frequency$n, 
                               max.words = 75,
                               colors = c("red","blue","green","black"),
                               random.color = T,
                               random.order = F )


##*****************Sentiment analysis********************##
## How we measure the sentiment of any text? through the usage of words
## different lexicons for classifying the words having positive or negative sentiment

bing_lexicon  <- get_sentiments("bing")
#nrc_lexicon   <- get_sentiments("nrc")
#afinn_lexicon <- get_sentiments("afinn")

ChatGPT_sentiment   <-  ChatGPT_tweets1 %>%
                        inner_join(bing_lexicon)

Positive_negative_words_freq <- ChatGPT_sentiment %>%
                                count(sentiment, sort = T)


##******************* Sentiment analysis:ChatGPT visualization*********************##
##*********************************************************************************##
ggplot(data = Positive_negative_words_freq, mapping = aes( x = as.factor(sentiment),y = n))+
  geom_col(fill = "blue")+
  scale_x_discrete(labels = c("Negative words","Positive words"))+
  labs(
    x = "Sentiment",
    y = "Word counts",## clearly we have more positive words than the negative words. Therefore, tweets are positive.
    title = "How twitter feels about ChatGPT?",
    caption = "Data source: Tweets"
    )+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.50),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
   ggsave("twitter_Sentiment1.png")
##******Top ten words contributing the positive and negative sentiments**************##

 data_for_visualization <-  ChatGPT_sentiment %>%
                            count(word, sentiment, sort = T)%>%
                            group_by(sentiment) %>%
                            slice_max(order_by = n, n = 12) %>%
                            ungroup()%>%
                            mutate(n = ifelse(sentiment == "negative",-n, n))%>%
                            mutate(word = reorder(word, n))
 
 
 ggplot(data = data_for_visualization, mapping = aes( y = as.factor(word),x = n))+
   geom_col(aes(fill = sentiment)) +
   scale_fill_discrete(name = "Sentiment", labels = c("Negative","Positive")) +
   labs(
     x = "Frequency of words",
     y = "Words",## clearly we have more positive words than the negative words. Therefore, tweets are positive.
     title = "Top ten words contributing positive and negative \nsentiments towards ChatGPT",
     caption = "Data source: Tweets"
   ) +
   theme_bw()+
   theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.50, color = "black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) 
  ggsave("Positive_negative_contribution.png")

##*****************Tweets per hour*************************##

data2<- ChatGPT_tweets %>%
        mutate(day = day(ChatGPT_tweets$created_at),
               hour = hour(ChatGPT_tweets$created_at))%>%
        select(day, hour) %>%
        group_by(day, hour) %>%
        arrange(day, hour) %>%
        count(day, hour)%>%
        ungroup()



data3 <- data2 %>%
         unite(day_hour, day, hour, sep =".") %>%
         mutate(day_hour = as.numeric(day_hour),
                n = as.numeric(n)) 

## Per hour tweet across the day
ggplot(data = data3, mapping = aes(x = day_hour, y = n) )+
  geom_smooth(se = F)+
  labs(
    x = "Sentiment",
    y = "Hourly tweets",## clearly we have more positive words than the negative words. Therefore, tweets are positive.
    title = "Hourly tweets on ChatGPT \n(Graph based on 5000 tweets)",
    caption = "Data source: Twitter"
  )+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.50),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
