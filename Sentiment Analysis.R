#Sentiment Analysis



#required libraries
library(readxl)
library(stringr)
library(RCurl)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)


#Loading and Preparing the dataframe
reviews <- read_excel("allreviews.xlsx")
reviews$X__1 = NULL

#Creating a rating column
a <- vector(mode = "integer", length = 0)
for(x in 1:(nrow(reviews))) {
  a <- c(a, 
   str_count(reviews$title[[x]], "\\*"))
}
reviews$ratings <- a






#To look at all of our reviews
allreviews <- reviews[,2] #only keeping the comment column
allreviews <- allreviews %>% mutate(user = 1:nrow(.)) #add in column to identify different comment
allreviews_tidy <- allreviews %>% unnest_tokens(word, comment) #split sentences into words (convert to lowercase and remove punctuation)
allreviews_tidy <- allreviews_tidy %>% anti_join(stop_words) #drop the stop words

#visualize the most common words used in all of our reviews
allreviews_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



get_sentiments("bing")

#sentiment analysis for all ratings
allreviews_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(sentiment)

#words that make the strongest contributions to the overall sentiment of the reviews
allreviews_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

allreviews_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free_y") +
  coord_flip() +
  labs(x = "", y = "Sentiment count") +
  theme(legend.position = "none")




#Dividing up the reviews according to the rating 
onestar <- data.frame(Title=character(), Comment=character(), Rating=integer(), stringsAsFactors=FALSE) 
for(x in 1:(nrow(reviews))) {
  if (reviews$ratings[x] == 1) {
    onestar <- rbind(onestar, reviews[x,2])
  }
}

twostars <- data.frame(Title=character(), Comment=character(), Rating=integer(), stringsAsFactors=FALSE) 
for(x in 1:(nrow(reviews))) {
  if (reviews$ratings[x] == 2) {
    twostars <- rbind(twostars, reviews[x,2])
  }
}

threestars <- data.frame(Title=character(), Comment=character(), Rating=integer(), stringsAsFactors=FALSE) 
for(x in 1:(nrow(reviews))) {
  if (reviews$ratings[x] == 3) {
    threestars <- rbind(threestars, reviews[x,2])
  }
}

fourstars <- data.frame(Title=character(), Comment=character(), Rating=integer(), stringsAsFactors=FALSE) 
for(x in 1:(nrow(reviews))) {
  if (reviews$ratings[x] == 4) {
    fourstars <- rbind(fourstars, reviews[x,2])
  }
}

fivestars <- data.frame(Title=character(), Comment=character(), Rating=integer(), stringsAsFactors=FALSE) 
for(x in 1:(nrow(reviews))) {
  if (reviews$ratings[x] == 5) {
    fivestars <- rbind(fivestars, reviews[x,2])
  }
}






#Working with 1 star rating reviews
onestar <- onestar %>% mutate(user = 1:nrow(.)) #add in column to identify different comment
onestar_tidy <- onestar %>% unnest_tokens(word, comment) #split sentences into words (convert to lowercase and remove punctuation)
head(stop_words) #text is dominated by these stop words
onestar_tidy <- onestar_tidy %>% anti_join(stop_words) #drop the stop words

#visualize the most common words used in the comments for 1 star ratings
onestar_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 70) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



get_sentiments("bing") # Sentiment lexicons

#sentiment analysis for 1 star ratings
onestar_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(sentiment)

#words that make the strongest contributions to the overall sentiment of the reviews
onestar_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

onestar_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free_y") +
  coord_flip() +
  labs(x = "", y = "Sentiment count") +
  theme(legend.position = "none")



#Working with 2 stars rating reviews (repeat the process)
twostars <- twostars %>% mutate(user = 1:nrow(.)) 
twostars_tidy <- twostars %>% unnest_tokens(word, comment)
twostars_tidy <- twostars_tidy %>% anti_join(stop_words) 

#visualize the most common words used in the comments for 2 stars ratings
twostars_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



get_sentiments("bing")


#sentiment analysis for 2 stars ratings
twostars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(sentiment)

#words that make the strongest contributions to the overall sentiment of the reviews
twostars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

twostars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free_y") +
  coord_flip() +
  labs(x = "", y = "Sentiment count") +
  theme(legend.position = "none")




#Working with 3 stars rating reviews (repeat the process)
threestars <- threestars %>% mutate(user = 1:nrow(.)) 
threestars_tidy <- threestars %>% unnest_tokens(word, comment)
threestars_tidy <- threestars_tidy %>% anti_join(stop_words) 

#visualize the most common words used in the comments for 3 stars ratings
threestars_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



get_sentiments("bing")


#sentiment analysis for 3 stars ratings
threestars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(sentiment)

#words that make the strongest contributions to the overall sentiment of the reviews
threestars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

threestars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free_y") +
  coord_flip() +
  labs(x = "", y = "Sentiment count") +
  theme(legend.position = "none")





#Working with 4 stars rating reviews (repeat the process)
fourstars <- fourstars %>% mutate(user = 1:nrow(.)) 
fourstars_tidy <- fourstars %>% unnest_tokens(word, comment)
fourstars_tidy <- fourstars_tidy %>% anti_join(stop_words) 

#visualize the most common words used in the comments for 4 stars ratings
fourstars_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 90) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



get_sentiments("bing")


#sentiment analysis for 4 stars ratings
fourstars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(sentiment)

#words that make the strongest contributions to the overall sentiment of the reviews
fourstars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

fourstars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free_y") +
  coord_flip() +
  labs(x = "", y = "Sentiment count") +
  theme(legend.position = "none")




#Working with 5 stars rating reviews (repeat the process)
fivestars <- fivestars %>% mutate(user = 1:nrow(.)) 
fivestars_tidy <- twostars %>% unnest_tokens(word, comment)
fivestars_tidy <- twostars_tidy %>% anti_join(stop_words)

#visualize the most common words used in the comments for 2 stars ratings
fivestars_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



get_sentiments("bing")


#sentiment analysis for 5 stars ratings
fivestars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(sentiment)

#words that make the strongest contributions to the overall sentiment of the reviews
fivestars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

fivestars_tidy %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free_y") +
  coord_flip() +
  labs(x = "", y = "Sentiment count") +
  theme(legend.position = "none")
