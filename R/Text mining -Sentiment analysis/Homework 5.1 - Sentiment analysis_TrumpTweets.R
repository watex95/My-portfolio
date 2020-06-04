
# Load the libraries
library(twitteR)
library(dplyr)
library(splitstackshape)
library(tidytext)
library(purrr)

# Load the blackboard dataset 
TrumpTweets=read.csv("trump_tweets3.csv",header = T,stringsAsFactors = FALSE)
head(TrumpTweets)

# To prepare our data for classification, let's get rid of links and
# format dataframe in a way when only one word is in line.
TrumpTweets <- TrumpTweets[-(grep('t.co', TrumpTweets$'text')),]
TrumpTweets$tweet <- 'tweet'
TrumpTweets <- TrumpTweets[ , c('text', 'tweet')]
TrumpTweets <- unnest_tokens(TrumpTweets, word, text)
tail(TrumpTweets)
head(TrumpTweets)

# It's obvious that dataframe also contains various words without useful content.
# So it's a good idea to get rid of them.
TrumpTweets <- anti_join(TrumpTweets, stop_words, by = c('word' = 'word'))

tail(TrumpTweets)

head(TrumpTweets)


# Let's see how many times each word appears in Donald Trump's tweets.
word_count <- dplyr::count(TrumpTweets, word, sort = TRUE)
head(word_count)



# Using the BING Lexicons
# --------------------------
# Now it's time to create some dataframe with sentiments that will be used for tweets
# classification. We will use bing dictionary although you can easily use any other source.
sentiments_bing <-get_sentiments("bing")
head(sentiments_bing)
sentiments_bing <- dplyr::select(sentiments_bing, word, sentiment)
TrumpTweets_sentiments_bing <- merge(word_count, sentiments_bing, by.x = c('word'), by.y = c('word'))
head(TrumpTweets_sentiments_bing)

# Above we did a simple classification of Trump's tweets words using our sentiment
# bag of words. And this is how the result looks:


# Let's look at the number of occurrences per sentiment in tweets.
sentiments_count_bing <- dplyr::count(TrumpTweets_sentiments_bing, sentiment, sort = TRUE)
sentiments_count_bing


# We also may want to know the total count and percentage of all the sentiments.
sentiments_sum <- sum(sentiments_count_bing$'n')
sentiments_count_bing$'percentage' <- sentiments_count_bing$'n' / sentiments_sum

# Let's now create an ordered dataframe for plotting counts of sentiments.
sentiments_count_bing <- rbind(sentiments_count_bing)

# sentiment_count <- sentiment_count[order(sentiment_count$sentiment), ]
sentiments_count_bing


# And now it's time for the visualization. We will plot the results of our classifier.

sentiments_count_bing$'colour' <- as.integer(4)
barplot(sentiments_count_bing$'n', names.arg = sentiments_count_bing$'sentiment', col = sentiments_count_bing$'colour', cex.names = .5)


barplot(sentiments_count_bing$'percentage', names.arg = sentiments_count_bing$'sentiment', col = sentiments_count_bing$'colour', cex.names = .5)





# Using the nrc Lexicons
# --------------------------
# Now it's time to create some dataframe with sentiments that will be used for tweets
# classification. We will use bing dictionary although you can easily use any other source.
sentiments_nrc <-get_sentiments("nrc")
head(sentiments_nrc)

sentiments_nrc <- dplyr::select(sentiments_nrc, word, sentiment)
head(sentiments_nrc)
TrumpTweets_sentiments <- merge(word_count, sentiments_nrc, by.x = c('word'), by.y = c('word'))

# Above we did a simple classification of Trump's tweets words using our sentiment
# bag of words. And this is how the result looks:
TrumpTweets_sentiments_nrc


# Let's look at the number of occurrences per sentiment in tweets.

sentiments_count <- dplyr::count(TrumpTweets_sentiments, sentiment, sort = TRUE)
sentiments_count


# We also may want to know the total count and percentage of all the sentiments.
sentiments_sum <- sum(sentiments_count$'n')
sentiments_count$'percentage' <- sentiments_count$'n' / sentiments_sum

# Let's now create an ordered dataframe for plotting counts of sentiments.
sentiment_count <- rbind(sentiments_count)

# sentiment_count <- sentiment_count[order(sentiment_count$sentiment), ]
sentiment_count


# And now it's time for the visualization. We will plot the results of our classifier.

sentiment_count$'colour' <- as.integer(4)
barplot(sentiment_count$'n', names.arg = sentiment_count$'sentiment', col = sentiment_count$'colour', cex.names = .5)

barplot(sentiment_count$'percentage', names.arg = sentiment_count$'sentiment', col = sentiment_count$'colour', cex.names = .5)



# AFINN lexicons

# Now it's time to create some dataframe with sentiments that will be used for tweets
# classification. We will use bing dictionary although you can easily use any other source.
sentiments_afinn <-get_sentiments("afinn")
head(sentiments_afinn)

sentiments_afinn <- dplyr::select(sentiments_afinn, word, value)
head(sentiments_afinn)
TrumpTweets_sentiments_afinn <- merge(word_count, sentiments_afinn, by.x = c('word'), by.y = c('word'))

# Above we did a simple classification of Trump's tweets words using our sentiment
# bag of words. And this is how the result looks:
TrumpTweets_sentiments_afinn

# Let's look at the number of occurrences per sentiment in tweets.

sentiments_count_afinn <- dplyr::count(TrumpTweets_sentiments_afinn, value, sort = TRUE)
sentiments_count_afinn


# We also may want to know the total count and percentage of all the sentiments.
sentiments_sum <- sum(sentiments_count_afinn$'n')
sentiments_count_afinn$'percentage' <- sentiments_count_afinn$'n' / sentiments_sum

# Let's now create an ordered dataframe for plotting counts of sentiments.
sentiment_count <- rbind(sentiments_count_afinn)
sentiment_count <- sentiment_count[order(sentiments_count_afinn$value), ]
sentiment_count



# And now it's time for the visualization. We will plot the results of our classifier.

sentiment_count$'colour' <- as.integer(4)
barplot(sentiment_count$'n', names.arg = sentiment_count$'sentiment_value', col = sentiment_count$'colour', cex.names = .5)

barplot(sentiment_count$'percentage', names.arg = sentiment_count$'sentiment_value', col = sentiment_count$'colour', cex.names = .5)



afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")




# WORD CLOUD
# Let's look at the most common words in Trump's tweets as a whole again,
# but this time as a wordcloud 
library(wordcloud)
TrumpTweets %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Show the most common positive and negative words used
library(reshape2)
TrumpTweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)







