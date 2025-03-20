##Project Title: Sentiment Analysis on Consumer Complaints

Overview

This project focuses on performing sentiment analysis on consumer complaint data. The analysis uses multiple sentiment lexicons such as Bing and NRC, and visualizes the results using bar plots and word clouds. This project helps to understand the emotions and sentiments expressed by consumers in their complaints.

Features:
Sentiment Analysis using Bing and NRC lexicons.
Visualizations of sentiment counts using bar plots.
Word cloud showing the most frequent terms in the dataset.
Getting Started

Prerequisites
Make sure you have the following libraries installed in your R environment:

install.packages("tidyverse")
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tidytext")
Data
The dataset used for this analysis is based on consumer complaints. Ensure your dataset is in a clean format and consists of complaint narratives (e.g., Consumer.complaint.narrative).

Steps to Run

1. Load Required Libraries
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
2. Clean the Data
Ensure your data is in a tidy format, remove null values, and convert text to lowercase.

df_clean <- df %>% 
  select(Consumer.complaint.narrative) %>% 
  filter(!is.na(Consumer.complaint.narrative)) %>% 
  mutate(Consumer.complaint.narrative = str_to_lower(Consumer.complaint.narrative))
3. Tokenization and Stop Word Removal
Tokenize the text data and remove stop words:

tidy_text <- df_clean %>% 
  unnest_tokens(word, Consumer.complaint.narrative) %>% 
  anti_join(stop_words)
4. Sentiment Analysis (Bing Lexicon)
Perform sentiment analysis using the Bing lexicon:

bing_sentiment <- tidy_text %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment)

bing_sentiment %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Sentiment Analysis using Bing") +
  scale_y_continuous(labels = scales::comma)
<!-- Placeholder for image -->

5. Sentiment Analysis (NRC Lexicon)
Perform sentiment analysis using the NRC lexicon:

nrc_sentiment <- tidy_text %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment)

nrc_sentiment %>% 
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Sentiment Analysis using NRC") 
<!-- Placeholder for image -->

6. Word Cloud Visualization
Generate a word cloud to visualize the most frequent words in the dataset:

word_freq <- tidy_text %>% count(word, sort = TRUE)

wordcloud(words = word_freq$word, 
          freq = word_freq$n, 
          max.words = 100, 
          colors = brewer.pal(8, "Dark2"))
<!-- Placeholder for image -->

Results

Sentiment Distribution:
The Bing lexicon categorizes words as either "positive" or "negative." The bar plot shows the distribution of sentiments in the consumer complaints.
Emotional Sentiment (NRC):
The NRC lexicon classifies words into multiple emotional categories like "joy," "anger," "fear," etc. The horizontal bar plot illustrates the frequency of each sentiment type in the dataset.
Frequent Terms:
The word cloud displays the most frequently occurring words in the complaint narratives, providing insights into the common themes in the complaints.
