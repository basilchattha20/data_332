library(tidyverse)
library(tidytext)
library(wordcloud)
library(tm)
library(ggplot2)
library(scales)

setwd('/Users/basilchattha/Documents/r_projects/text_analysis')

df <- read.csv('data/Consumer_Complaints.csv',stringsAsFactors = FALSE)
head(df)

# Checking for nulls
any(is.na(df))  
# No Nulls

# Select the 'Consumer.complaint.narrative' column from the dataframe
tidy_text <- df %>% 
  dplyr::select(Consumer.complaint.narrative) %>%  
  
  # Tokenize the 'Consumer.complaint.narrative' column into individual words
  tidytext::unnest_tokens(output = "word", input = Consumer.complaint.narrative) %>%  
  
  # Convert all words to lowercase to ensure uniformity
  dplyr::mutate(word = stringr::str_to_lower(word)) %>%  
  
  # Remove common stop words (such as "the", "and", etc.) that don’t add much analytical value
  dplyr::anti_join(tidytext::stop_words)  

# Remove any rows where the word is exactly 'xxxx' (likely a placeholder or unwanted word)
tidy_text <- tidy_text %>% 
  dplyr::filter(word != "xxxx")  

# Count the frequency of each word and sort the results in descending order
word_counts <- tidy_text %>% 
  dplyr::count(word) %>%  # Count occurrences of each word
  dplyr::arrange(dplyr::desc(n))  # Sort by frequency, most frequent words first

# Filter words that appear more than 25,000 times and prepare them for plotting
word_counts %>% 
  dplyr::filter(n > 25000) %>%  # Keep only words with more than 25,000 occurrences
  dplyr::mutate(word = factor(word, levels = word[order(n)])) %>%  # Reorder words based on frequency
  ggplot2::ggplot(ggplot2::aes(x = n, y = word)) +  # Set up the plot with word counts on the x-axis and words on the y-axis
  ggplot2::geom_col() +  # Create a bar plot (geom_col) to visualize word frequencies
  ggplot2::labs(y = NULL)  # Remove the y-axis label since the y-axis contains words

# Perform sentiment analysis by joining the tidy_text data with the Bing sentiment lexicon and counting sentiment occurrences
bing_sentiment <- tidy_text %>% 
  dplyr::inner_join(get_sentiments("bing")) %>%  # Join the tidy_text with the Bing sentiment lexicon
  dplyr::count(sentiment)  # Count the occurrences of each sentiment (positive/negative)

# Create a bar plot to visualize the sentiment analysis results
bing_sentiment %>% 
  ggplot2::ggplot(aes(x = sentiment, y = n, fill = sentiment)) +  # Set up the plot with 'sentiment' on the x-axis and count on the y-axis
  ggplot2::geom_bar(stat = "identity", show.legend = FALSE) +  # Use a bar plot to show sentiment count, hide the legend for clarity
  ggplot2::theme_minimal() +  # Apply a minimal theme for a cleaner look
  ggplot2::ggtitle("Sentiment Analysis of Complaints using Bing Lexicon") +  # Add a more descriptive title to the plot
  ggplot2::scale_y_continuous(labels = scales::comma)  # Format y-axis with commas for better readability of large numbers

# Perform sentiment analysis by joining the tidy_text data with the NRC sentiment lexicon and counting sentiment occurrences
nrc_sentiment <- tidy_text %>% 
  dplyr::inner_join(get_sentiments("nrc")) %>%  # Join the tidy_text with the NRC sentiment lexicon
  dplyr::count(sentiment)  # Count the occurrences of each sentiment in the NRC lexicon

# Create a horizontal bar plot to visualize the NRC sentiment analysis results
nrc_sentiment %>% 
  ggplot2::ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +  # Set up the plot with sentiment on the x-axis and count on the y-axis, reorder based on count
  ggplot2::geom_bar(stat = "identity", show.legend = FALSE) +  # Use a bar plot and hide the legend for clarity
  ggplot2::coord_flip() +  # Flip the coordinates to make it a horizontal bar plot
  ggplot2::theme_minimal() +  # Apply a minimal theme for a cleaner look
  ggplot2::ggtitle("Sentiment Analysis of Complaints using NRC Lexicon")  # Add a more descriptive title

# Count the frequency of each word in the tidy_text data and sort by frequency in descending order
word_freq <- tidy_text %>% 
  dplyr::count(word, sort = TRUE)  # Count occurrences of each word and sort them by frequency

# Create a word cloud to visualize the most frequent words
wordcloud::wordcloud(words = word_freq$word,  # Pass the words to the wordcloud function
                     freq = word_freq$n,  # Pass the frequency of the words
                     max.words = 100,  # Limit the word cloud to the top 100 most frequent words
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))  # Use a color palette for the word cloud
