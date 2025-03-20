# Sentiment Analysis on Consumer Complaints

## Overview
This project focuses on performing sentiment analysis on consumer complaint data. The analysis uses multiple sentiment lexicons such as **Bing** and **NRC**, and visualizes the results using bar plots and word clouds. This project helps to understand the emotions and sentiments expressed by consumers in their complaints.

### Features:
- Sentiment Analysis using **Bing** and **NRC** lexicons.
- Visualizations of sentiment counts using bar plots.
- Word cloud showing the most frequent terms in the dataset.

## Getting Started

### Prerequisites
Make sure you have the following libraries installed in your R environment:
```r
library(tidyverse)
library(tidytext)
library(wordcloud)
library(tm)
library(ggplot2)
library(scales)
```
## Steps to Run

### 1. Load Required Libraries
```r
library(tidyverse)
library(tidytext)
library(wordcloud)
library(tm)
library(ggplot2)
library(scales)
```

### 2. Clean the Data
Ensure your data is in a tidy format, remove null values, and convert text to lowercase.
```r
df_clean <- df %>% 
  select(Consumer.complaint.narrative) %>% 
  filter(!is.na(Consumer.complaint.narrative)) %>% 
  mutate(Consumer.complaint.narrative = str_to_lower(Consumer.complaint.narrative))
````
