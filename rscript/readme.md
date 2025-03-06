# Student Data Analysis

## Loading necessary libraries
```r
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(stringr)
```

## Reading in data
```r
df_Student <- read_excel('Student.xlsx', sheet = 1, .name_repair = 'universal')
df_Registration <- read_excel('Registration.xlsx', sheet = 1, .name_repair = 'universal')
df_course <- read_excel('Course.xlsx', sheet = 1, .name_repair = 'universal')
```

## 1. Left join dataframes together for analysis
```r
df_merged <- df_Student %>%
  left_join(df_Registration, by = "Student.ID")

df_merged <- df_merged %>%
  left_join(df_course, by = "Instance.ID")
```

## 2. Counting students by major
```r
df_major_counts <- df_merged %>%
  group_by(Title) %>%
  summarise(Total_Students = n(), .groups = 'drop')
```
## Plot chart
```r
ggplot(df_major_counts, aes(x = Title, y = Total_Students, fill = Title)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Students by Major",
    x = "Major",
    y = "Total Students"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
<img src="" alt="Alt text" width="400">


