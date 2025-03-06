library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(stringr)

rm(list = ls())

setwd('/Users/basilchattha/Documents/r_projects/student')

df_Student <- read_excel('Student.xlsx', sheet = 1, .name_repair = 'universal')
df_Registration <- read_excel('Registration.xlsx', sheet = 1, .name_repair = 'universal')
df_course <- read_excel('Course.xlsx', sheet = 1, .name_repair = 'universal')

df_merged <- df_Student %>%
  left_join(df_Registration, by = "Student.ID")

df_merged <- df_merged %>%
  left_join(df_course, by = "Instance.ID")

df_major_counts <- df_merged %>%
  group_by(Title) %>%
  summarise(Total_Students = n(), .groups = 'drop')

df_major_counts

ggplot(df_major_counts, aes(x = Title, y = Total_Students, fill = Title)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Students by Major",
    x = "Major",
    y = "Total Students"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

df_birthday_year <- df_merged %>%
  mutate(birth_year = as.numeric(format(Birth.Date, "%Y")))

ggplot(df_birthday_year, aes(x = birth_year)) +
  geom_histogram(binwidth = 3, fill = "yellow", color = "black") +
  labs(title = "Birth Years of Student Body", x = "Birth Year", y = "Count") +
  theme_minimal()

df_payment_plan <- df_merged %>%
  mutate(Payment.Plan = case_when(
    Payment.Plan == TRUE  ~ "Installment Plan",
    Payment.Plan == FALSE ~ "Full Payment",
    TRUE ~ as.character(Payment.Plan)
  ))

total_cost <- df_payment_plan %>%
  group_by(Title, Payment.Plan) %>%
  summarise(Total_Cost = sum(Total.Cost, na.rm = TRUE)) %>%
  ungroup()

ggplot(total_cost, aes(x = reorder(Title, -Total_Cost), y = Total_Cost, fill = Payment.Plan)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Total Cost of Attendance by Major and Payment Plan",
       x = "Major",
       y = "Total Cost ($)",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

total_balance <- df_payment_plan %>%
  group_by(Title, Payment.Plan) %>%
  summarise(Total_Balance_Due = sum(Balance.Due, na.rm = TRUE)) %>%
  ungroup()

ggplot(total_balance, aes(x = reorder(Title, -Total_Balance_Due), y = Total_Balance_Due, fill = Payment.Plan)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Total Balance Due by Major and Payment Plan",
       x = "Major",
       y = "Total Balance Due ($)",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_fill_brewer(palette = "Set2")
