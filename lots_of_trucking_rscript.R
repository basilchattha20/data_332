library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(stringr)
library(ggplot2)

rm(list = ls())

setwd('/Users/basilchattha/Documents/r_projects/lots_of_trucking')

df_truck_0001 <- read_excel('truck data 0001.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_0369 <- read_excel('truck data 0369.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1226 <- read_excel('truck data 1226.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1442 <- read_excel('truck data 1442.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1478 <- read_excel('truck data 1478.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1539 <- read_excel('truck data 1539.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1769 <- read_excel('truck data 1769.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_pay <- read_excel('Driver Pay Sheet.xlsx', .name_repair = 'universal')

df <- rbind(df_truck_0001, df_truck_0369, df_truck_1226, df_truck_1442, df_truck_1478, df_truck_1539, df_truck_1769)

df_starting_Pivot <- df %>%
  group_by(Truck.ID) %>%
  summarize(count = n())

df <- left_join(df, df_pay, by = c('Truck.ID'))

date_min <- min(df$Date)
date_max <- max(df$Date)

number_days_on_road <- as.numeric(date_max - date_min, units = "days") 

num_hours_of_driving <- sum(df$Hours)

avg_hours_daily <-num_hours_of_driving / number_days_on_road

toll_expenses <- sum(df$Tolls)
misc_expenses <- sum(df$Misc)
total_other_expenses <- sum(toll_expenses, misc_expenses)
print(total_other_expenses)

total_fuel_expenses <- sum(df$Gallons * df$Price.per.Gallon)
print(total_fuel_expenses)

total_expenses <- total_other_expenses + total_fuel_expenses
print(total_expenses)

total_gallons_consumed <- sum(df$Gallons)
print(total_gallons_consumed)

total_miles_driven <- sum(df$Odometer.Ending - df$Odometer.Beginning)
print(total_miles_driven)

avg_mileage <- total_miles_driven / total_gallons_consumed
print(avg_mileage)

total_cost_per_mile <- total_expenses / total_miles_driven
print(total_cost_per_mile)

df[c('warehouse', 'starting_city_state')] <-
  str_split_fixed(df$Starting.Location, ',', 2)

df$starting_city_state <-
  gsub(',',"", df$starting_city_state)

df[c('col1', 'col2')] <-
  str_split_fixed(df$starting_city_state, ' ', 2)

df[c('col1', 'col2', 'col3')] <-
  str_split_fixed(df$col2, ' ', 3)

df_starting_Pivot <- df%>%
  group_by(starting_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_starting_Pivot, aes(x = starting_city_state, y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))

df_pay_pivot <- df %>%
  group_by(Truck.ID) %>%
  summarize( pay = sum(labor_per_mil * total_miles_driven), .groups = "drop")

ggplot(df_pay_pivot, aes( x = Truck.ID, y = pay, fill = Truck.ID)) +
  geom_col() +
  scale_fill_brewer(palette = "set1") +
  theme_minimal()
