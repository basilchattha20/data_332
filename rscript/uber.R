# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(leaflet)
library(DT)

# --------- Load and Prepare Data ---------
setwd("/Users/basilchattha/Documents/r_projects/uber")

# Read the Excel files
df1 <- read_excel("data/uber-raw-data-apr14.xlsx")
df2 <- read_excel("data/uber-raw-data-jul14.xlsx")
df3 <- read_excel("data/uber-raw-data-aug14.xlsx")

# Combine and clean data
uber_df <- bind_rows(df1, df2, df3) %>%
  rename(timestamp = `Date/Time`) %>%
  mutate(
    timestamp = mdy_hms(timestamp),
    hour = hour(timestamp),
    day = day(timestamp),
    month = month(timestamp),
    month_name = month(timestamp, label = TRUE, abbr = TRUE),
    weekday = wday(timestamp, label = TRUE, abbr = TRUE),
    week_num = isoweek(timestamp)
  )

uber_df$month <- as.integer(uber_df$month)

# --------- UI ---------
ui <- navbarPage("Uber Trips Explorer",
                 
                 tabPanel("Visual Insights",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("month_pick", "Select Month:", choices = unique(as.character(uber_df$month_name)))
                            ),
                            mainPanel(
                              h4("Hourly Trip Distribution"),
                              plotOutput("hourly_plot"),
                              
                              h4("All-Months Hourly Pattern"),
                              plotOutput("all_hours_plot"),
                              
                              h4("Trips per Day of Month"),
                              plotOutput("daily_count_plot"),
                              
                              h4("Weekday vs Month Trends"),
                              plotOutput("weekday_month_plot"),
                              
                              h4("Trips per Dispatch Base per Month"),
                              plotOutput("base_month_plot"),
                              
                              h4("Monthly Totals"),
                              plotOutput("monthly_totals_plot")
                            )
                          )
                 ),
                 
                 tabPanel("Heatmaps",
                          fluidPage(
                            h4("Hourly Demand by Weekday"),
                            plotOutput("heat_hour_day"),
                            
                            h4("Month vs Day Heatmap"),
                            plotOutput("heat_month_day"),
                            
                            h4("Trips by Week and Month"),
                            plotOutput("heat_month_week"),
                            
                            h4("Base Activity by Weekday"),
                            plotOutput("heat_base_day")
                          )
                 ),
                 
                 tabPanel("Trip Summary Table",
                          fluidPage(
                            h4("Trips Per Day (1–31)"),
                            dataTableOutput("trip_table")
                          )
                 ),
                 
                 tabPanel("Geo Map",
                          leafletOutput("uber_map", height = 500)
                 ),
                 
                 tabPanel("Trip Prediction",
                          fluidPage(
                            h4("Predicting Trips by Hour"),
                            plotOutput("model_plot")
                          )
                 )
)

# --------- Server ---------
server <- function(input, output, session) {
  
  month_filtered <- reactive({
    uber_df %>% filter(as.character(month_name) == input$month_pick)
  })
  
  output$hourly_plot <- renderPlot({
    month_filtered() %>%
      count(hour) %>%
      ggplot(aes(x = hour, y = n)) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      labs(title = "Hourly Trips in Selected Month", x = "Hour", y = "Trip Count") +
      theme_minimal()
  })
  
  output$all_hours_plot <- renderPlot({
    uber_df %>%
      count(hour) %>%
      ggplot(aes(x = hour, y = n)) +
      geom_col(fill = "#E69F00") +
      labs(title = "Hourly Trips Across All Months", x = "Hour", y = "Total Trips") +
      theme_minimal()
  })
  
  output$daily_count_plot <- renderPlot({
    month_filtered() %>%
      count(day) %>%
      ggplot(aes(x = day, y = n)) +
      geom_col(fill = "#009E73") +
      labs(title = "Trips by Day of Month", x = "Day", y = "Trips") +
      theme_minimal()
  })
  
  output$weekday_month_plot <- renderPlot({
    uber_df %>%
      count(weekday, month_name) %>%
      ggplot(aes(x = weekday, y = n, fill = month_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Weekday vs Month Trip Trends", x = "Weekday", y = "Trip Count") +
      theme_minimal()
  })
  
  output$base_month_plot <- renderPlot({
    uber_df %>%
      count(Base, month_name) %>%
      ggplot(aes(x = Base, y = n, fill = month_name)) +
      geom_col(position = "dodge") +
      labs(title = "Trips by Base and Month", x = "Base Code", y = "Trips") +
      theme_minimal()
  })
  
  output$monthly_totals_plot <- renderPlot({
    uber_df %>%
      count(month_name) %>%
      ggplot(aes(x = month_name, y = n)) +
      geom_col(fill = "#D55E00") +
      labs(title = "Monthly Trip Volume", x = "Month", y = "Total Trips") +
      theme_minimal()
  })
  
  output$heat_hour_day <- renderPlot({
    uber_df %>%
      count(hour, weekday) %>%
      ggplot(aes(x = hour, y = weekday, fill = n)) +
      geom_tile() +
      labs(title = "Trips by Hour and Day", x = "Hour", y = "Day of Week") +
      theme_minimal()
  })
  
  output$heat_month_day <- renderPlot({
    uber_df %>%
      count(month_name, day) %>%
      ggplot(aes(x = day, y = month_name, fill = n)) +
      geom_tile() +
      labs(title = "Trips by Day and Month", x = "Day", y = "Month") +
      theme_minimal()
  })
  
  output$heat_month_week <- renderPlot({
    uber_df %>%
      count(month_name, week_num) %>%
      ggplot(aes(x = week_num, y = month_name, fill = n)) +
      geom_tile() +
      labs(title = "Trips by Week and Month", x = "Week #", y = "Month") +
      theme_minimal()
  })
  
  output$heat_base_day <- renderPlot({
    uber_df %>%
      count(Base, weekday) %>%
      ggplot(aes(x = weekday, y = Base, fill = n)) +
      geom_tile() +
      labs(title = "Base vs Day of Week", x = "Day", y = "Base") +
      theme_minimal()
  })
  
  output$uber_map <- renderLeaflet({
    leaflet(uber_df[1:500, ]) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Lon,
        lat = ~Lat,
        radius = 2,
        color = "#2b83ba",
        fillOpacity = 0.5,
        clusterOptions = markerClusterOptions()
      )
  })
  
  output$trip_table <- renderDataTable({
    uber_df %>%
      count(day) %>%
      arrange(day) %>%
      rename(`Day` = day, `Trips` = n)
  })
  
  output$model_plot <- renderPlot({
    model_data <- month_filtered() %>% count(hour)
    model_fit <- lm(n ~ hour, data = model_data)
    ggplot(model_data, aes(x = hour, y = n)) +
      geom_point(color = "darkblue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Linear Regression: Hour vs Trip Count", x = "Hour", y = "Trips") +
      theme_minimal()
  })
}

# Launch the application
shinyApp(ui, server)

