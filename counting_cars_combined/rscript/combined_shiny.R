library(rsconnect)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(shiny)
library(DT)
library(plotly)
library(bslib)
library(shinycssloaders)
library(httr)

rm(list = ls())

# setting working directory
setwd('/Users/basilchattha/Documents/r_projects/counting_cars_combined/data')

# reading files in
df1 <- read_excel("basildata.xlsx")
df2 <- read_excel("nisrinedata.xlsx")
df3 <- read_excel("tannerdata.xlsx")
df4 <- read_excel("ahbibdata.xlsx")
df5 <- read_excel("nickdata.xlsx")
df6 <- read_excel("tommydata.xlsx")

#comparing column names
names(df1)
names(df2)
names(df3)
names(df4)
names(df5)
names(df6)

# cleaning df1
df1_clean <- df1 %>%
  rename(
    Vehicle_Type = vehicle_type,
    Initial_Speed = init_speed,
    Final_Speed = final_speed,
    Speed_Change = speed_change,
    Location = location,
    Weather = weather,
    Recorder = recorder,
    Flashing = flashing
  ) %>%
  mutate(
    Date = NA,
    Time = NA,
    Color = NA
  ) %>%
  select(Vehicle_Type, Initial_Speed, Final_Speed, Speed_Change, Location, Weather, Recorder, Flashing, Date, Time, Color)

# cleaning df2
df2_clean <- df2 %>%
  rename(
    Date = Date,
    Time = Time,
    Final_Speed = `Speed (mph)`,
    Color = Color,
    Recorder = Observer
  ) %>%
  mutate(
    Vehicle_Type = NA,
    Initial_Speed = NA,
    Speed_Change = NA,
    Location = NA,
    Weather = NA,
    Flashing = NA
  ) %>%
  select(Vehicle_Type, Initial_Speed, Final_Speed, Speed_Change, Location, Weather, Recorder, Flashing, Date, Time, Color)

# cleaning df3
df3_clean <- df3 %>%
  rename(
    Vehicle_Type = Type_of_Car,
    Initial_Speed = Initial_Read,
    Final_Speed = Final_Read,
    Speed_Change = Difference_In_Readings,
    Location = Location,
    Weather = Weather,
    Recorder = Name,
    Flashing = Commercial_yes_no,
    Date = Date_Recorded,
    Time = Time_Recorded
  ) %>%
  mutate(
    Color = NA
  ) %>%
  select(Vehicle_Type, Initial_Speed, Final_Speed, Speed_Change, Location, Weather, Recorder, Flashing, Date, Time, Color)

# cleaning df4
df4_clean <- df4 %>%
  rename(
    Initial_Speed = Initial_Speed,
    Final_Speed = Final_Speed,
    Speed_Change = Difference,
    Vehicle_Type = Body_Style
  ) %>%
  mutate(
    Location = NA,
    Weather = NA,
    Recorder = NA,
    Flashing = NA,
    Date = NA,
    Time = NA,
    Color = NA
  ) %>%
  select(Vehicle_Type, Initial_Speed, Final_Speed, Speed_Change, Location, Weather, Recorder, Flashing, Date, Time, Color)

# cleaning df5
df5_clean <- df5 %>%
  rename(
    Recorder = student,
    Date = date,
    Final_Speed = mph,
    Vehicle_Type = vehicle_style,
    Time = `hr:min`,
    Flashing = `if_they_slow_down_(YES/ NO)`
  ) %>%
  mutate(
    Initial_Speed = NA,
    Speed_Change = NA,
    Location = NA,
    Weather = NA,
    Color = brand
  ) %>%
  select(Vehicle_Type, Initial_Speed, Final_Speed, Speed_Change, Location, Weather, Recorder, Flashing, Date, Time, Color)

# cleaning df6
df6_clean <- df6 %>%
  rename(
    Final_Speed = Speed,
    Weather = Weather,
    Date = Date,
    Color = Color,
    Vehicle_Type = `Type of Car`,
    Recorder = `Collector Name`,
    Time = `Time of the day`
  ) %>%
  mutate(
    Initial_Speed = NA,
    Speed_Change = NA,
    Location = NA,
    Flashing = NA
  ) %>%
  select(Vehicle_Type, Initial_Speed, Final_Speed, Speed_Change, Location, Weather, Recorder, Flashing, Date, Time, Color)

# making everything to character so wont encounter issue with binding
df1_clean <- df1_clean %>% mutate(across(everything(), as.character))
df2_clean <- df2_clean %>% mutate(across(everything(), as.character))
df3_clean <- df3_clean %>% mutate(across(everything(), as.character))
df4_clean <- df4_clean %>% mutate(across(everything(), as.character))
df5_clean <- df5_clean %>% mutate(across(everything(), as.character))
df6_clean <- df6_clean %>% mutate(across(everything(), as.character))

# bind all cleaned dataframes together
final_df <- bind_rows(df1_clean, df2_clean, df3_clean, df4_clean, df5_clean, df6_clean)

# view the final combined dataframe
View(final_df)

# Shiny app

df <- final_df

ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly", base_font = font_google("Roboto Mono")),
  
  titlePanel("🚘 Vehicle Speed Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("vehicle_filter", "Select Vehicle Types:", choices = NULL)
    ),
    
    mainPanel(
      fluidRow(
        column(4, wellPanel(h4("Total Observations"), textOutput("total_obs"))),
        column(4, wellPanel(h4("Avg Initial Speed"), textOutput("avg_init"))),
        column(4, wellPanel(h4("Avg Final Speed"), textOutput("avg_final")))
      ),
      
      tabsetPanel(
        tabPanel("Scatter Plot", withSpinner(plotlyOutput("speed_plot"))),
        tabPanel("Bar Chart & Averages",
                 withSpinner(plotOutput("bar_chart")),
                 h4("Average Initial Speeds by Vehicle Type"),
                 DTOutput("avg_init_speed_table"),
                 h4("Average Final Speeds by Vehicle Type"),
                 DTOutput("avg_final_speed_table")
        ),
        tabPanel("Flashing Sign Effect",
                 h4("Effect of Flashing Sign on Speed (Flashing = 1)"),
                 withSpinner(plotOutput("flashing_effect_plot"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    updateCheckboxGroupInput(session, "vehicle_filter",
                             choices = unique(df$Vehicle_Type),
                             selected = unique(df$Vehicle_Type))
  })
  
  filtered_data <- reactive({
    req(input$vehicle_filter)
    df %>% filter(Vehicle_Type %in% input$vehicle_filter)
  })
  
  output$total_obs <- renderText({ nrow(filtered_data()) })
  
  output$avg_init <- renderText({ 
    init_vals <- as.numeric(filtered_data()$Initial_Speed)
    round(mean(init_vals, na.rm = TRUE), 2)
  })
  
  output$avg_final <- renderText({ 
    final_vals <- as.numeric(filtered_data()$Final_Speed)
    round(mean(final_vals, na.rm = TRUE), 2)
  })
  
  output$speed_plot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = as.numeric(Initial_Speed), 
                                      y = as.numeric(Final_Speed), 
                                      color = Vehicle_Type)) +
      geom_point(alpha = 0.7, size = 3) +
      labs(title = "Initial vs Final Speed",
           x = "Initial Speed",
           y = "Final Speed",
           color = "Vehicle Type") +
      theme_minimal()
    ggplotly(gg)
  })
  
  output$bar_chart <- renderPlot({
    avg_df <- filtered_data() %>%
      filter(Speed_Change == 1) %>%
      group_by(Vehicle_Type) %>%
      summarise(
        init = mean(as.numeric(Initial_Speed), na.rm = TRUE),
        final = mean(as.numeric(Final_Speed), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = c(init, final), 
                   names_to = "Speed_Type", 
                   values_to = "Average_Speed") %>%
      mutate(Speed_Type = factor(Speed_Type, levels = c("init", "final")))
    
    ggplot(avg_df, aes(x = Vehicle_Type, y = Average_Speed, fill = Speed_Type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      labs(title = "Average Speeds by Vehicle Type (Speed Change = 1)",
           x = "Vehicle Type",
           y = "Average Speed",
           fill = "Speed Type") +
      theme_minimal()
  })
  
  output$avg_init_speed_table <- renderDT({
    avg_init <- filtered_data() %>%
      group_by(Vehicle_Type) %>%
      summarise(Average_Initial_Speed = round(mean(as.numeric(Initial_Speed), na.rm = TRUE), 2))
    datatable(avg_init, options = list(dom = 't'))
  })
  
  output$avg_final_speed_table <- renderDT({
    avg_final <- filtered_data() %>%
      group_by(Vehicle_Type) %>%
      summarise(Average_Final_Speed = round(mean(as.numeric(Final_Speed), na.rm = TRUE), 2))
    datatable(avg_final, options = list(dom = 't'))
  })
  
  output$flashing_effect_plot <- renderPlot({
    flashing_df <- df %>%
      filter(Flashing == 1) %>%
      pivot_longer(cols = c(Initial_Speed, Final_Speed),
                   names_to = "Speed_Type",
                   values_to = "Speed") %>%
      mutate(
        Speed = as.numeric(Speed),
        Speed_Type = recode(Speed_Type, Initial_Speed = "Initial", Final_Speed = "Final"),
        Speed_Type = factor(Speed_Type, levels = c("Initial", "Final"))
      )
    
    ggplot(flashing_df, aes(x = Speed_Type, y = Speed, fill = Speed_Type)) +
      geom_violin(trim = FALSE, alpha = 0.5) +
      geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +
      labs(title = "Speed Distribution Before and After Flashing Sign",
           x = "Speed Type",
           y = "Speed (mph)") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

