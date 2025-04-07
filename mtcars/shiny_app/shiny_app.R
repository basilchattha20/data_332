library(shiny)

ui <- fluidPage(
  titlePanel("Basic Histogram in Shiny"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotOutput("histPlot") # Placeholder for histogram
    )
  )
)

server <- function(input, output) {
  output$histPlot <- renderPlot({
    hist(mtcars$mpg, col = "blue", main = "Histogram of MPG", xlab =
           "MPG")
  })
}

shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Histogram with ggplot2"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotOutput("histPlot") # ggplot histogram
    )
  )
)

server <- function(input, output) {
  output$histPlot <- renderPlot({
    ggplot(mtcars, aes(x = mpg)) +
      geom_histogram(fill = "blue", color = "black", bins = 10) +
      labs(title = "Histogram of MPG", x = "MPG", y = "Count")
  })
}

shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Interactive Histogram with ggplot2"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Choose a Variable:",
                  choices = c("MPG" = "mpg", "Horsepower" = "hp",
                              "Weight" = "wt"),
                  selected = "mpg") # Default selection
    ),
    mainPanel(
      plotOutput("histPlot") # Interactive plot
    )
  )
)

server <- function(input, output) {
  output$histPlot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$var)) + # Uses user-selected
      variable
    geom_histogram(fill = "blue", color = "black", bins = 10) +
      labs(title = paste("Histogram of", input$var), x = input$var, y =
             "Count")
  })
}

shinyApp(ui = ui, server = server)
