#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# NOTE: set your working directory to this folder when you're working on it, don't
# move the data file.
data <- read_delim("data/streaming-platform-data.csv")
services = c("Netflix", "Hulu", "Prime Video", "Disney+")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Movies by Year for Different Streaming Services"), 
  sidebarLayout(
    sidebarPanel(
      selectInput("service",
                  "Select streaming service:",
                  choices = services),
      sliderInput("year", 
                  "Select year range:", 
                  min = min(data$Year), 
                  max = max(data$Year), 
                  c(1940, 2002),
                  sep = "")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Movies by Year", plotOutput("plot"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get information from input
  service <- reactive({input$service})
  min_year <- reactive({input$year[1]})
  max_year <- reactive({input$year[2]})
  
  filtered <- data %>% 
    filter(Year >= min_year & Year <= max_year) %>% 
    filter(!!as.symbol(service) == 1)
  
  output$plot <- renderPlot({
    ggplot(filtered) +
      geom_bar(mapping = aes(x = filtered$Year), stat = "count") +
      labs(title = paste("Movies by Year on", service),
           x = "Year",
           y = "Movie count",
           fill = "Year")
  })
  
  output$description <- renderText({
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
