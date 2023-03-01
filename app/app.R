#
# Shiny web application displaying information on the release year for movies
# on various streaming platforms. Includes a brief summary, an interactive plot,
# and an interactive table.
#

# Load libraries
library(shiny)
library(tidyverse)

# Import data
data <- read_delim("data/streaming-platform-data.csv")
View(data)
names(data)

# List of streaming services in the dataset
services = c("Netflix", "Hulu", "Prime Video", "Disney+")

# Define UI for application that shows tabs for a table, histogram, and explanation.
ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Visualizations",
      titlePanel("Movies by Year"),
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
                      sep = ""),
          textOutput("description")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Plot",
              plotOutput("plot")
            ),
            tabPanel(
              "Table",
              dataTableOutput("table")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    # Filter data by minimum year, maximum year, and streaming service.
    filtered <- data %>%
      filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(!!rlang::sym(input$service) == 1)
    
    ggplot(filtered, aes(fill = as.factor(Year))) +
      geom_bar(mapping = aes(x = filtered$Year), stat = "count") +
      labs(title = paste("Movies by Year on", input$service),
           x = "Year",
           y = "Movie count",
           fill = "Year")
  })
  
  output$description <- renderText({
    filtered <- data %>%
      filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(!!rlang::sym(input$service) == 1)
    
    paste("There are", nrow(filtered), 
          "movies that were released between", input$year[1], "and", input$year[2], 
          "on", input$service)
  })
  
  output$table <- renderDataTable(
    data %>% 
      filter(Year >= input$year[1] & Year <= input$year[2]) %>% 
      filter(!!rlang::sym(input$service) == 1) %>% 
      mutate(
        Netflix = NULL,
        Hulu = NULL,
        `Prime Video` = NULL,
        `Disney+` = NULL,
        Type = NULL,
        `...1` = NULL,
        ID = NULL
      )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
