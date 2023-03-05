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
n_obs <- nrow(data)
n_var <- ncol(data)
random <- data[sample(n_obs, 5), ]

# List of streaming services in the dataset
services <-  c("Netflix", "Hulu", "Prime Video", "Disney+")
ratings <-  unique(data$Age)
ratings <- ratings[!is.na(ratings)]

# Define UI for application that shows tabs for a table, histogram, and explanation.
ui <- fluidPage(
  titlePanel("Movies on Netflix, Prime Video, Hulu, and Disney+"),
  tabsetPanel(
    tabPanel(
      "About",
      p("The dataset", em("Movies on Netflix, Prime Video, Hulu and Disney+"), 
        "from", a("Kaggle", 
                  href="https://www.kaggle.com/datasets/ruchi798/movies-on-netflix-prime-video-hulu-and-disney"),
      "includes information on the movies found on the streaming services listed."),
      p("There are", strong(n_obs), "observations and", strong(n_var), "variables 
        in the data."),
      p("Below is a small, random sample of the data:"),
      tableOutput("random_table")
    ),
    tabPanel(
      "Plot",
      sidebarLayout(
        sidebarPanel(
          p("Select the desired streaming service and year range to view the 
            number of movies released in each year."),
          selectInput("plot_service",
                      "Select streaming service:",
                      choices = services),
          sliderInput("plot_year", 
                      "Select year range:", 
                      min = min(data$Year), 
                      max = max(data$Year), 
                      c(1940, 2002),
                      sep = ""),
          radioButtons(
            "fill",
            "Pick a color!",
            choices = c("orange", "blue", "purple")
          ),
          textOutput("plot_description")
        ),
        mainPanel(
          plotOutput("plot")
        )
      )
    ),
    tabPanel(
     "Table",
     sidebarLayout(
       sidebarPanel(
         p("Select a streaming service and age ratings to view all movies with
           the chosen ratings. If nothing is selected, movies with no rating will
           be shown."),
         selectInput("table_service",
                     "Select streaming service:",
                     choices = services),
         checkboxGroupInput("table_age",
                            "View movies with age rating...",
                            choices = ratings),
         textOutput("table_description")
       ),
       mainPanel(
         dataTableOutput("table")
       )
     )
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    
    # Filter data by minimum year, maximum year, and streaming service.
    filtered <- data %>%
      filter(Year >= input$plot_year[1] & Year <= input$plot_year[2]) %>%
      filter(!!rlang::sym(input$plot_service) == 1)
    
    ggplot(filtered) +
      geom_bar(mapping = aes(x = filtered$Year), stat = "count", fill = input$fill) +
      labs(title = paste("Movies by Year on", input$plot_year),
           x = "Year",
           y = "Movie count")
  })
  
  output$plot_description <- renderText({
    filtered <- data %>%
      filter(Year >= input$plot_year[1] & Year <= input$plot_year[2]) %>%
      filter(!!rlang::sym(input$plot_service) == 1)
    
    paste("There are", nrow(filtered), 
          "movies that were released between", input$plot_year[1], "and", 
          input$plot_year[2], "on", paste0(input$plot_service, "."))
  })
  
  output$table <- renderDataTable({
    data %>% 
      filter(!!rlang::sym(input$table_service) == 1) %>%
      filter(
        if (!is.null(input$table_age)) {
          Age %in% input$table_age
        } else {
          is.na(Age)
        }
      ) %>% 
      arrange(Year) %>% 
      select(Title, Year, Age, `Rotten Tomatoes`)
  })
  
  output$table_description <- renderText({
    all_service <- data %>% 
      filter(!!rlang::sym(input$table_service) == 1)
      
    input$table_age
    filtered <- all_service %>% 
      filter(
        if (!is.null(input$table_age)) {
          Age %in% input$table_age
        } else {
          is.na(Age)
        }
      )
    
    num_chosen <- nrow(filtered)
    num_movies <- nrow(all_service)
    choices <- input$table_age
    choice_string <- "(none)"
    
    for (choice in choices) {
      if (choice_string == "(none)") {
        choice_string <- choice  
      } else if (match(choice, choices) == length(choices)) {
        choice_string <- str_c(choice_string, ", or ", choice)
      } else {
        choice_string <- str_c(choice_string, ", ", choice)
      }
    }
    
    paste(num_chosen, "out of", num_movies, "movies on",
          input$service, "have the rating",
          paste0(choice_string, "."))
  })
  
  output$random_table <- renderTable({
    random
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
