library(shiny)
library(dataxray)
library(tidyverse)
library(glue)

# Define function to load data
load_tidied_data <- function(base_url, file_names) {
  dfs <- map(file_names, ~ read_csv(glue::glue("{base_url}{.x}"), show_col_types = FALSE))
  names(dfs) <- names(file_names)
  return(dfs)
}

# Define the named vector of file names
file_names <- c(
  "Water Samples" = "2024-03-15_water-samples-data-final.csv",
  "Profiles" = "2024-04-09_profiles-data-final.csv",
  "Clam Growth" = "2024-04-09_ksf-clams-growth-data-final.csv",
  "Oyster Growth" = "2024-04-09_ksf-oyster-cylinder-growth-data-final.csv",
  "Weather" = "2024-03-20_weather-data-final.csv",
  "Kauaʻi Sea Farm Data" = "2024-03-15_ksf-compiled-data-final.csv"
)

base_url <- "https://raw.githubusercontent.com/Lysbethk/nomilo-fishpond-analysis/main/data/output/"
dfs <- load_tidied_data(base_url, file_names)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        body, html {
          height: 100%;
          margin: 0;
        }
        .shiny-app {
          height: 100vh;
          width: 100vw;
        }
        .report-content {
          display: inline-block;
          white-space: normal;
          width: 100%;
        }
        "
      )
    )
  ),
  mainPanel(
    align = "center",
    fluidRow(
      column(width = 6, offset = 6, h4("Please select a dataset to describe.", style = "color: red; font-face: bold; text-align: center;"))
    ),
    fluidRow(
      column(width = 6, offset = 6, selectInput("dataset", "", choices = c("Select a Dataset", names(dfs))))
    ),
    fluidRow(
      column(width = 12, offset = 3, uiOutput("report"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$dataset, {
    if (input$dataset != "Select a Dataset") {
      output$report <- renderUI({
        dataset_name <- input$dataset
        dataset <- dfs[[dataset_name]]
        make_xray(dataset) %>% view_xray()
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
