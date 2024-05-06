library(shiny)
library(dataxray)
library(tidyverse)
library(glue)

source(here::here("code/functions/load_tidied_data.R"))

# Define the named vector of file names
file_names <- c(
  "Water Samples" = "2024-05-05_water-samples-data-final.csv",
  "Profiles" = "2024-05-05_water-profiles-data-final.csv",
  "Clam Growth" = "2024-05-05_clam-growth-data-final.csv",
  "Oyster Growth" = "2024-05-05_oyster-growth-data-final.csv",
  "Weather" = "2024-05-05_weather-data-final.csv",
  "Kauaʻi Sea Farm Data" = "2024-05-05_ksf-data-final.csv"
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
      column(width = 6, offset = 6, h4("Please select a dataset to view its data dictionary.", style = "color: red; font-face: bold; text-align: center;"))
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
