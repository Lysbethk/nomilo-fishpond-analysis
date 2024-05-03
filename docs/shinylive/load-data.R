library(shiny)
library(readr)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)

# Preload data
dfs <- list(
  "Water Samples" = readr::read_csv("https://raw.githubusercontent.com/Lysbethk/nomilo-fishpond-analysis/main/data/output/2024-03-15_water-samples-data-tidied.csv"),
  "Profiles" = readr::read_csv("https://raw.githubusercontent.com/Lysbethk/nomilo-fishpond-analysis/main/data/output/2024-03-15_profiles-data-tidied.csv")
)

# Helper function to convert variable names to title case with spaces
make_title_case <- function(variable_name) {
  stringr::str_to_title(stringr::str_replace_all(variable_name, pattern = "_", replacement = " "))
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@500&display=swap');
      body { color: #505050; font-family: 'Roboto', sans-serif; padding-left: 20px; padding-right: 20px;}
      h2 { font-family: 'Roboto', sans-serif; color: #black;}
      .main-title { text-align: center; font-size: 30px; font-weight: bold; margin-bottom: 20px; }
    "))
  ),
  titlePanel(div("Exploring Depth-wise Biogeochemical Variations Across Nomilo Fishpond Locations Over Time", class = "main-title")),
  fluidRow(
    column(width = 3, offset = 3, selectInput("dataset", "Choose Dataset:", choices = c("Select a Dataset" = "", names(dfs)))),
    column(width = 3, uiOutput("y_var_ui"))
  ),
  fluidRow(
    column(width = 3, offset = 3, numericInput("y_min", "Y-axis Minimum:", value = 0)),
    column(width = 3, numericInput("y_max", "Y-axis Maximum:", value = 100))
  ),
  fluidRow(
    column(width = 3, offset = 5, actionButton("generate", "Generate"))
  ),
  uiOutput("plot_ui")
)

server <- function(input, output, session) {
  output$y_var_ui <- renderUI({
    if(input$dataset != ""){
      df <- dfs[[input$dataset]]
      num_vars <- names(which(sapply(df, is.numeric)))
      selectInput("y_var", "Choose Y Variable:", choices = c("Select a Y Variable" = "", num_vars))
    } else {
      selectInput("y_var", "Choose Y Variable:", choices = c("Select a Y Variable" = ""))
    }
  })
  
  output$plot_ui <- renderUI({
    if(input$dataset == "" || input$y_var == "" || input$y_var == "Select a Y Variable"){
      return(h4("Please select a dataset and y variable to view the dataset and generate plots.", style = "color: red; text-align: center;"))
    } else {
      tabsetPanel(
        tabPanel("Data Table", DTOutput("table")),
        tabPanel("Static Plot", plotOutput("staticplot")),
        tabPanel("Interactive Plot", plotlyOutput("interactiveplot")),
        selected = "Data Table"
      )
    }
  })
  
  observeEvent(input$generate, {
    # Require that both a dataset and a Y variable have been selected
    req(input$dataset, input$y_var != "")
    
    withProgress(message = 'Generating plots...', value = 0, {
      for(i in 1:10) {
        incProgress(0.1)
        Sys.sleep(0.1) # Simulate some processing time
      }
      
      # Generate data table
      output$table <- renderDT({
        datatable(dfs[[input$dataset]], options = list(pageLength = 10, autoWidth = TRUE))
      })
      
      # Generate static plot
      output$staticplot <- renderPlot({
        df <- dfs[[input$dataset]]
        df$depth <- as.factor(df$depth)
        p <- ggplot(df, aes(x = date, y = !!rlang::sym(input$y_var), color = depth)) +
          geom_point(size = 2.75, alpha = 0.8) +
          geom_line(linewidth = 2, alpha = 0.6) +
          facet_wrap(~ location) +
          labs(color = "Depth", x = "Month", y = make_title_case(input$y_var)) +
          scale_color_viridis_d() +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 14),
            axis.text.y = element_text(margin = margin(r = 10), size = 14),
            axis.title.x = element_text(margin = margin(t = 20), face = "bold", size = 18),
            axis.title.y = element_text(face = "bold", margin = margin(r = 5), size = 18),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
            legend.position = "bottom",
            legend.title = element_text(face = "bold", size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 16),
            plot.margin = margin(20, 20, 20, 20),
            panel.spacing = unit(1.5, "lines")
          ) +
          guides(color = guide_legend(override.aes = list(size=4))) +
          ylim(input$y_min, input$y_max)
        p
      })
      
      # Generate interactive plot
      output$interactiveplot <- renderPlotly({
        df <- dfs[[input$dataset]]
        df$depth <- as.factor(df$depth)
        p <- ggplot(df, aes(x = date, y = !!rlang::sym(input$y_var), color = depth)) +
          geom_point(size = 1.5, alpha = 0.8) +
          geom_line(linewidth = 1, alpha = 0.6) +
          facet_wrap(~ location) +
          labs(color = "Depth", x = "Month", y = make_title_case(input$y_var)) +
          scale_color_viridis_d() +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            axis.title.x = element_text(margin = margin(t = 10), face = "bold", size = 12),
            axis.title.y = element_text(face = "bold", margin = margin(l = 15), size = 12),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom",
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 10),
            strip.text = element_text(size = 12),
            plot.margin = margin(20, 20, 20, 20)
          ) +
          ylim(input$y_min, input$y_max)
        ggplotly(p, tooltip = c("text", "y")) %>% 
          layout(legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.5))
      })
    })
  })
}

shinyApp(ui, server)