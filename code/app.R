source(here::here("data/tidied/2024-03-15_profiles-data-tidied.csv"))
source(here::here("data/tidied/2024-03-15_water-samples-data-tidied.csv"))

# Helper function to convert variable names to title case with spaces
make_title_case <- function(variable_name) {
  str_to_title(str_replace_all(variable_name, pattern = "_", replacement = " "))
}

# Define the function for generating the plot
generate_profile_changes_lineplot <- function(df, y_var, y_var_title, y_min, y_max) {
  ggplot(df, aes(x = date, y = !!rlang::sym(y_var), color = as.factor(depth))) +
    geom_point(size = 1.5, alpha = 0.8) +
    geom_line(linewidth = 1, alpha = 0.6) +
    facet_wrap(~ location, scales = "free_y") +
    theme_minimal() +
    labs(color = "Depth", x = "Month", y = y_var_title,
         title = paste(y_var_title, "Over Time by Location and Depth")) +
    scale_color_viridis_d() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
          axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
          axis.title.y = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          legend.title = element_text(face = "bold")) +
    ylim(as.numeric(y_min), as.numeric(y_max))
}

# Shiny UI definition
ui <- fluidPage(
  titlePanel("Nomilo Fishpond Biogeochemical Changes Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose Dataset:",
                  choices = c("Water Samples" = "water_samples_data_tidied",
                              "Profiles" = "profiles_data_tidied")),
      uiOutput("y_var_ui"),
      numericInput("y_min", "Y-axis Minimum:", value = 0),
      numericInput("y_max", "Y-axis Maximum:", value = 100)
    ),
    mainPanel(
      plotOutput("lineplot")
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output, session) {
  output$y_var_ui <- renderUI({
    df <- get(input$dataset, envir = .GlobalEnv)
    num_vars <- df %>%
      select_if(is.numeric) %>%
      names()
    
    selectInput("y_var", "Choose Y Variable:", choices = num_vars)
  })
  
  output$lineplot <- renderPlot({
    req(input$dataset)
    df <- get(input$dataset, envir = .GlobalEnv)
    
    formatted_title <- make_title_case(input$y_var)
    
    generate_profile_changes_lineplot(df, input$y_var, formatted_title, input$y_min, input$y_max)
  })
}

shinyApp(ui, server)