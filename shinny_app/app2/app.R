library(shiny)
library(dplyr)
library(readr)
library(knitr)
library(broom)  
library(DT)
library(kableExtra)
library(here)  # For resolving paths


data_path <- "latest_clean.csv"  
data_2022 <- read_csv(data_path, show_col_types = FALSE)
log_model_2022 <- lm(log_RENTGRS ~ log_INCTOT + ROOMS + NFAMS + AGE + MARST + RACE + EMPSTAT + REGION_CLASSIFIED, data = data_2022)
ui <- fluidPage(
  titlePanel(tags$h1("Model Summary Table_log_log Model", style = "font-size: 18px;")),  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:", choices = NULL),
      hr(),
      p("The model suggests that a 10% increase in income is predicted to raise rent by an average of 13.34%, with all other variables held constant, indicating income's elasticity effect on rent.")
    ),
    mainPanel(
      DTOutput("modelTable")
    )
  )
)

server <- function(input, output, session) {
  data_2022$log_INCTOT <- log(data_2022$INCTOT + 1)  
  data_2022$log_RENTGRS <- log(data_2022$RENTGRS + 1)  
  log_model_2022 <- lm(log_RENTGRS ~ log_INCTOT + ROOMS + NFAMS + AGE + MARST + RACE + EMPSTAT + REGION_CLASSIFIED, data = data_2022)
  tidy_log_model_2022 <- tidy(log_model_2022)
  observe({
    vars <- tidy_log_model_2022$term
    default_selection <- "log_INCTOT"
    if (default_selection %in% vars) {
      updateSelectInput(session, "variable", choices = vars, selected = default_selection)
    } else {
      updateSelectInput(session, "variable", choices = vars, selected = vars[1])
    }
  })
  output$modelTable <- renderDT({
    req(input$variable)  # Ensure that input$variable is reactive and triggers the rendering
    selected_row <- tidy_log_model_2022 %>% filter(term == input$variable)
    datatable(selected_row, options = list(pageLength = 5, autoWidth = TRUE))
  })
}



shinyApp(ui = ui, server = server)


