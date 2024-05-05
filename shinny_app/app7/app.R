library(shiny)
library(DT)
library(purrr)
library(tidyr)
library(broom)
library(dplyr)
library(readr)
library(knitr)
library(lme4)
library(broom.mixed)
library(kableExtra)
data_twenty <- readRDS("data_twenty_clean.rds") 
 

model_with_income_log <- lmer(log_RENTGRS ~ log_INCTOT + ROOMS + NFAMS + MARST + RACE + EMPSTAT + (1 | REGION_CLASSIFIED), data = data_twenty, REML = FALSE)
tidy_lm <- tidy(model_with_income_log)
# Define UI for application
ui <- fluidPage(
  titlePanel("Interactive Coefficient Viewer"),
  sidebarLayout(
    sidebarPanel(
      # Setting default value for the coefficient selection
      selectInput("coeff", "Select a Coefficient:", choices = tidy_lm$term, selected = "log_INCTOT")
    ),
    mainPanel(
      DTOutput("selected_row"),
      textOutput("detail_text")
    )
  )
)

server <- function(input, output) {
  # Server logic to filter the selected coefficient
  output$selected_row <- renderDT({
    req(input$coeff)  # ensure that input$coeff is selected
    selected_data <- tidy_lm %>%
      filter(term == input$coeff) %>%
      select(term, estimate, std.error, statistic)  # selecting only the necessary columns
    
    datatable(selected_data, options = list(pageLength = 5, autoWidth = TRUE))
  }, options = list(pageLength = 5))
  
  # Display additional details or messages
  output$detail_text <- renderText({
    if (!is.null(input$coeff)) {
      paste("Showing details for the coefficient:", input$coeff)
    }
  })
}

shinyApp(ui = ui, server = server)
