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
library(here)

data_path <- "data_twenty_clean.csv"
data_twenty <- read_csv(data_path, show_col_types = FALSE) 

models_by_year <- data_twenty %>%
  group_by(YEAR) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(log_RENTGRS ~ log_INCTOT + KITCHEN + ROOMS + NFAMS + AGE + MARST + RACE + EMPSTAT + REGION_CLASSIFIED, data = .)),
    tidied = map(model, tidy),
    glance_data = map(model, glance),
    augmented = map(model, augment)
  ) %>%
  ungroup()

ui <- fluidPage(
  titlePanel("Model Summary Statistics by Year"),
  sidebarLayout(
    sidebarPanel(
      # Ensure years are sorted in descending order
      selectInput("year", "Choose a year:", choices = sort(unique(models_by_year$YEAR), decreasing = TRUE))
    ),
    mainPanel(
      DTOutput("model_summary_table")  # Use DTOutput for data tables
    )
  )
)

server <- function(input, output) {
  output$model_summary_table <- renderDT({
    req(models_by_year) 
    year_data <- models_by_year %>%
      filter(YEAR == input$year)
    year_data <- year_data %>%
      select(YEAR, glance_data) %>%
      unnest(cols = glance_data) %>%
      arrange(YEAR) %>%
      select(YEAR, r.squared, adj.r.squared, sigma, statistic, p.value, df, logLik, AIC, BIC, deviance, df.residual, nobs) 
    datatable(year_data, 
              options = list(
                pageLength = 10,      
                lengthMenu = c(5, 10, 25, 50, 100),
                autoWidth = TRUE
              ),
              rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)


