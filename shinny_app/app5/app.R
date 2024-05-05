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



data_twenty  <- readRDS("data_twenty_clean.rds") 

categorical_vars <- c("KITCHEN", "MARST", "EMPSTAT", "REGION_CLASSIFIED")
data_twenty[categorical_vars] <- lapply(data_twenty[categorical_vars], factor)

categorical_vars <- c("KITCHEN", "MARST", "EMPSTAT", "REGION_CLASSIFIED")
data_twenty[categorical_vars] <- lapply(data_twenty[categorical_vars], factor)


race_levels <- data_twenty %>%
  group_by(RACE) %>%
  summarize(across(all_of(categorical_vars), ~n_distinct(.))) %>%
  ungroup()

races_with_single_levels <- race_levels %>%
  filter(if_any(everything(), ~.x == 1)) %>%
  pull(RACE)

data_twenty_filtered <- data_twenty %>%
  filter(!RACE %in% races_with_single_levels)
data_twenty_filtered[categorical_vars] <- lapply(data_twenty_filtered[categorical_vars], factor)

race_year_levels <- data_twenty_filtered %>%
  group_by(RACE, YEAR) %>%
  summarize(across(all_of(categorical_vars), ~n_distinct(.))) %>%
  ungroup()

race_year_with_single_levels <- race_year_levels %>%
  filter(if_any(everything(), ~.x == 1))

data_twenty_filtered_final <- data_twenty_filtered %>%
  anti_join(race_year_with_single_levels, by = c("RACE", "YEAR"))

models_by_race_year <- data_twenty_filtered_final %>%
  group_by(RACE, YEAR) %>%
  do({
    model = lm(log_RENTGRS ~ log_INCTOT + KITCHEN + ROOMS + NFAMS + AGE + MARST + EMPSTAT + REGION_CLASSIFIED, data = .)
    glance(model)
  }) %>%
  ungroup() %>%
  select(RACE, YEAR, everything())

unique_races <- unique(models_by_race_year$RACE)

ui <- fluidPage(
  titlePanel("INCTOT Model Summaries by Race"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_race", "Choose a race:", choices = unique_races)
    ),
    mainPanel(
      dataTableOutput("model_summary")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    models_by_race_year %>%
      filter(RACE == input$selected_race)
  })
  
  output$model_summary <- renderDataTable({
    datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
}

shinyApp(ui = ui, server = server)


