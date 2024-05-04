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


data_path <- "data_twenty_clean.csv"
data_twenty  <- read_csv(data_path, show_col_types = FALSE) 


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
  do(model = lm(log_RENTGRS ~ log_INCTOT + KITCHEN + ROOMS + NFAMS + AGE + MARST + EMPSTAT + REGION_CLASSIFIED, data = .)
  )

models_with_coef <- models_by_race_year %>%
  rowwise() %>% 
  mutate(log_INCTOT_coef = coef(model)[["log_INCTOT"]]) %>%
  ungroup() %>%
  select(RACE, YEAR, log_INCTOT_coef)

coefficients_wide <- models_with_coef %>%
  pivot_wider(names_from = RACE, values_from = log_INCTOT_coef, names_prefix = "coef_")

log_inctot_p_values <- data_twenty %>%
  group_by(RACE, YEAR) %>%
  filter(if_all(c(KITCHEN, ROOMS, NFAMS, AGE, MARST, EMPSTAT, REGION_CLASSIFIED), ~n_distinct(.) > 1)) %>% # Ensure factors have more than one level
  do({safely_result = tryCatch({
    tidy(lm(log_RENTGRS ~ log_INCTOT + KITCHEN + ROOMS + NFAMS + AGE + MARST + EMPSTAT + REGION_CLASSIFIED, data = .))
  }, error = function(e) return(NULL))  # Handle any errors in lm
  
  if (!is.null(safely_result)) {
    safely_result
  } else {
    data.frame(YEAR = first(.$YEAR), RACE = first(.$RACE), term = NA, estimate = NA, p.value = NA)  # Placeholder for error groups
  }
  }) %>%
  ungroup() %>%
  filter(term == "log_INCTOT" & !is.na(estimate)) %>%
  select(RACE, YEAR, term, estimate, p.value)

unique_races <- unique(log_inctot_p_values$RACE)

# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)

# Assuming data_twenty_filtered_final is your final, cleaned dataset
# Running model fitting
models_by_race_year <- data_twenty_filtered_final %>%
  group_by(RACE, YEAR) %>%
  do(model = lm(log_RENTGRS ~ log_INCTOT + KITCHEN + ROOMS + NFAMS + AGE + MARST + EMPSTAT + REGION_CLASSIFIED, data = .))

# Extracting coefficients
models_with_coef <- models_by_race_year %>%
  rowwise() %>% 
  mutate(log_INCTOT_coef = coef(model)[["log_INCTOT"]]) %>%
  ungroup() %>%
  select(RACE, YEAR, log_INCTOT_coef)

# Wide to long transformation for plotting
coefficients_long <- models_with_coef %>%
  pivot_longer(cols = log_INCTOT_coef, names_to = "variable", values_to = "value")

# Define UI
ui <- fluidPage(
  titlePanel("Income Coefficients Analysis by Race"),
  sidebarLayout(
    sidebarPanel(
      selectInput("race_selector", "Select Race:", choices = unique(models_with_coef$RACE))
    ),
    mainPanel(
      plotOutput("race_plot")
    )
  )
)

server <- function(input, output) {
  output$race_plot <- renderPlot({
    race_data <- coefficients_long %>%
      filter(RACE == input$race_selector)
    
    ggplot(race_data, aes(x = as.factor(YEAR), y = value)) +
      geom_col(fill = "lightblue", width = 0.7) +  # Using geom_col for bar plot
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Adjust text for readability
      labs(title = paste("Income Coefficient:", input$race_selector),
           x = "Year",
           y = "Coefficient")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


