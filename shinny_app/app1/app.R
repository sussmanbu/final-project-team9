library(shiny)
library(dplyr)
library(readr)
library(knitr)
library(broom)  # For tidy() function
library(kableExtra)
library(here)
data_path <- "latest_clean.csv"
data_2022 <- read_csv(data_path, show_col_types = FALSE)


# Define the UI
ui <- fluidPage(
  titlePanel("Model Summary Table_MLR Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:", choices = NULL),
      hr(),
      p("A $1,000 increase in total income (INCTOT) is associated with an average increase of 5.2 in gross rent (RENTGRS), assuming other variables are constant.")
    ),
    mainPanel(
      uiOutput("modelTable")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {  # Include session here
  # Example to load data
  data_path <- "latest_clean.csv"
  data_2022 <- read_csv(data_path, show_col_types = FALSE)
  
  mlr_model <- lm(RENTGRS ~ INCTOT + ROOMS + NFAMS + AGE + MARST + RACE + EMPSTAT + REGION_CLASSIFIED, data = data_2022)
  tidy_mlr <- broom::tidy(mlr_model)
  
  observe({
    # Correct use of session in updateSelectInput
    updateSelectInput(session, "variable", choices = names(coef(mlr_model)), selected = "INCTOT")
  })
  
  output$modelTable <- renderUI({
    selected_var <- input$variable
    if (is.null(selected_var)) {
      selected_var <- "INCTOT"
    }
    selected_row <- tidy_mlr %>% filter(term == selected_var)
    tab <- kable(selected_row, "html") %>%
      kable_styling("striped", full_width = F)
    HTML(tab)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

