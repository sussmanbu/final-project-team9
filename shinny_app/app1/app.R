library(shiny)
library(dplyr)
library(readr)
library(knitr)
library(broom)  
library(kableExtra)
library(here)
data_2022 <- readRDS("latest.rds") 


ui <- fluidPage(
  titlePanel("Model Summary Table_MLR Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:", choices = NULL),
      hr()
    ),
    mainPanel(
      uiOutput("modelTable")
    )
  )
)

server <- function(input, output, session) {  
  
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

shinyApp(ui = ui, server = server)

