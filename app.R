# Load the required packages
#library(shiny)
library(ggplot2)
library(car)

data <- read.csv("./fertility.csv")

server <- function(input, output) {
  data_reactive <- reactive({
    if (length(input$x) > 0) {
      if (is.numeric(data[[input$y]]) == FALSE) {
        levels <- unique(data[[input$y]])
        data[[input$y]] <- ifelse(data[[input$y]] == levels[1], 0, 1)
        formula <- as.formula(paste(input$y, "~", paste(input$x, collapse = " + ")))
        fit <- glm(formula, data = data, family = binomial())
      } else {
        formula <- as.formula(paste(input$y, "~", paste(input$x, collapse = " + ")))
        fit <- lm(formula, data = data)
        if (input$outlier_cut) {
          cooksd <- cooks.distance(fit)
          data <- data[-which(cooksd > 4 / (nobs(fit) - length(fit$coefficients) - 1)), ]
        }
      }
      
      
      
      data
    } else {
      data
    }
  })
  
  fit <- reactive({
    if (!is.null(data_reactive()) && length(input$x) > 0) {
      formula <- as.formula(paste(input$y, "~", paste(input$x, collapse = " + ")))
      if (is.numeric(data_reactive()[[input$y]])) {
        fit = lm(formula, data = data_reactive())
      } else {
        fit = glm(formula, data = data_reactive(), family = binomial())
      }
    }
  })
  
  output$regPlots <- renderUI({

    if (length(input$x) > 0) {
      plot_output_list <- lapply(input$x, function(x) {
        plotOutput(paste("plot_", x, sep = ""))
      })
      
      do.call(tagList, plot_output_list)
    }
  })
  
  lapply(names(data), function(x) {
    output[[paste("plot_", x, sep = "")]] <- renderPlot({
      if (!is.null(fit())) {
        ggplot(data_reactive(), aes_string(x = x, y = residuals(fit()))) +
          geom_point() +
          geom_smooth(method = "lm") +
          labs(x = x, y = "Residuals", title = paste("Residuals vs", x))
      }
    }, height = 400, width = 400)
  })
  
  output$summary <- renderPrint({
    if (!is.null(fit())) {
      summary(fit())
    }
  })
  
  output$diagnostics <- renderPlot({
    if (!is.null(fit())) {
      par(mfrow = c(2, 2))
      plot(fit())
    }
  })
}

# Define the UI
ui <- fluidPage(
  titlePanel("Multiple Linear Regression App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x", "Select predictors:", choices = names(data), multiple = TRUE),
      selectInput("y", "Select a target:", choices = names(data)),
      checkboxInput("outlier_cut", "Remove outliers", FALSE)
    ),
    
    mainPanel(
      uiOutput("regPlots"),
      verbatimTextOutput("summary"),
      plotOutput("diagnostics")
    )
  )
)

shinyApp(ui = ui, server = server)
