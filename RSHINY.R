library(shiny)
library(shinydashboard)
library(tseries)
library(ggplot2)
library(forecast)
library(TTR)
library(lmtest)
library(readxl)
library(rugarch)
library(urca)
library(shinyWidgets)
library(gridExtra)

ui <- dashboardPage(
  dashboardHeader(title = "Time Series Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series Workflow", tabName = "ts_workflow", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "ts_workflow",
              fluidRow(
                box(fileInput("file", "Upload Excel File", accept = ".xlsx"), width = 6),
                box(numericInput("frequency", "Enter Data Frequency (e.g., 4 for Quarterly)", value = 4), width = 6),
                box(uiOutput("columnSelector"), width = 6),
                box(tableOutput("columnDisplay"), width = 12),
                box(plotOutput("tsPlot"), width = 12),
                box(selectInput("decompType", "Choose Decomposition Type", choices = c("Additive", "Multiplicative")), width = 6),
                box(plotOutput("baseDecompPlot"), width = 12),
                box(selectInput("transform", "Choose Transformation", 
                                choices = c("None", "Log", "Differencing", "Seasonal Differencing", "Moving Average")), width = 6),
                box(numericInput("maWindow", "Moving Average Window:", value = 3), width = 6),
                box(actionButton("applyTransform", "Apply Transformation"), width = 6),
                box(plotOutput("acfPacfPlot"), width = 12),
                box(selectInput("modelType", "Select Model Type:",
                                choices = c("Auto ARIMA", "AR", "MA", "ARMA", "ARIMA", "SARIMA", "ARCH", "GARCH")), width = 4),
                box(numericInput("p_order", "AR (p):", value = 1), width = 2),
                box(numericInput("d_order", "I (d):", value = 0), width = 2),
                box(numericInput("q_order", "MA (q):", value = 1), width = 2),
                box(actionButton("fitModel", "Fit Model"), width = 2),
                box(verbatimTextOutput("modelSummary"), width = 12),
                box(plotOutput("modelDiagnosticPlot"), width = 12),
                box(plotOutput("residualsPlot"), width = 12),
                box(plotOutput("acfPlot"), width = 12),
                box(plotOutput("qqPlot"), width = 12),
                box(verbatimTextOutput("shapiroTest"), width = 6),
                box(verbatimTextOutput("ljungBoxTest"), width = 6),
                box(verbatimTextOutput("whiteTest"), width = 12),
                box(selectInput("forecastMethod", "Forecast Method:",
                                choices = c("Exponential Smoothing", "Holt-Winters", "ARIMA")), width = 4),
                box(numericInput("horizon", "Forecast Horizon", value = 10), width = 2),
                box(actionButton("runForecast", "Forecast"), width = 2),
                box(plotOutput("forecastPlot"), width = 6),
                box(tableOutput("forecastTable"), width = 6),
                box(verbatimTextOutput("forecastMAE"), width = 6),
                box(verbatimTextOutput("forecastAICBIC"), width = 6),
                box(verbatimTextOutput("finalReport"), width = 12),
                box(downloadButton("downloadReport", "Download Report"), width = 4)
              )
      )
    )
  )
)

server <- function(input, output, session) {
  data_imported <- reactiveVal()
  time_series_data <- reactiveVal()
  modelFit <- reactiveVal()
  forecastData <- reactiveVal()
  
  observeEvent(input$file, {
    req(input$file)
    df <- read_excel(input$file$datapath)
    data_imported(df)
    updateSelectInput(session, "selectedCol", choices = names(df))
  })
  
  output$columnSelector <- renderUI({
    req(data_imported())
    selectInput("selectedCol", "Choose Column to Analyze:", choices = names(data_imported()))
  })
  
  observe({
    req(data_imported(), input$selectedCol, input$frequency)
    
    col_data <- data_imported()[[input$selectedCol]]
    
    if (!is.numeric(col_data)) {
      showNotification("Selected column is not numeric. Please choose a numeric column.", type = "error")
      return()
    }
    
    if (all(is.na(col_data))) {
      showNotification("Selected column contains only missing values.", type = "error")
      return()
    }
    
    ts_obj <- tryCatch({
      ts(na.omit(col_data), frequency = input$frequency)
    }, error = function(e) {
      showNotification("Failed to convert to time series. Check the data and frequency.", type = "error")
      NULL
    })
    
    if (!is.null(ts_obj)) {
      time_series_data(ts_obj)
    }
  })
  
  output$columnDisplay <- renderTable({
    req(data_imported())
    head(data_imported(), 10)
  })
  
  output$tsPlot <- renderPlot({
    req(time_series_data())
    plot(time_series_data(), main = "Time Series Plot", ylab = input$selectedCol)
  })
  
  output$baseDecompPlot <- renderPlot({
    req(time_series_data(), input$decompType)
    tsData <- na.omit(time_series_data())
    decomp <- decompose(tsData, type = tolower(input$decompType))
    plot(decomp)
  })
  
  transformedData <- eventReactive(input$applyTransform, {
    tsData <- time_series_data()
    if (is.null(tsData)) return(NULL)
    if (input$transform == "Log") {
      tsData <- log(tsData)
    } else if (input$transform == "Differencing") {
      tsData <- diff(tsData)
    } else if (input$transform == "Seasonal Differencing") {
      tsData <- diff(tsData, lag = input$frequency)
    } else if (input$transform == "Moving Average") {
      tsData <- ts(SMA(tsData, n = input$maWindow), frequency = input$frequency)
    }
    tsData
  }, ignoreNULL = FALSE)
  
  output$acfPacfPlot <- renderPlot({
    tsData <- na.omit(transformedData())
    par(mfrow = c(1,2))
    acf(tsData, main = "ACF")
    pacf(tsData, main = "PACF")
  })
  
  observeEvent(input$fitModel, {
    tsData <- na.omit(transformedData())
    fit <- tryCatch({
      if (input$modelType == "Auto ARIMA") {
        auto.arima(tsData)
      } else if (input$modelType == "ARIMA") {
        Arima(tsData, order = c(input$p_order, input$d_order, input$q_order))
      } else if (input$modelType == "GARCH") {
        spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(input$p_order, input$q_order)))
        ugarchfit(spec, tsData)
      } else {
        auto.arima(tsData) # fallback
      }
    }, error = function(e) NULL)
    modelFit(fit)
  })
  
  output$modelSummary <- renderPrint({
    req(modelFit())
    summary(modelFit())
  })
  
  output$modelDiagnosticPlot <- renderPlot({
    fit <- modelFit()
    req(fit)
    if (inherits(fit, "Arima")) {
      tsdiag(fit)
    }
  })
  
  output$residualsPlot <- renderPlot({
    fit <- modelFit()
    req(fit)
    res <- residuals(fit)
    ts.plot(res, main = "Residuals")
  })
  
  output$acfPlot <- renderPlot({
    req(modelFit())
    acf(residuals(modelFit()), main = "ACF of Residuals")
  })
  
  output$qqPlot <- renderPlot({
    qqnorm(residuals(modelFit()))
    qqline(residuals(modelFit()), col = "red")
  })
  
  output$shapiroTest <- renderPrint({
    shapiro.test(residuals(modelFit()))
  })
  
  output$ljungBoxTest <- renderPrint({
    Box.test(residuals(modelFit()), lag = 10, type = "Ljung")
  })
  
  output$whiteTest <- renderPrint({
    bptest(residuals(modelFit()) ~ fitted(modelFit()))
  })
  
  observeEvent(input$runForecast, {
    tsData <- na.omit(transformedData())
    fit <- modelFit()
    fc <- tryCatch({
      if (input$forecastMethod == "Holt-Winters") {
        forecast(HoltWinters(tsData), h = input$horizon)
      } else if (input$forecastMethod == "ARIMA" && inherits(fit, "Arima")) {
        forecast(fit, h = input$horizon)
      } else {
        forecast(ets(tsData), h = input$horizon)
      }
    }, error = function(e) NULL)
    forecastData(fc)
  })
  
  output$forecastPlot <- renderPlot({
    req(forecastData())
    autoplot(forecastData())
  })
  
  output$forecastTable <- renderTable({
    req(forecastData())
    as.data.frame(forecastData())
  })
  
  output$forecastMAE <- renderPrint({
    fc <- forecastData()
    req(fc)
    if (is.null(fc$residuals)) {
      cat("MAE: N/A (residuals not available)")
    } else {
      mae <- mean(abs(fc$residuals), na.rm = TRUE)
      cat("MAE:", round(mae, 3))
    }
  })
  
  output$forecastAICBIC <- renderPrint({
    fit <- modelFit()
    req(fit)
    cat("AIC:", AIC(fit), "\nBIC:", BIC(fit))
  })
  
  output$finalReport <- renderPrint({
    cat("\u2713 Dataset Summary:\n")
    print(summary(data_imported()))
    cat("\n\u2713 Transformations Applied:", input$transform)
    cat("\n\u2713 Model Chosen:", input$modelType)
    cat("\n\u2713 Orders - p:", input$p_order, ", d:", input$d_order, ", q:", input$q_order)
    fit <- modelFit()
    if (!is.null(fit)) {
      if (!inherits(fit, "uGARCHfit")) {
        cat("\n\u2713 Model AIC:", AIC(fit), "\n\u2713 BIC:", BIC(fit))
      }
    }
    fc <- forecastData()
    if (!is.null(fc)) {
      cat("\n\u2713 Forecast Completed\n")
      print(fc)
    }
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("TimeSeries_Report", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      capture.output({
        cat("Time Series Report\n====================\n")
        print(summary(data_imported()))
        cat("\nModel Summary:\n")
        print(summary(modelFit()))
        cat("\nForecast Summary:\n")
        print(forecastData())
      }, file = file)
    }
  )
}

shinyApp(ui, server)