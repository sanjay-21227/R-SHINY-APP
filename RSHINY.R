library(shinydashboard)
library(tseries)
library(ggplot2)
library(forecast)
library(TTR)
library(lmtest)
library(readxl)
library(rugarch)
library(urca)

ui <- dashboardPage(
  dashboardHeader(title = "Time Series Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Column Info", tabName = "columns", icon = icon("table")),
      menuItem("Data Visualization", tabName = "viz", icon = icon("chart-line")),
      menuItem("Decomposition", tabName = "decomposition", icon = icon("project-diagram")),
      menuItem("Stationarity Test", tabName = "stationarity", icon = icon("check-circle")),
      menuItem("Modeling", tabName = "model", icon = icon("cogs")),
      menuItem("Residual Analysis", tabName = "residuals", icon = icon("chart-bar")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-area")),
      menuItem("Final Report", tabName = "report", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(fileInput("file", "Upload Excel File", accept = ".xlsx"), width = 6),
                box(numericInput("frequency", "Enter Data Frequency (e.g., 4 for Quarterly)", value = 4), width = 6)
              )
      ),
      tabItem(tabName = "columns",
              fluidRow(
                box(tableOutput("columnDisplay"), width = 12)
              )
      ),
      tabItem(tabName = "decomposition",
              fluidRow(
                box(selectInput("decompType", "Choose Decomposition Type", choices = c("Additive", "Multiplicative")), width = 6),
                box(plotOutput("baseDecompPlot"), width = 12)
              )
      ),
      tabItem(tabName = "viz",
              fluidRow(
                box(plotOutput("tsPlot"), width = 12)
              ),
              fluidRow(
                box(selectInput("transform", "Choose Transformation", 
                                choices = c("None", "Log", "Differencing", "Seasonal Differencing", "Moving Average")), width = 6),
                
                conditionalPanel(
                  condition = "input.transform == 'Moving Average'",
                  box(numericInput("maWindow", "Moving Average Window:", value = 3), width = 6)
                ),
                
                box(actionButton("applyTransform", "Apply Transformation"), width = 6)
              )
      )
      ,
      tabItem(tabName = "stationarity",
              fluidRow(
                box(plotOutput("acfStationarityPlot"), width = 12),
                box(verbatimTextOutput("adfTest"), width = 12)
              )
      )
      ,
      tabItem(tabName = "model",
              fluidRow(
                box(plotOutput("acfPlotModel"), width = 6),
                box(plotOutput("pacfPlotModel"), width = 6)
              ),
              fluidRow(
                box(selectInput("modelType", "Select Model Type:",
                                choices = c("AR", "MA", "ARMA", "ARIMA", "SARIMA", "ARCH", "GARCH")), width = 4),
                box(numericInput("p_order", "AR (p):", value = 1), width = 2),
                box(numericInput("d_order", "I (d):", value = 0), width = 2),
                box(numericInput("q_order", "MA (q):", value = 1), width = 2),
                box(actionButton("fitModel", "Fit Model"), width = 2)
              ),
              fluidRow(
                box(verbatimTextOutput("modelSummary"), width = 12)
              )
      ),
      tabItem(tabName = "residuals",
              fluidRow(
                box(plotOutput("residualsPlot"), width = 12),
                box(plotOutput("acfPlot"), width = 12),
                box(plotOutput("qqPlot"), width = 12),
                box(verbatimTextOutput("shapiroTest"), width = 6),
                box(verbatimTextOutput("ljungBoxTest"), width = 6),
                box(verbatimTextOutput("whiteTest"), width = 12)
              )
      ),
      tabItem(tabName = "forecasting",
              fluidRow(
                box(selectInput("forecastMethod", "Forecast Method:",
                                choices = c("Exponential Smoothing", "Holt-Winters", "ARIMA")), width = 4),
                box(actionButton("runForecast", "Forecast"), width = 2)
              ),
              fluidRow(
                box(plotOutput("forecastPlot"), width = 6),
                box(tableOutput("forecastTable"), width = 6)
              ),
              fluidRow(
                box(verbatimTextOutput("forecastMAE"), width = 6),
                box(verbatimTextOutput("forecastAICBIC"), width = 6)
              )
      ),
      tabItem(tabName = "report",
              fluidRow(
                box(verbatimTextOutput("finalReport"), width = 12)
              )
      )
    )
  )
)

server <- function(input, output, session) {
  data_imported <- reactiveVal()
  time_series_data <- reactiveVal()
  
  observeEvent(input$file, {
    req(input$file)
    df <- read_excel(input$file$datapath)
    data_imported(df)
    ts_obj <- ts(df[[2]], frequency = input$frequency)
    time_series_data(ts_obj)
  })
  
  output$columnDisplay <- renderTable({
    req(data_imported())
    head(data_imported())
  })
  
  output$baseDecompPlot <- renderPlot({
    tsData <- time_series_data()
    req(tsData)
    if (input$decompType == "Additive") {
      autoplot(decompose(tsData, type = "additive"))
    } else {
      autoplot(decompose(tsData, type = "multiplicative"))
    }
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
  
  output$tsPlot <- renderPlot({
    tsData <- transformedData()
    req(tsData)
    autoplot(tsData) + ggtitle(paste("Time Series -", input$transform))
  })
  
  output$decompPlot <- renderPlot({
    autoplot(decompose(na.omit(transformedData()))) + ggtitle("Decomposition")
  })
  
  output$adfTest <- renderPrint({
    adf_result <- adf.test(na.omit(transformedData()), alternative = "stationary")
    print(adf_result)
    cat("\n--- Interpretation ---\n")
    if (adf_result$p.value < 0.05) {
      cat("Since p-value <", adf_result$p.value, "< 0.05, the series is stationary.\n")
    } else {
      cat("Since p-value =", adf_result$p.value, "> 0.05, the series is NOT stationary.\n")
    }
  })
  output$acfStationarityPlot <- renderPlot({
    tsData <- transformedData()
    req(tsData)
    Acf(na.omit(tsData), main = "ACF Plot (Stationarity Check)")
  })
  
  
  modelFit <- eventReactive(input$fitModel, {
    tsData <- na.omit(transformedData())
    req(tsData)
    
    model_type <- input$modelType
    p <- input$p_order
    d <- input$d_order
    q <- input$q_order
    
    if (model_type == "ARIMA" || model_type == "AR" || model_type == "MA" || model_type == "ARMA") {
      return(Arima(tsData, order = c(p, d, q)))
    } else if (model_type == "ARCH" || model_type == "GARCH") {
      spec <- ugarchspec(
        variance.model = list(model = model_type, garchOrder = c(p, q)),
        mean.model = list(armaOrder = c(p, q), include.mean = TRUE)
      )
      return(ugarchfit(spec = spec, data = tsData))
    }
    NULL
  })
  output$modelSummary <- renderPrint({
    model <- modelFit()
    req(model)
    if (inherits(model, "uGARCHfit")) {
      show(model)
    } else {
      summary(model)
    }
  })
  output$acfPacfPlot <- renderPlot({
    tsData <- transformedData()
    req(tsData)
    
    par(mfrow = c(1, 2))  # Show ACF and PACF side-by-side
    Acf(na.omit(tsData), main = "ACF Plot")
    Pacf(na.omit(tsData), main = "PACF Plot")
  })
  # Residual plots and diagnostics
  output$residualsPlot <- renderPlot({
    model <- modelFit()
    req(model)
    
    if (inherits(model, "uGARCHfit")) {
      res <- residuals(model, standardize = TRUE)
    } else {
      res <- residuals(model)
    }
    
    ts.plot(res, main = "Residuals", ylab = "Residuals")
  })
  
  output$acfPlot <- renderPlot({
    model <- modelFit()
    req(model)
    
    if (inherits(model, "uGARCHfit")) {
      res <- residuals(model, standardize = TRUE)
    } else {
      res <- residuals(model)
    }
    
    Acf(res, main = "ACF of Residuals")
  })
  
  output$qqPlot <- renderPlot({
    model <- modelFit()
    req(model)
    
    if (inherits(model, "uGARCHfit")) {
      res <- residuals(model, standardize = TRUE)
    } else {
      res <- residuals(model)
    }
    
    qqnorm(res)
    qqline(res, col = "red")
  })
  
  output$shapiroTest <- renderPrint({
    model <- modelFit()
    req(model)
    
    if (inherits(model, "uGARCHfit")) {
      res <- residuals(model, standardize = TRUE)
    } else {
      res <- residuals(model)
    }
    
    shapiro.test(res)
  })
  
  output$ljungBoxTest <- renderPrint({
    model <- modelFit()
    req(model)
    
    if (inherits(model, "uGARCHfit")) {
      res <- residuals(model, standardize = TRUE)
    } else {
      res <- residuals(model)
    }
    
    Box.test(res, lag = 20, type = "Ljung-Box")
  })
  
  output$whiteTest <- renderPrint({
    model <- modelFit()
    req(model)
    
    if (inherits(model, "uGARCHfit")) {
      cat("White test not supported for GARCH models.")
    } else {
      res <- residuals(model)
      fitted_vals <- fitted(model)
      white_result <- bptest(res ~ fitted_vals + I(fitted_vals^2))
      print(white_result)
    }
  })
  
  
  
  forecastData <- eventReactive(input$runForecast, {
    tsData <- na.omit(transformedData())
    req(tsData)
    
    method <- input$forecastMethod
    if (method == "Exponential Smoothing") {
      fit <- ses(tsData)
    } else if (method == "Holt-Winters") {
      fit <- HoltWinters(tsData)
    } else if (method == "ARIMA") {
      fit <- auto.arima(tsData)
    } else {
      return(NULL)
    }
    
    forecast(fit, h = 8)
  })
  output$forecastPlot <- renderPlot({
    fc <- forecastData()
    req(fc)
    autoplot(fc)
  })
  
  output$forecastTable <- renderTable({
    fc <- forecastData()
    req(fc)
    data.frame(Time = time(fc$mean), Forecast = fc$mean)
  })
  
  output$forecastMAE <- renderPrint({
    fc <- forecastData()
    req(fc)
    cat("Mean Absolute Error (MAE):\n")
    mean(abs(fc$residuals), na.rm = TRUE)
  })
  
  output$forecastAICBIC <- renderPrint({
    fit <- modelFit()
    if (!is.null(fit) && !inherits(fit, "uGARCHfit")) {
      cat("AIC:", AIC(fit), "\n")
      cat("BIC:", BIC(fit), "\n")
    } else {
      cat("Not applicable.")
    }
  })
  
  
  
  
  output$finalReport <- renderPrint({
    cat("\u2713 Dataset Summary:\n")
    print(summary(data_imported()))
    cat("\n\u2713 Transformations Applied: ", input$transform)
    cat("\n\u2713 Model Chosen: ", input$modelType)
    cat("\n\u2713 Orders - p:", input$p_order, ", d:", input$d_order, ", q:", input$q_order)
    model <- modelFit()
    if (!is.null(model)) {
      if (!inherits(model, "uGARCHfit")) {
        cat("\n\u2713 Model AIC:", AIC(model), "\n\u2713 BIC:", BIC(model))
      }
    }
    fc <- forecastData()
    if (!is.null(fc)) {
      cat("\n\u2713 Forecast Completed\n")
      cat("\nForecast Summary:\n")
      print(fc)
    }
  })
}

shinyApp(ui, server)
