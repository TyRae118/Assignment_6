# BDA400 Assignment 6
# Technical Analysis Dashboard

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

# ---------------------------
# User interface
# ---------------------------

ui <- fluidPage(
  titlePanel("Stock Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock symbol:", value = "AAPL"),
      
      dateRangeInput(
        "dates",
        "Date range:",
        start = "2024-01-01",
        end = "2025-12-31"
      ),
      
      selectInput(
        "timeframe",
        "Time frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      selectInput(
        "chart_type",
        "Chart type:",
        choices = c("Line", "Area", "Candlestick")
      ),
      
      checkboxGroupInput(
        "indicators",
        "Technical indicators:",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI", "MACD")
      ),
      
      numericInput("short_period", "Short MA period:", 20, min = 2),
      numericInput("long_period", "Long MA period:", 50, min = 3),
      
      actionButton("load_data", "Load stock data")
    ),
    
    mainPanel(
      h3(textOutput("stock_title")),
      plotOutput("price_plot", height = "450px"),
      conditionalPanel(
        condition = "input.indicators.indexOf('RSI') >= 0",
        plotOutput("rsi_plot", height = "200px")
      ),
      conditionalPanel(
        condition = "input.indicators.indexOf('MACD') >= 0",
        plotOutput("macd_plot", height = "220px")
      ),
      h4("Recent trading signals"),
      tableOutput("signal_table")
    )
  )
)

# ---------------------------
# Server
# ---------------------------

server <- function(input, output, session) {
  
  # Fetch Yahoo Finance data when the button is clicked
  stock_data <- eventReactive(input$load_data, {
    symbol <- toupper(trimws(input$symbol))
    
    validate(
      need(nchar(symbol) > 0, "Enter a stock symbol."),
      need(
        input$dates[1] <= input$dates[2],
        "The start date must be before the end date."
      )
    )
    
    result <- tryCatch(
      getSymbols(
        symbol,
        src = "yahoo",
        from = input$dates[1],
        to = input$dates[2] + 1,
        auto.assign = FALSE
      ),
      error = function(e) NULL
    )
    
    validate(
      need(
        !is.null(result) && NROW(result) > 0,
        "No data was found. Check the symbol, dates, or connection."
      )
    )
    
    result
  }, ignoreInit = TRUE)
  
  # Convert data to the selected time frame and calculate indicators
  analysis_data <- reactive({
    stock <- stock_data()
    
    if (input$timeframe == "Weekly") {
      stock <- to.weekly(stock, indexAt = "lastof", drop.time = TRUE)
    } else if (input$timeframe == "Monthly") {
      stock <- to.monthly(stock, indexAt = "lastof", drop.time = TRUE)
    }
    
    short_n <- as.integer(input$short_period)
    long_n <- as.integer(input$long_period)
    
    validate(
      need(short_n >= 2, "Short MA period must be at least 2."),
      need(long_n > short_n, "Long MA period must exceed short MA period."),
      need(
        NROW(stock) >= max(long_n, 35),
        "Not enough data for these indicators. Choose a longer date range or shorter periods."
      )
    )
    
    data <- data.frame(
      Date = as.Date(index(stock)),
      Open = as.numeric(Op(stock)),
      High = as.numeric(Hi(stock)),
      Low = as.numeric(Lo(stock)),
      Close = as.numeric(Cl(stock))
    )
    
    data$Short_MA <- as.numeric(SMA(data$Close, n = short_n))
    data$Long_MA <- as.numeric(SMA(data$Close, n = long_n))
    data$RSI <- as.numeric(RSI(data$Close, n = 14))
    
    macd_values <- MACD(
      data$Close,
      nFast = 12,
      nSlow = 26,
      nSig = 9,
      percent = FALSE
    )
    
    data$MACD <- as.numeric(macd_values[, "macd"])
    data$MACD_Signal <- as.numeric(macd_values[, "signal"])
    
    # A signal occurs when the short MA crosses the long MA
    data$Signal <- "Hold"
    
    for (i in 2:nrow(data)) {
      previous <- c(data$Short_MA[i - 1], data$Long_MA[i - 1])
      current <- c(data$Short_MA[i], data$Long_MA[i])
      
      if (!any(is.na(c(previous, current)))) {
        if (previous[1] <= previous[2] && current[1] > current[2]) {
          data$Signal[i] <- "Buy"
        } else if (previous[1] >= previous[2] && current[1] < current[2]) {
          data$Signal[i] <- "Sell"
        }
      }
    }
    
    data
  })
  
  output$stock_title <- renderText({
    stock_data()
    paste(toupper(trimws(input$symbol)), "Stock Prices")
  })
  
  # Price chart and moving averages
  output$price_plot <- renderPlot({
    data <- analysis_data()
    
    p <- ggplot(data, aes(x = Date))
    
    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), colour = "steelblue")
    } else if (input$chart_type == "Area") {
      p <- p +
        geom_area(aes(y = Close), fill = "lightblue", alpha = 0.6) +
        geom_line(aes(y = Close), colour = "steelblue")
    } else {
      # Candlestick wicks and bodies
      p <- p +
        geom_segment(aes(xend = Date, y = Low, yend = High)) +
        geom_segment(
          aes(
            xend = Date,
            y = Open,
            yend = Close,
            colour = Close >= Open
          ),
          linewidth = 3
        ) +
        scale_colour_manual(
          values = c("TRUE" = "darkgreen", "FALSE" = "firebrick"),
          guide = "none"
        )
    }
    
    if ("Moving Averages" %in% input$indicators) {
      p <- p +
        geom_line(aes(y = Short_MA), colour = "orange", na.rm = TRUE) +
        geom_line(aes(y = Long_MA), colour = "purple", na.rm = TRUE)
    }
    
    signals <- subset(data, Signal != "Hold")
    
    p +
      geom_point(
        data = signals,
        aes(y = Close),
        colour = "black",
        size = 2
      ) +
      geom_text(
        data = signals,
        aes(y = Close, label = Signal),
        vjust = -1,
        size = 3
      ) +
      labs(
        x = "Date",
        y = "Price",
        subtitle = "Orange: short MA | Purple: long MA"
      ) +
      theme_minimal()
  })
  
  # RSI is shown separately because it uses a different scale
  output$rsi_plot <- renderPlot({
    req("RSI" %in% input$indicators)
    data <- analysis_data()
    
    ggplot(data, aes(Date, RSI)) +
      geom_line(colour = "darkorange", na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed") +
      labs(title = "Relative Strength Index", x = NULL, y = "RSI") +
      theme_minimal()
  })
  
  # MACD is also shown on its own scale
  output$macd_plot <- renderPlot({
    req("MACD" %in% input$indicators)
    data <- analysis_data()
    
    ggplot(data, aes(Date)) +
      geom_line(aes(y = MACD), colour = "steelblue", na.rm = TRUE) +
      geom_line(aes(y = MACD_Signal), colour = "red", na.rm = TRUE) +
      labs(
        title = "MACD",
        subtitle = "Blue: MACD | Red: signal line",
        x = "Date",
        y = "MACD"
      ) +
      theme_minimal()
  })
  
  output$signal_table <- renderTable({
    data <- analysis_data()
    signals <- subset(data, Signal != "Hold")
    signals$Date <- as.character(signals$Date)
    
    tail(signals[, c("Date", "Close", "Signal")], 10)
  })
}

# Start the app
shinyApp(ui, server)