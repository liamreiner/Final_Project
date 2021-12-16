library(shiny)
library(tidyquant)
library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(tidyverse)
library(forecast)
library(ts343)
library(knitr)


##
# RUN THE UI
##

ui <- fluidPage(
  titlePanel("Analysis of Equities: Portfolio Growth"),
  
  sidebarLayout(
    sidebarPanel(
      
      tabsetPanel(
        
        tabPanel("Portfolio",
                 
                 textInput("Ticker1",                             # Enter ticker symbol to get data
                           label = h3("Enter Ticker Symbol"), 
                           value = "JPM"),
                 textInput("Ticker2",                             # Enter ticker symbol to get data
                           label = h3("Enter Ticker Symbol"), 
                           value = "TSLA"),
                 textInput("Ticker3",                             # Enter ticker symbol to get data
                           label = h3("Enter Ticker Symbol"), 
                           value = "MA"),
                 textInput("Ticker4",                             # Enter ticker symbol to get data
                           label = h3("Enter Ticker Symbol"), 
                           value = "MSFT"),
                 textInput("Ticker5",                             # Enter ticker symbol to get data
                           label = h3("Enter Ticker Symbol"), 
                           value = "BRK-A"),
                 textInput("Benchmark1",                          # Enter benchmark ticker symbol to get data
                           label = h3("Enter Benchmark Symbol"),
                           value = "^GSPC"),
                 
                 
                 dateRangeInput("Date", label = h3("Date range"), start = "2015-01-01" ),
                 
                 helpText("Use YYYY-MM-DD Format"),
                 
                 checkboxInput("SMR", label = "Simple Linear Regression", value = FALSE),
                 checkboxInput("Smoother", label = "Smoother", value = FALSE),
                 checkboxInput("DoubleExponentialMovingAverage", label = "Double Exponential Moving Average", value = FALSE),
                 checkboxInput("benchmark", label = "Benchmark Ticker", value = FALSE),
                 
                 numericInput("DBLMovAvgN", label = h3("Weeks for Double Exponential Moving Average"), value = 27)
                 
        )
        
        ,
        
        tabPanel("Weighting & Investment Size",
                 h5("Weights Must Sum to 1"),
                 textInput("Weight1",                             # Enter ticker symbol to get data
                           label = h3("Enter Weighting on 1st Stock"), 
                           value = "0.3"),
                 textInput("Weight2",                             # Enter ticker symbol to get data
                           label = h3("Enter Weighting on 2nd Stock"), 
                           value = "0.133"),
                 textInput("Weight3",                             # Enter ticker symbol to get data
                           label = h3("Enter Weighting on 3rd Stock"), 
                           value = "0.133"),
                 textInput("Weight4",                             # Enter ticker symbol to get data
                           label = h3("Enter Weighting on 4th Stock"), 
                           value = "0.134"),
                 textInput("Weight5",                             # Enter ticker symbol to get data
                           label = h3("Enter Weighting on 5th Stock"), 
                           value = "0.3"),
                 textInput("Investment",
                           label = h3("Enter Investment Size"),
                           value = "10000"),
                 textInput("ForecastDate",
                           label = h3("Start Year for Forecasting (Year of Initial Investment)"),
                           value = "2015"),
                 helpText("Use YYYY Format")
                 
        )
      )),
    
    
    mainPanel(tabsetPanel(id = "tabs",
                          tabPanel("Historical Portfolio Performance", plotOutput("plot", height= 1000),
                                   textOutput("SumStats")
                                   
                          ),
                          
                          tabPanel("Aggregated Weekly Portfolio Returns", plotOutput("bar", height= 1000)
                                   
                          ),
                          
                          tabPanel("Forecasted Portfolio Performance", plotOutput("plot2", height= 1000),
                                   textOutput("model")
                                   
                          )
                          
                          
    )
    )
  ))


##
# RUN THE SERVER
##

server <- function(input, output) {
  
  portfolio <- reactive({
    
    weekly_portfolio_returns <- c(input$Ticker1, input$Ticker2, input$Ticker3, input$Ticker4, input$Ticker5)
    weekly_portfolio_returns <- tq_get(weekly_portfolio_returns,
                                        get  = "stock.prices",
                                        from = "1850-01-01",
                                        to   = input$Date[2]) %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "weekly", 
                   col_rename = "Portfolio")
    
    weekly_portfolio_returns <- weekly_portfolio_returns %>%
      filter(date >= input$Date[1]) %>%
      filter(date <= input$Date[2])
    
    weights <- c(input$Weight1,input$Weight2,input$Weight3,input$Weight4,input$Weight5)
    
    portfolio <- weekly_portfolio_returns %>%
      tq_portfolio(assets_col   = symbol, 
                   returns_col  = Portfolio, 
                   weights      = weights, 
                   col_rename   = "investment.growth",
                   wealth.index = TRUE) %>%
      mutate(investment.growth = investment.growth * as.numeric(input$Investment))
    
    
    portfolio$MA6 <- TTR::DEMA(portfolio$investment.growth, n = input$DBLMovAvgN)
    
    
    return(portfolio)
    
  })
  
  weekly_returns <- reactive({
    
    weekly_returns <- c(input$Ticker1, input$Ticker2, input$Ticker3, input$Ticker4, input$Ticker5)
    weekly_returns <- tq_get(weekly_returns,
                                        get  = "stock.prices",
                                        from = "1850-01-01",
                                        to   = input$Date[2]) %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "weekly", 
                   col_rename = "Weekly_Returns")
    
    weekly_returns <- weekly_returns %>%
      filter(date >= input$Date[1]) %>%
      filter(date <= input$Date[2])
    
    return(weekly_returns)
    
  })
  
  
  
  benchmark <- reactive({
    benchmark <- c(input$Benchmark1)
    benchmark <- tq_get(benchmark,
                        get  = "stock.prices",
                        from = "1850-01-01",
                        to   = input$Date[2]) %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = "weekly",
                   col_rename = "Benchmark")
    
    benchmark <- benchmark %>%
      filter(date >= input$Date[1]) %>%
      filter(date <= input$Date[2])
    
    benchmark <- benchmark %>%
      tq_portfolio(assets_col   = symbol,
                   returns_col  = Benchmark,
                   col_rename   = "benchmark.growth",
                   wealth.index = TRUE) %>%
      mutate(benchmark.growth = benchmark.growth * as.numeric(input$Investment))
    
    
    return(benchmark)
    
  })
  
  portfolio_forecast <- reactive({
    
    portfolio_forecast <- ts(portfolio()$investment.growth, frequency = 52, start = input$ForecastDate)
    
    portfolio_forecast <- hw(portfolio_forecast, initial="simple", h = 260, level=0.90)
    
    return(portfolio_forecast)
    
  })
  
  
  output$plot <- renderPlot({
    
    g = ggplot(data = portfolio(), aes(x = date, y = investment.growth)) +
      geom_line(size = 1) +
      labs(x = "Time", y = "Portfolio Value") +
      scale_y_continuous(labels = scales::dollar) +
      theme_grey(base_size = 16)
    
    if(input$SMR == TRUE){
      g <- g + geom_smooth(method=lm, se=FALSE, color = "red")
    }
    
    if(input$Smoother == TRUE){
      g <- g + geom_smooth(color = "blue", se = FALSE)
    }
    
    if(input$DoubleExponentialMovingAverage == TRUE){
      g <- g + geom_line(aes(x = date, y = MA6), color = "purple", size = 0.75)
    }
    
    if(input$benchmark == TRUE){
      g <- g + geom_line(data = benchmark(), aes(x = date, y = benchmark.growth), color = "mediumseagreen", size = 0.75)
    }
    
    return(g) })
  
  output$SumStats <- renderText({
    textStats = ""
    b = sd(portfolio()$investment.growth)
    c = max(portfolio()$investment.growth)
    d = min(portfolio()$investment.growth)
    textStats = paste("Standard Deviation of the Portfolio = ", round(b,0), "|",
                      "Max Portfolio Value = ", round(c,0), "|",
                      "Min Portfolio Value = ", round(d,0))
    
    
    return(textStats)
    
  })
  
  
  output$bar <- renderPlot({
    
    ggplot(data = weekly_returns(), aes(x = date, y = Weekly_Returns)) +
      geom_area(data = weekly_returns(), stat = "identity", aes(fill = symbol)) +
      labs(title = "Portfolio Returns by Stock (Weekly)",
           x = "Date", y = "Weekly Returns (%)") +
      scale_y_continuous(labels = scales::percent) +
      facet_wrap(~symbol) + 
      theme_grey(base_size = 16)
    
  })
  
  output$plot2 <- renderPlot({""
    
    
    sesplotmod = function (mod, include = NULL, attach = "fits", main = "", ylab = "", xlab = "", 
                           lwd = 2) 
    {
      x = mod$x
      n = length(x)
      past = mod$fitted
      upper = mod$upper
      lower = mod$lower
      nextfit = mod$mean
      if (!is.null(include)) {
        x = subset(x, start = n - include + 1)
        past = subset(past, start = n - include + 1)
      }
      if (attach == "series") {
        nextfit = ts(c(tail(x, 1), nextfit), start = end(x), 
                     freq = frequency(x))
        lower = ts(c(tail(x, 1), lower), start = end(x), freq = frequency(x))
        upper = ts(c(tail(x, 1), upper), start = end(x), freq = frequency(x))
      }
      if (attach == "fits") {
        
        nextfit = ts(c(tail(past, 1), nextfit), start = end(past), 
                     freq = frequency(past))
        lower = ts(c(tail(past, 1), lower), start = end(past), 
                   freq = frequency(past))
        upper = ts(c(tail(past, 1), upper), start = end(past), 
                   freq = frequency(past))
      }
      
      ts.plot(x, past, nextfit, lower, upper, col = c("black", 
                                                      "blue", "blue", "red", "red"), 
              lwd = lwd, main = main, ylab = ylab, xlab = xlab)
    }
    
    sesplotmod(mod = portfolio_forecast(), ylab = "Portfolio Value (USD)", xlab = "Date (Years)", main = "Forecasted Portfolio Performance")
    
    
  })
  
  output$model <- renderText({
    textModel = ("Holt-Winter Smoothing Model Used to Forecast Portfolio Performance")
    
    return(textModel)
    
  })
  
  
}

##
# RUN THE APP
##

shinyApp(ui = ui, server = server)
