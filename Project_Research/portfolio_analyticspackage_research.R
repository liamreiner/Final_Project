library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(quantmod)
library(shiny)



ui <- fluidPage (pageWithSidebar(
  
  headerPanel("Portfolio Analytics"),
  
  sidebarPanel(
    selectInput("dataset", "Choose A Dataset:",
                choices=c("EDHEC", "Indices")),
    p(br()),
    numericInput("riskfree", "Risk free Rate: ", 0),
    sliderInput("frontier_points", "Number Of Frontier Points :", value=10, 
                min=5, max=100, step=1),
    numericInput("target_return", "Target Return %: ", 10),
    numericInput("target_risk", "Target Risk %", 2),
    selectInput("constraints", "Constraints: ",
                choices=c("None","Long Only")),
    selectInput("estimator", "Portfolio Estimator",
                choices=c("MVE", "Kendall","Covariance OGK", "Covariance MCD",
                          "NNVE")),
    selectInput("optimize", "Optimize For: ",
                choices=c("Minimum Risk","Maximum Return")),
    p(br()),
    actionButton("goButton", "Start"),
    
    helpText("This is the ATYS experimental risk charting app. More details will be 
             filled in as the app becomes more developed."),
    
    p(br(), a("ATYS Bonds Home", href="http://atysbonds.com/blog")),
    p(br(), a("ATYS Support", href="mailto:support@atysbonds.com"))
    
  ),
  
  mainPanel(
    conditionalPanel("updateBusy() || $('html').hasClass('shiny-busy')",
                     id='progressIndicator',
                     "Processing Request",
                     div(id='progress',includeHTML("timer.js"))
    ),
    tags$head(tags$style(type="text/css",
                         '#progressIndicator {',
                         '  position: float; top: 8px; right: 8px; width: 200px; height: 50px;',
                         '  padding: 8px; border: 1px solid #CCC; border-radius: 8px;',
                         '}')),
    
    tabsetPanel(
      
      tabPanel("Charts",				
               h4("Tailored Frontier Plot"),
               plotOutput("frontierPlot"),
               h4("Portfolio Weights"),
               plotOutput("weightsPlot"),
               h4("Weighted Returns Plot"),
               plotOutput("weightedReturnsPlot")),
      
      tabPanel("Summary",
               verbatimTextOutput("summaryTable"))
    )
  )
))


server <- function(input, output) {

options( error = recover )

get_portfolio_frontier <- function (data, riskFree, estimator="kendallEstimator",
                                   optimizeParam, frontierPoints, riskFreeRate,
                                   targetReturn, targetRisk, constraints="none") {
  data = timeSeries(data)
  
  riskFree = (riskFree/100)/12
  targetReturn = targetReturn/100
  targetRisk = targetRisk/100
  
  Spec1 = portfolioSpec(
    model=list(
      type="MV",
      optimize=optimizeParam,
      estimator=estimator,
      tailRisk=list(),
      params=list(alpha=0.05,a=2)),
    portfolio = list(
      weights=NULL,
      targetReturn=targetReturn,
      targetRisk=targetRisk,
      riskFreeRate = riskFree,
      nFrontierPoints = frontierPoints),
    optim = list(
      solver = "solveRquadprog",
      objective=NULL,
      control=list(meq=2)))
  
  
  if (constraints != "none") {
    setSolver(Spec1) = "solveRshortExact"
    portfFrontier = portfolioFrontier(data=data, spec=Spec1, constraints=constraints)
  } else {
    portfFrontier = portfolioFrontier(data=data, spec=Spec1)  
  }
  return(portfFrontier)
}

getColor <- function(numberOfEntries) {
  return(divPalette(numberOfEntries, "RdBu"))
}

shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "EDHEC" = edhec,
           "Indices" = indexes)
  })
  
  constraints <- reactive({
    switch(input$constraints,
           "None" = "none",
           "Long Only" = "LongOnly")
  })
  
  portfolio_estimator <- reactive({
    switch(input$estimator,
           "MVE" = "mveEstimator",
           "Kendall" = "kendallEstimator",
           "Covariance MCD" = "covMcdEstimator",
           "Covariance OGK" = "covOGKEstimator",
           "NNVE" = "nnveEstimator")
  })
  
  optimize_parameters <- reactive({
    switch(input$optimize,
           "Minimum Risk" = "minRisk",
           "Maximum Return" = "maxReturn")
  })
  
  riskfree_rate <- reactive({input$riskfree})
  frontier_points <- reactive({input$frontier_points})
  target_return <- reactive({input$target_return})
  target_risk <- reactive({input$target_risk})
  
  
  
  output$frontierPlot <- renderPlot({
    # Take a dependency on input$goButton
    input$goButton
    
    if (input$goButton == 0) {
      return()
    } else {
      portfolio <- datasetInput()
      pfrontier = get_portfolio_frontier(portfolio, riskFree=riskfree_rate(),
                                         estimator=portfolio_estimator(), optimizeParam=optimize_parameters(),
                                         frontierPoints = frontier_points(), riskFreeRate=riskfree_rate(),
                                         targetReturn=target_return(), targetRisk=target_risk(), 
                                         constraints = constraints())
      
      tailoredFrontierPlot(pfrontier, risk="Sigma")
    }
  })
  
  output$weightsPlot <- renderPlot({
    input$goButton
    if (input$goButton == 0) {
      return()
    } else {
      portfolio <- datasetInput()
      pfrontier = get_portfolio_frontier(portfolio, riskFree=riskfree_rate(),
                                         estimator=portfolio_estimator(), optimizeParam=optimize_parameters(),
                                         frontierPoints = frontier_points(), riskFreeRate=riskfree_rate(),
                                         targetReturn=target_return(), targetRisk=target_risk(), 
                                         constraints = constraints())
      weightsPlot(pfrontier, mtext=FALSE, col=getColor(length(colnames(portfolio))))
    }
  })
  
  output$weightedReturnsPlot <- renderPlot({
    input$goButton
    
    if (input$goButton == 0) {
      return()
    } else {
      portfolio <- datasetInput()
      pfrontier = get_portfolio_frontier(portfolio, riskFree=riskfree_rate(),
                                         estimator=portfolio_estimator(), optimizeParam=optimize_parameters(),
                                         frontierPoints = frontier_points(), riskFreeRate=riskfree_rate(),
                                         targetReturn=target_return(), targetRisk=target_risk(), 
                                         constraints = constraints())
      weightedReturnsPlot(pfrontier, mtext=FALSE, col=getColor(length(colnames(portfolio))))
    }
  })
  
  output$summaryTable <- renderPrint({
    input$goButton
    
    if (input$goButton == 0) {
      return()
    } else {
      portfolio <- datasetInput()
      pfrontier = get_portfolio_frontier(portfolio, riskFree=riskfree_rate(),
                                         estimator=portfolio_estimator(), optimizeParam=optimize_parameters(),
                                         frontierPoints = frontier_points(), riskFreeRate=riskfree_rate(),
                                         targetReturn=target_return(), targetRisk=target_risk(), 
                                         constraints = constraints())
      print(pfrontier)
    }
  })
  
  
})

}

shinyApp(ui = ui, server = server)

##The code below comes from...
##<https://gist.github.com/chibondking/5606460/revisions>