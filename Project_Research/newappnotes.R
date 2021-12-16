library(shiny)
library(quantmod)
library(tidyverse)

ui <- fluidPage(
  plotOutput("plot")
)

server <- function(input, output, session) { 
  getSymbols(Symbols = "NRXP", src = "yahoo") 

  output$plot <- renderPlot({
    chartSeries(NRXP, TA=NULL)  
  })
}
shinyApp(ui, server)