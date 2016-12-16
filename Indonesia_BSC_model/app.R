## INDONESIA BLUE SWIMMING CRAB POLICY EXPLORATION SIMULATION MODEL

library(shiny)

# load source model functions
source(file="bsc_src_fxns.R")

# Params
paramNames <- c('sizelimit','size','compliance','trawlban','openTrap','openNet','openTrawl')

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Policy Impacts on Indonesia's Blue Swimming Crab Fishery"),
   fluidRow(h3("Product of UCSB and MMAF Indonesia Partnership")),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
      column(3,
        wellPanel(
          checkboxInput("sizelimit",label="Size Limit?",value=FALSE),
          sliderInput("size",label="Size Limit (mm)",min=50,max=150,value=100,step=10),
          sliderInput("compliance",label="Size Limit Compliance",min=0,max=1,value=1,step=0.1),
          checkboxInput("trawlban",label="Trawl Ban",value=FALSE),
          checkboxInput("openTrap",label="Trap Open Access",value=F),
          checkboxInput("openNet",label="Gillnet Open Access",value=F),
          checkboxInput("openTrawl",label="Trawl Open Access",value=F)
        )
      ),
      column(9,
         fluidRow(
          column(3,plotOutput("BioTot")),
          column(3,plotOutput("TotHarvest")),
          column(3,plotOutput("Effort"))
         ),
         fluidRow(
          column(3,plotOutput("Ind")),
          column(3,plotOutput("BioAge")),
          column(3,plotOutput("BioCW"))
         ),
         fluidRow(
          column(3,plotOutput("TrapHarvest")),
          column(3,plotOutput("NetHarvest")),
          column(3,plotOutput("TrawlHarvest"))
         ),
         fluidRow(
          column(3,plotOutput("TrapProfit")),
          column(3,plotOutput("NetProfit")),
          column(3,plotOutput("TrawlProfit"))
         )
        )
      )
))

server <- shinyServer(function(input, output) {
  
  getParams <- function() {
    params <- lapply(paramNames,function(p) input[[p]])
    names(params) <- paramNames
    params
  }
  
  simdat <- reactive(do.call(runSim,getParams()))
   
  output$BioTot <- renderPlot({
    plotBioTot(simdat())
  })
  output$TotHarvest <- renderPlot({
    plotTotHarvest(simdat())
  })
  output$BioAge <- renderPlot({
    plotBioAge(simdat())
  })
  output$BioCW <- renderPlot({
    plotBioCW(simdat())
  })
  output$Effort <- renderPlot({
    plotEffort(simdat())
  })
  output$Ind <- renderPlot({
    plotInd(simdat())
  })
  output$NetHarvest <- renderPlot({
    plotNetHarvest(simdat())
  })
  output$NetProfit <- renderPlot({
    plotNetProfit(simdat())
  })
  output$TrapHarvest <- renderPlot({
    plotTrapHarvest(simdat())
  })
  output$TrapProfit <- renderPlot({
    plotTrapProfit(simdat())
  })
  output$TrawlHarvest <- renderPlot({
    plotTrawlHarvest(simdat())
  })
  output$TrawlProfit <- renderPlot({
    plotTrawlProfit(simdat())
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

