## INDONESIA BLUE SWIMMING CRAB POLICY EXPLORATION SIMULATION MODEL

library(shiny)

# load source model functions
source(file="bsc_src_fxns_2.R")

# Params
paramNames <- c("PricePremium",
                "TrawlBanCompliance", 
                "SizeLCompliance",
                "SIZELIMIT",
                "SIZELIMITpolicy",
                "TRAWLBANpolicy",
                "OPENACCESSpolicy")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Policy Impacts on Indonesia's Blue Swimming Crab Fishery"),
   fluidRow(column(8,h3("Product of UCSB and MMAF Indonesia Partnership"))),
   
   
   fluidRow(
      column(3,
        # Inputs
        wellPanel(
          checkboxInput("SIZELIMITpolicy",label="Size Limit?",value=FALSE),
          checkboxInput("PricePremium",label="Price Premium?",value=FALSE),
          sliderInput("SIZELIMIT",label="Size Limit (mm)",min=50,max=150,value=100,step=10),
          sliderInput("SizeLCompliance",label="Size Limit Compliance",min=0,max=1,value=1,step=0.1),
          checkboxInput("TRAWLBANpolicy",label="Trawl Ban",value=FALSE),
          sliderInput("TrawlBanCompliance",label="Trawl Ban Compliance",min=0,max=1,value=1,step=0.1),
          checkboxInput("OPENACCESSpolicy",label="Open Access?",value=FALSE)
        )
      ),
      column(9,
       plotOutput("allPlot")
        # # Output plots
        #  fluidRow(
        #   column(3,plotOutput("BioTot")),
        #   column(3,plotOutput("TotHarvest")),
        #   column(3,plotOutput("Effort"))
        #  ),
        #  fluidRow(
        #   column(3,plotOutput("Ind")),
        #   column(3,plotOutput("BioAge")),
        #   column(3,plotOutput("BioCW"))
        #  ),
        #  fluidRow(
        #   column(3,plotOutput("TrapHarvest")),
        #   column(3,plotOutput("NetHarvest")),
        #   column(3,plotOutput("TrawlHarvest"))
        #  ),
        #  fluidRow(
        #   column(3,plotOutput("TrapProfit")),
        #   column(3,plotOutput("NetProfit")),
        #   column(3,plotOutput("TrawlProfit"))
        #  )
        )
      )
))

server <- shinyServer(function(input, output) {
  
  getParams <- function() {
    params <- lapply(paramNames,function(p) input[[p]])
    names(params) <- paramNames
    params
  }
  
  simdat <- reactive(do.call(BSCFUNCTION,getParams()))
   
  output$allPlot <- renderPlot({
    plot_all(simdat())
  })
  # output$TotHarvest <- renderPlot({
  #   plotTotHarvest(simdat())
  # })
  # output$BioAge <- renderPlot({
  #   plotBioAge(simdat())
  # })
  # output$BioCW <- renderPlot({
  #   plotBioCW(simdat())
  # })
  # output$Effort <- renderPlot({
  #   plotEffort(simdat())
  # })
  # output$Ind <- renderPlot({
  #   plotInd(simdat())
  # })
  # output$NetHarvest <- renderPlot({
  #   plotNetHarvest(simdat())
  # })
  # output$NetProfit <- renderPlot({
  #   plotNetProfit(simdat())
  # })
  # output$TrapHarvest <- renderPlot({
  #   plotTrapHarvest(simdat())
  # })
  # output$TrapProfit <- renderPlot({
  #   plotTrapProfit(simdat())
  # })
  # output$TrawlHarvest <- renderPlot({
  #   plotTrawlHarvest(simdat())
  # })
  # output$TrawlProfit <- renderPlot({
  #   plotTrawlProfit(simdat())
  # })
})

# Run the application 
shinyApp(ui = ui, server = server)

