## INDONESIA BLUE SWIMMING CRAB POLICY EXPLORATION SIMULATION MODEL

library(shiny)
library(shinythemes)

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
ui <- shinyUI(fluidPage(theme=shinytheme("darkly"),
   
   # Application title
   titlePanel("Policy Impacts on Indonesia's Blue Swimming Crab Fishery"),
   fluidRow(column(8,h3("Product of UCSB and MMAF Indonesia Partnership"))),
   
   
   sidebarPanel(width=3,
        wellPanel(
          checkboxInput("SIZELIMITpolicy",label="Size Limit?",value=FALSE),
          checkboxInput("PricePremium",label="Price Premium?",value=FALSE),
          sliderInput("SIZELIMIT",label="Size Limit (mm)",min=50,max=150,value=100,step=10),
          sliderInput("SizeLCompliance",label="Size Limit Compliance",min=0,max=1,value=1,step=0.1),
          checkboxInput("TRAWLBANpolicy",label="Trawl Ban",value=FALSE),
          sliderInput("TrawlBanCompliance",label="Trawl Ban Compliance",min=0,max=1,value=1,step=0.1),
          checkboxInput("OPENACCESSpolicy",label="Open Access?",value=FALSE)
        ),
        actionButton("update","Re-run simulation", icon("refresh")),
        helpText("Note: Baseline scenario (black) is constant effort, with no size limit,
                 trawl ban, or price premium.")
      ),
   mainPanel(width=9,
        # Output plots
         fluidRow(
          column(3,plotOutput("TotBio")),
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
)

server <- shinyServer(function(input, output) {
  
  getParams <- function() {
    params <- lapply(paramNames,function(p) input[[p]])
    names(params) <- paramNames
    params
  }
  
  simdat <- eventReactive(input$update, {do.call(BSCFUNCTION,getParams())})
   
  output$TotHarvest <- renderPlot({
    plot_all(simdat(),"TotHarvest",label="Total Harvest")
  })
  output$BioAge <- renderPlot({
    plot_all(simdat(),"BIOM",label="Biomass (MT)")
  })
  output$BioCW <- renderPlot({
    plot_all(simdat(),"BioCW",label="Biomass (MT)")
  })
  output$Effort <- renderPlot({
    plot_all(simdat(),"Effort",label="Effort")
  })
  output$TotBio <- renderPlot({
    plot_all(simdat(),"TotBio",label="Biomass (MT)")
  })
  output$Ind <- renderPlot({
    plot_all(simdat(),"Pop",label="Individuals")
  })
  output$NetHarvest <- renderPlot({
    plot_all(simdat(),"NetHarvest",label="Gillnet Harvest")
  })
  output$NetProfit <- renderPlot({
    plot_all(simdat(),"NetProfit",label="Gillnet Profit")
  })
  output$TrapHarvest <- renderPlot({
    plot_all(simdat(),"TrapHarvest",label="Trap Harvest")
  })
  output$TrapProfit <- renderPlot({
    plot_all(simdat(),"TrapProfit",label="Trap Profit")
  })
  output$TrawlHarvest <- renderPlot({
    plot_all(simdat(),"TrawlHarvest",label="Trawl Harvest")
  })
  output$TrawlProfit <- renderPlot({
    plot_all(simdat(),"TrawlProfit",label="Trawl Profit")
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

