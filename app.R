library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)

ui <- navbarPage(
  "Police Accountability",
  tabPanel("About", 
          sidebarLayout(sidebarPanel(),
                        mainPanel())
  ),
  tabPanel("Maps", 
          sidebarLayout(sidebarPanel(),
                        mainPanel())
  ),
  tabPanel("Scatterplots",
          sidebarLayout(sidebarPanel(),
                        mainPanel())
  ),
  tabPanel("Likelihood of death",
           sidebarLayout(sidebarPanel(),
                         mainPanel())
  ),
  #make the background interesting
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom")
)
                                         
server <- function(input, output){
  output$plot1 <- renderPlot({
    
  })
  
  output$plot2 <- renderPlot({
    
  })
}
shinyApp(ui = ui, server = server )
