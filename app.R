library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)

ui <- navbarPage(
  "Police Accountability",
  tabPanel("About", 
           HTML('<center><img src="download.png" width="400"></center>'),
           h3(),
           p("This website hopes to show a precursory analysis of the relationships between police brutality, demographics, and police residence.")
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
  tabPanel("Citations",
           p("This website hopes to show a precursory analysis of the relationships between police brutality, demographics, and police residence.")
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
