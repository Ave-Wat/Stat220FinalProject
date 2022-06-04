library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)

ui <- fluidPage(
  #make the background interesting
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"),
  #create title and main panel for entire website page
  titlePanel("Police Accountability"),
  mainPanel(
    #within website page create tabs for the two different plots, each of which will have a sidebar and main layout
    tabsetPanel(id="main", 
                tabPanel("Figure 1: ", 
                         sidebarLayout(sidebarPanel(),
                                       mainPanel())),
                tabPanel("Figure 2: ",
                         sidebarLayout(sidebarPanel(),
                                       mainPanel())))))
                                         
server <- function(input, output){
  output$plot1 <- renderPlot({
    
  })
  
  output$plot2 <- renderPlot({
    
  })
}
shinyApp(ui = ui, server = server )
