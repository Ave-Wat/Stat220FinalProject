library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)
library(leaflet)
library(maps)

ui <- navbarPage(
  "Police Accountability",
  tabPanel("About", 
           HTML('<center><img src="download.png" width="400"></center>'),
           h3(),
           p("This website hopes to show a precursory analysis of the relationships between police brutality, demographics, and police residence.")
  ),
  tabPanel("Police Residency", 
           tabsetPanel(
             tabPanel("Percentage of Police Living in their Communities", 
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            "race",
                            "Percentage of Police Living in their Communities by Race",
                            c("All Races" = "all", "White" = "white", "Non-White" = "non-white", "Black" = "black", "Hispanic" = "hispanic")
                            ),
                        ),
                        mainPanel(leafletOutput("residence_map"))
                      )
             ),
             tabPanel("",
                      h2("hi")
             )
           )
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
  output$residence_map <- renderLeaflet({ 
    print(unlist(input$race))
    leaflet(data=joined_cities) %>%
      addTiles(data = map("state", fill = TRUE, plot = FALSE)) %>%
      addPolygons(data = map("state", fill = TRUE, plot = FALSE), fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
      addCircles(lng=joined_cities$lon, lat=joined_cities$lat, radius = ~150000*joined_cities[,unlist(input$race)], weight = 1, color = "#777777", 
                 #fillColor = ~colorNumeric(brewer.pal.info["Blues",], joined_cities$all),
                 fillOpacity = 0.7, popup = ~paste(joined_cities[,unlist(input$race)])
      )
  })
  
  
  
  output$plot2 <- renderPlot({
    
  })
}
shinyApp(ui = ui, server = server )
