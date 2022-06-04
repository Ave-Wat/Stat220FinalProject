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
  tabPanel("Maps", 
          sidebarLayout(
            sidebarPanel(),
            mainPanel(
              leafletOutput("residence_map")
            ))
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
    leaflet(data=joined_cities) %>% 
      addTiles(data = map("state", fill = TRUE, plot = FALSE)) %>%
      addPolygons(data = map("state", fill = TRUE, plot = FALSE), fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
      addCircles(lng=joined_cities$lon, lat=joined_cities$lat#, radius = ~10^mag/10, weight = 1, color = "#777777"
                 #, fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
                 )
  })
  
  output$plot2 <- renderPlot({
    
  })
}
shinyApp(ui = ui, server = server )
