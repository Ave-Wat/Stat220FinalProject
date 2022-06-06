library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)
library(leaflet)
library(maps)

joined_cities <- read_csv('data/joined_cities.csv')

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
             tabPanel("Residency and Police Killings",
                      sidebarLayout(
                        sidebarPanel(
                          
                        ),
                        mainPanel(plotOutput(outputId = "residency_scatterplot"))
                      )
             )
           )
  ),
  tabPanel("Scatterplots",
           sidebarLayout(sidebarPanel(),
                         mainPanel())
  ),
  tabPanel("Will Your City Have a Police Killing?",
           fluidRow(column(width = 12, numericInput(inputId = 'pop', label = 'City Pop.', value = 5000))),
           fluidRow(column(width = 12))
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
  filteredData <- reactive({
    select(joined_cities, input$race)
  })
  
  output$residence_map <- renderLeaflet({ 
    residency <- select(joined_cities, input$race)
    leaflet(data=joined_cities) %>%
      addTiles(data = map("state", fill = TRUE, plot = FALSE)) %>%
      addPolygons(data = map("state", fill = TRUE, plot = FALSE), fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
      
  })
  
  observe({
    leafletProxy("residence_map", data = filteredData()) %>%
      addCircles(lng=joined_cities$lon, lat=joined_cities$lat, radius = ~150000*filteredData, weight = 1, color = "#777777", 
                 #fillColor = ~colorNumeric(brewer.pal.info["Blues",], joined_cities$all),
                 fillOpacity = 0.7, popup = ~paste(filteredData)
      )
  })
  
  
  
  output$residency_scatterplot <- renderPlot({
    ggplot(data=joined_cities, aes(x=all, y=killings_by_city))
  })
}
shinyApp(ui = ui, server = server )
