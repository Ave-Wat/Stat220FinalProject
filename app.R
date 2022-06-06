library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)
library(leaflet)
library(maps)
library(randomForest)
library(ggplot2)
library(ggrepel)

#load data
joined_cities <- read_csv('data/joined_cities.csv')

#fit preliminary rf model with all variables
rf_data <- joined_cities %>%
  mutate(had_killing = as.factor(ifelse(killings > 0, 'yes', 'no')),
         had_killing = fct_relevel(had_killing, 'yes')) %>%
  select(-c('city', 'state_code', 'state', 'killings')) %>%
  drop_na()

killings_rf_all <- randomForest(had_killing ~ . , data = rf_data, mtry = 14)
varImpPlot(killings_rf_all, n.var = 15, main = 'City Stats Most Influential to Classification')

#set ui constants
div_style <- "color:black; background-color:white; 
             margin-bottom:20px; border: 2px solid black; 
             border-radius: 8px; font-size: medium; 
             padding-top: 5px; padding-right: 5px; 
             padding-bottom: 5px; padding-left: 5px;"

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
                            c("All Races" = "all", "White" = "white", "Non-White" = "non_white", "Black" = "black", "Hispanic" = "hispanic")
                            )
                        ),
                        mainPanel(leafletOutput("residence_map"))
                      )
             ),
             tabPanel("Residency and Police Killings",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            "race2",
                            "Percentage of Police Living in their Communities by Race",
                            c("All Races" = "all", "White" = "white", "Non-White" = "non_white", "Black" = "black", "Hispanic" = "hispanic")
                          )
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
           sidebarLayout(sidebarPanel(),
                         mainPanel(verticalLayout(
                           div(strong('Our prediction based on the selected city stats: '), style = div_style),
                           div(strong(textOutput(outputId = 'class')), style = div_style),
                           div(strong("Below is a breakdown of which city characteristics our model deemed
                                      most important for determining whether or not a city had a police killing.
                                      Note that many of these characteristics are difficult to explain or 
                                      likely confounded with other characteristics: for example, longitude is
                                      probably highly correlated with population. "), 
                               style = div_style),
                           plotOutput(outputId = 'var_imp_plot'))))
  ),
  tabPanel("Citations",
           p("This website hopes to show a precursory analysis of the relationships between police brutality, demographics, and police residence.")
  ),
  #make the background interesting
  setBackgroundColor(
    color = c("#F7FBFF", "#687178"),
    gradient = "radial",
    direction = c("top", "left"))
)

server <- function(input, output){
  filteredData <- reactive({
    select(joined_cities, input$race)
  })
  
  output$residence_map <- renderLeaflet({ 
    if(input$race == "all"){
      leaflet(data=joined_cities) %>%
        addTiles(data = map("state", fill = TRUE, plot = FALSE)) %>%
        addPolygons(data = map("state", fill = TRUE, plot = FALSE), fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
        addCircles(lng=joined_cities$lon, lat=joined_cities$lat, radius = ~150000*joined_cities$all, weight = 1, color = "#777777", 
                   #fillColor = ~colorNumeric(brewer.pal.info["Blues",], joined_cities$all),
                   fillOpacity = 0.7, popup = ~paste(joined_cities$all)
        )
    } else if (input$race == "white"){
      leaflet(data=joined_cities) %>%
        addTiles(data = map("state", fill = TRUE, plot = FALSE)) %>%
        addPolygons(data = map("state", fill = TRUE, plot = FALSE), fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
        addCircles(lng=joined_cities$lon, lat=joined_cities$lat, radius = ~150000*joined_cities$white, weight = 1, color = "#777777", 
                   #fillColor = ~colorNumeric(brewer.pal.info["Blues",], joined_cities$all),
                   fillOpacity = 0.7, popup = ~paste(joined_cities$white)
        )
    } else if (input$race == "non_white"){
      leaflet(data=joined_cities) %>%
        addTiles(data = map("state", fill = TRUE, plot = FALSE)) %>%
        addPolygons(data = map("state", fill = TRUE, plot = FALSE), fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
        addCircles(lng=joined_cities$lon, lat=joined_cities$lat, radius = ~150000*joined_cities$non_white, weight = 1, color = "#777777", 
                   #fillColor = ~colorNumeric(brewer.pal.info["Blues",], joined_cities$non_white),
                   fillOpacity = 0.7, popup = ~paste(joined_cities$non_white)
        )
    } else if (input$race == "black"){
      leaflet(data=joined_cities) %>%
        addTiles(data = map("state", fill = TRUE, plot = FALSE)) %>%
        addPolygons(data = map("state", fill = TRUE, plot = FALSE), fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
        addCircles(lng=joined_cities$lon, lat=joined_cities$lat, radius = ~150000*joined_cities$black, weight = 1, color = "#777777", 
                   #fillColor = ~colorNumeric(brewer.pal.info["Blues",], joined_cities$all),
                   fillOpacity = 0.7, popup = ~paste(joined_cities$black)
        )
    } else if (input$race == "hispanic"){
      leaflet(data=joined_cities) %>%
        addTiles(data = map("state", fill = TRUE, plot = FALSE)) %>%
        addPolygons(data = map("state", fill = TRUE, plot = FALSE), fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
        addCircles(lng=joined_cities$lon, lat=joined_cities$lat, radius = ~150000*joined_cities$hispanic, weight = 1, color = "#777777", 
                   #fillColor = ~colorNumeric(brewer.pal.info["Blues",], joined_cities$all),
                   fillOpacity = 0.7, popup = ~paste(joined_cities$hispanic)
        )
    } else {
      
    }
  })
  
  output$residency_scatterplot <- renderPlot({
    ggplot(joined_cities, aes_string(x=input$race2, y="killings", label="city")) +
      geom_point() +
      geom_label_repel(box.padding   = 0.35, 
                       point.padding = 0.5,
                       segment.color = 'grey50') +
      geom_smooth(method='lm')
      
  })

  output$class <- renderText({
    ifelse('yes' == 'yes', 'We predict that your city has had a police killing.', 'We predict that your city has not had a police killing.')
  })
  
  output$var_imp_plot <- renderPlot({varImpPlot(killings_rf_all, n.var = 15, main = 'City Stats Most Influential to Classification')})
}
shinyApp(ui = ui, server = server )
