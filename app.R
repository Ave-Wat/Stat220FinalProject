library(shiny)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)
library(leaflet)
library(maps)
library(randomForest)

#load data
joined_cities <- read_csv('data/joined_cities.csv')

#fit preliminary rf model with all variables
rf_data <- joined_cities %>%
  mutate(had_killing = as.factor(ifelse(killings > 0, 'yes', 'no')),
         had_killing = fct_relevel(had_killing, 'yes')) %>%
  select(-c('city', 'state_code', 'state', 'killings')) %>%
  drop_na()

killings_rf_all <- randomForest(had_killing ~ . , data = rf_data, mtry = 14)

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
           sidebarLayout(sidebarPanel(div(strong("Enter the police force size, the proportion of police that live
                                                 within the city, and the population for a city, and we will predict whether 
                                                 that city has had a police killing using a random forest model."), style = div_style),
                                      numericInput(inputId = 'force', label = 'Police Force Size: ',
                                                   min = 1, max = max(rf_data$police_force_size), value = mean(rf_data$police_force_size)),
                                      numericInput(inputId = 'residence', label = 'Proportion of Police Living in City: ',
                                                   min = 0, max = 1, value = mean(rf_data$all)),
                                      numericInput(inputId = 'pop', label = 'Population: ',
                                                   min = 1, max = max(rf_data$total_population), value = mean(rf_data$total_population))),
                         mainPanel(verticalLayout(
                           div(strong('Our prediction based on the selected city stats: '), style = div_style),
                           div(strong(textOutput(outputId = 'class')), style = div_style),
                           div(strong("Below is a breakdown of which city characteristics (from the entire dataset, not just the 3 used above) our model deemed
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

  output$class <- renderText({
    ifelse('yes' == 'yes', 'We predict that your city has had a police killing.', 'We predict that your city has not had a police killing.')
  })
  
  output$var_imp_plot <- renderPlot({varImpPlot(killings_rf_all, n.var = 15, main = 'City Stats Most Influential to Classification')})
}
shinyApp(ui = ui, server = server )
