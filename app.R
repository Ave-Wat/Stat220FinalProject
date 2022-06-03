ui <- fluidPage(
  varSelectInput(inputId = "my_var", 
                 label = "Pick a Variable", 
                 data = select(tweets, charCount, nWords,nRealWords)),
  tableOutput("stats")
)

server <- function(input, output){
  output$stats <- renderTable({
    tweets %>% 
      summarize(mean = mean(!!input$my_var), sd = sd(!!input$my_var))
  })
}
shinyApp(ui = ui, server = server )
