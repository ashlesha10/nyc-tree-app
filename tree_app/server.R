#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

load('../output/boroughs.RData')
load('../output/borough_problems.RData')

shinyServer(function(input, output, session) {
  
  #trees <- trees %>%
#    filter(sidewalk != "")
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    
    if(input$sidewalk == FALSE){
      damage <- "NoDamage"
    }
    else{
      damage <- "Damage"
    }
    if(input$status == 'Alive'){
      trees %>%
        filter(status == input$status) %>%
        filter(health == input$health) %>%
        filter(spc_common == input$species) %>%
        filter(sidewalk == damage) %>%
        filter(borough == input$borough) %>%
        filter(tree_dbh >= input$diameter[1] & tree_dbh <= input$diameter[2])
    }
    else{
      trees %>%
        filter(status == input$status) %>%
        filter(borough == input$borough)
    }
    
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    filteredData() %>%
      leaflet() %>%
      addCircleMarkers(fillColor = "red", radius = 3,
                       stroke = FALSE, fillOpacity = 0.5,label = ~address) %>%
      addProviderTiles("CartoDB.Positron")
  })
  
    output$plot1 <- renderPlot({
    
    ggplot(data = borough_problems, aes(x = Problem, y = Manhattan)) +
      geom_bar(stat = "identity") +
      labs(x = "Problem", y = "Count", 
           title = "Tree problems in selected borough")
    
  })
  
})
