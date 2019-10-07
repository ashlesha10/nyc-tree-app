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
library(htmltools)

load('../output/boroughs.RData')
#load('../output/borough_problems.RData')

dogs <- read.csv('../output/NYC_Dogs.csv')
dog_count <- dogs %>%
  group_by(borough, ZipCode) %>%
  summarise(countbyzip = n())
dogs <- dogs %>%
  left_join(dog_count, by = c('ZipCode','borough'))

shinyServer(function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  tree_filteredData <- reactive({
    
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
  
  dog_filteredData <- reactive({
    dogs %>%
      filter(borough == input$borough) %>%
      filter(countbyzip >= input$range[1] & countbyzip <= input$range[2])
      
  })
  
  dtdt<-reactive({
    dog_filteredData()%>%
    select(ZipCode,longitude,latitude,countbyzip) %>%
    unique() %>%
    mutate(prop = countbyzip/sum(countbyzip)*10)
  })
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    tree_filteredData() %>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(fillColor = "red", radius = 3,
                       stroke = FALSE, fillOpacity = 0.5) %>%
      addCircleMarkers(data = dtdt(), radius = ~countbyzip/10, stroke = FALSE,
                       fillOpacity = ~prop, label = mapply(function(x, y) {
                         HTML(sprintf("Zip %s:\n %s", htmlEscape(x), htmlEscape(y)))},
                         dtdt()$ZipCode, dtdt()$countbyzip, SIMPLIFY = F),
                       labelOptions = labelOptions(noHide = F, direction = 'up'))
  })
  
  # output$plot1 <- renderPlot({
  #   ggplot(data = borough_problems, aes(x = Borough, y = input$borough)) +
  #     geom_bar(stat = "identity") +
  #     labs(x = "Problem", y = "Count", 
  #          title = "Tree problems in selected borough")
  # })

  # observe({
  #   leafletProxy("map", data = dog_filteredData()) %>%
  #     clearShapes() %>%
  #     addCircleMarkers(fillColor = "blue", radius = 3,
  #                      stroke = FALSE, fillOpacity = 0.5)
  # })
  
  
})
