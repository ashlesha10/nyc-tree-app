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
library(ggplot2)
library(dplyr)
library(ggpubr)

load('../output/boroughs.RData')
load('../output/borough_problems.RData')

dogs <- read.csv('../output/NYC_Dogs.csv')
dog_count <- dogs %>%
  group_by(borough, ZipCode) %>%
  summarise(countbyzip = n())
dogs <- dogs %>%
  left_join(dog_count, by = c('ZipCode','borough'))
dogs.count <- dog_count[,2:3]
names(dogs.count) <- c("ZipCode", "Dogs")

shinyServer(function(input, output, session) {
  
  output$plot2 <- renderPlot({
    trees.dead <- trees[which(trees$status == "Dead"),]
    trees.dead$postcode <- as.character(trees.dead$postcode)
    trees.count <- trees.dead %>%
      group_by(postcode) %>% 
      summarise(countbyzip = n())
    names(trees.count) <- c("ZipCode", "Dead Trees")
    dogs.trees <- merge(data.frame(dogs.count, row.names=NULL), data.frame(trees.count, row.names=NULL), by = "ZipCode", all = FALSE)
    ggscatter(dogs.trees, x = "Dogs", y = "Dead.Trees", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", title = "Dogs vs Trees",
              xlab = "Number of Dogs", ylab = "Number of Dead Trees")
  })
  output$plot3 <- renderPlot({
    trees.poor <- trees[which(trees$health == "Poor"),]
    trees.poor$postcode <- as.character(trees.poor$postcode)
    treespoor.count <- trees.poor %>%
      group_by(postcode) %>% 
      summarise(countbyzip = n())
    names(treespoor.count) <- c("ZipCode", "Poor Trees")
    dogs.poor <- merge(data.frame(dogs.count, row.names=NULL), data.frame(treespoor.count, row.names=NULL), by = "ZipCode", all = FALSE)
    ggscatter(dogs.poor, x = "Dogs", y = "Poor.Trees", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", title = "Dogs vs Poor Health Trees", 
              xlab = "Number of Dogs", ylab = "Number of Poor Health Trees") 
  })
  output$plot4 <- renderPlot({
    trees.root <- trees[which(trees$root_other == "Yes"),]
    trees.root$postcode <- as.character(trees.root$postcode)
    treesroot.count <- trees.root %>%
      group_by(postcode) %>% 
      summarise(countbyzip = n())
    names(treesroot.count) <- c("ZipCode", "Root Problem")
    dogs.root <- merge(data.frame(dogs.count, row.names=NULL), data.frame(treesroot.count, row.names=NULL), by = "ZipCode", all = FALSE)
    ggscatter(dogs.root, x = "Dogs", y = "Root.Problem", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Number of Dogs", ylab = "Number of Trees with other Root Problems") +
      labs(title = "Dogs vs Trees \nwith Other Root Problems")
  })
  output$plot5 <- renderPlot({
    trees.trunk <- trees[which(trees$trnk_other == "Yes"),]
    trees.trunk$postcode <- as.character(trees.trunk$postcode)
    treestrunk.count <- trees.trunk %>%
      group_by(postcode) %>% 
      summarise(countbyzip = n())
    names(treestrunk.count) <- c("ZipCode", "Trunk Problem")
    dogs.trunk <- merge(data.frame(dogs.count, row.names=NULL), data.frame(treestrunk.count, row.names=NULL), by = "ZipCode", all = FALSE)
    ggscatter(dogs.trunk, x = "Dogs", y = "Trunk.Problem", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Number of Dogs", ylab = "Number of Trees with other Trunk Problems") + 
      labs(title = "Dogs vs Trees \nwith other Trunk Problems")
  })
  
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
  
  vars <- reactive({ 
    
    bors <- borough_problems %>%
      select(input$problem, Problem)
    
    colnames(bors) <- c("Borough", "Problem")
    
    as.data.frame(bors)
    
  })
  
  output$plot1 <- renderPlot({
    
    ggplot(data = vars(), aes(x = reorder(Problem, -Borough), y = Borough)) +
      geom_bar(stat = "identity", fill = "#FF6666") + 
      labs(x = "Problem", y = "Count", title = "Problems in selected borough") 
    
  })
  
})
