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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB") %>%  
            setView(lng=40.730610, lat= -73.935242, zoom=8)
    })

})
