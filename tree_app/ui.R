#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)

 load('./output/boroughs.RData')
# # store species in a vector
 species <- trees %>%
     filter(status == "Alive") %>%
     select(spc_common) %>%
     lapply(as.character) %>%
     unlist(.) %>%
     as.factor(.)
 species <- levels(species)

shinyUI(navbarPage("NYC Tree",id="map",
                   
                   ### FIRST MAP PANEL ###
                   tabPanel("NYC Tree Overview",
                            div(class="outer",
                                tags$style(".outer {position: fixed; top: 41px; left: 0; right: 0; 
                                           bottom: 0; overflow: hidden; padding: 0;}"),
                                leafletOutput("map", width = "100%", height = "100%"),
                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = 60, right = 40, bottom = "auto",
                                              width = 330, height = "auto",

                                              h3("Selection Panel"),

                                              radioButtons("status", "Status", choices = c("Alive","Dead","Stump"),
                                                           selected = "Alive"),
                                              conditionalPanel(condition = "input.status == 'Alive'",
                                                               selectInput("health", "Health", choices = c("Fair", 
                                                                                                           "Good", "Poor",
                                                                                                          multiple = TRUE),
                                                                           selected = "Good"),
                                                               selectInput("species", "Species", choices = species[2:133],
                                                                           selected = "'Schubert' chokecherry", 
                                                                           multiple = TRUE),
                                                               h4("Side Walk Condtion"),
                                                               checkboxInput("sidewalk", "Damaged", value = FALSE)),
                                              checkboxGroupInput("borough", h4("Borough"),
                                                                 choices = c("Bronx", "Brooklyn", "Manhattan", "Queens", 
                                                                             "Staten Island"),
                                                                 selected = "Bronx"),
                                              sliderInput("diameter", "Tree Diameter", min=0, max=450, value = c(4, 11))


                   )
                   )
                   )
))

                    
                                               
                                               
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              
    
