library(shiny)
library(leaflet)
library(tidyverse)
library(shinyWidgets)

load('/Users/Chen/Desktop/GR5243/fall2019-proj2--sec2-grp4/output/boroughs.RData')
# store species in a vector
species <- trees %>%
  filter(status == "Alive") %>%
  select(spc_common) %>%
  lapply(as.character) %>%
  unlist(.) %>%
  as.factor(.)
species <- levels(species)
choiceBronx <- dogs %>%filter(borough =='Bronx') %>%
  select(ZipCode) %>% unique()
choiceBroo <- dogs %>%filter(borough =='Brooklyn') %>%
  select(ZipCode) %>% unique()
choiceMan <- dogs %>%filter(borough =='Manhattan') %>%
  select(ZipCode) %>% unique()
choiceQ <- dogs %>%filter(borough =='Queens') %>%
  select(ZipCode) %>% unique()
choiceSI <- dogs %>%filter(borough =='Staten Island') %>%
  select(ZipCode) %>% unique()

shinyUI(navbarPage("NYC Tree",id="map",
                   tabPanel("Overview",
                            setBackgroundImage(
                              src="background3.jpg"
                            ),
                            h2(strong("Motivations", style = "color:white")),
                            
                            div("This APP is designed for 'NYC Street Tree Caring Program', which is a volunteer program
                                that intend to help better preserve and protect the street trees of New York City. This APP 
                                allows more convnient ways to locate trees, identify specific tree problem, and spot additional 
                                threats to trees from surrounding environment.", style = "color: white; font-size: 16px"),
                            
                            br(),
                            h2(strong("Features", style = "color:white")),
                            
                            div("The main feature of this APP is an interactive map where user can apply mutiple filters, so that 
                                they are able to identify tree based on specific requirements. Moreover, we have incorporate the 
                                NYC dog dataset, in order to identify whether street tree conditions are affected by the the number 
                                and species of dogs.", 
                                style = "color: white; font-size: 16px"),
                   
                            br(),
                            h3(strong("**Dog Damage to Trees", style = "color:white; font-size:16px")),
                            
                            splitLayout(cellWidths = c("25%","25%","25%","25%"),
                              plotOutput("plot2"),plotOutput("plot3"),plotOutput("plot4"),plotOutput("plot5")
                            )
                   
                            ),
                   
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
                                                                                                           "Good", "Poor"),
                                                                           selected = "Good", multiple = TRUE),
                                                               selectInput("species", "Species", choices = species[2:133],
                                                                           selected = "'Schubert' chokecherry", 
                                                                           multiple = TRUE),
                                                               h4("Side Walk Condition"),
                                                               checkboxInput("sidewalk", "Damaged", value = FALSE)),
                                              checkboxGroupInput("borough", h4("Borough"),
                                                                 choices = c("Bronx", "Brooklyn", "Manhattan", "Queens", 
                                                                             "Staten Island"),
                                                                 selected = "Bronx"),
                                              sliderInput("diameter", "Tree Diameter", min=0, max=450, value = c(4, 11))
                                              
                                              
                                 ),
                                absolutePanel(top = 10, right = 10,
                                              sliderInput("range", "Number of Dogs", min(dogs$count), max(dogs$count),
                                                          value = range(dogs$count), step = 1
                                              )
                                )
                                
                                )
                            
                   ),
                   tabPanel("Borough Details",
                            
                            sidebarPanel(id = "probcontrols",
                                          radioButtons("problem", h4("Please select borough"), choices = c("Bronx", 
                                                                                                       "Brooklyn", "Manhattan", "Queens", 
                                                                                                       "Staten Island"), 
                                                                                           selected = "Manhattan"),
                                         p("Branch_light = presence of a branch problem caused by lights or wires"),
                                         p("...")
                                          ),        
                            
                            mainPanel(plotOutput("plot1")))
                   
                   )
        )































