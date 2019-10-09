library(shiny)
library(leaflet)
library(tidyverse)

load('../output/boroughs.RData')
dogs <- read.csv('../output/NYC_Dogs.csv')

dog_count <- dogs %>%
  group_by(borough, ZipCode) %>%
  summarise(countbyzip = n())
dogs <- dogs %>%
  left_join(dog_count, by = c('ZipCode','borough'))


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
                            h2(strong("MOTIVATION", style = "color:black")),
                            
                            div("Trees are essential to the well-being of humans living in cities. 
                                They help regulate the temperature, they filter the air, and lower the overall noise level. 
                                Especially in times of climate change, trees in cities are perceived an essential element to ensure sustainability", style = "color: black; font-size: 16px"),
                            br(),
                            div("The NYC Tree App is designed for the ‘NYC Street Tree Caring Program’, 
                                which is a volunteer program that intends to help better preserve and protect the street trees of New York City. 
                                This App allows more convenient ways to locate trees, identify specific tree problems, 
                                and spot additional threats to trees from surrounding environment.", style = "color:black; font-size:16px"),
                            br(),
                            div("In recent years, dogs pose an increasing threat to the health of trees", 
                                a("(See details)",
                                  href="https://www.ltoa.org.uk/resources/dog-damage-to-trees"), ".",
                                "Our research indicates that dogs can harm trees in certain ways. 
                                Due to this fact, the App enables volunteers with insides into how many dogs are registered in a certain Zip code in NYC, 
                                helping the volunteers to take protective measures.", style = "color:black; font-size:16px"),
                            br(),
                            h2(strong("FEATURES", style = "color:black")),
                            
                            div("The main feature of the NCY Tree App is an interactive map where users can apply multiple filters, 
                                so that they are able to identify trees based on specific requirements.", style = "color:black; font-size:16px"),
                            br(),
                            div("Moreover, we have incorporated the NYC dog dataset, in order to identify whether street tree conditions are affected by the number and species of dogs.",
                                style = "color:black; font-size:16px"),
                            br(),
                            div("Since NYC differs heavily from borough to borough, we provide relevant insights into which problems trees are facing in which borough. 
                                This information is intended to support the volunteers preserving the trees in their specific neighborhood with the right measures. ",
                                style = "color:black; font-size:16px"),
                            br(),
                            h2(strong("Dog Damage to Trees", style = "color:black")),
                            
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
                                                               selectInput("health", "Health", choices = c("Fair", "Good", "Poor"),
                                                                           selected = "Good", multiple = FALSE),
                                                               h5(strong("Species")),
                                                               checkboxInput("all_species", "All Species", value = TRUE),
                                                              
                                                               conditionalPanel(condition = "input.all_species == 0",
                                                                                selectInput("species", "", choices = species[2:133],
                                                                                            selected = "'Schubert' chokecherry", 
                                                                                            multiple = FALSE)
                                                                                ),
                                                               h5(strong("Side Walk Condition")),
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
                                         radioButtons("problem", h4("Please select borough"),
                                                      choices = c("Bronx", "Brooklyn",
                                                                  "Manhattan", "Queens", "Staten Island"), 
                                                      selected = "Manhattan"),
                                         h4("Variables"),
                                         p("Branch_light = Branch problem caused by lights or wires"),
                                         p("Branch_shoe = Branch problem caused by sneakers"),
                                         p("Branch_other = Another branch problem"),
                                         p("Root_stone = Root problem due to paving stones"),
                                         p("Root_grate = Root problemdue to metal grates"),
                                         p("Root_other = Another root problem"),
                                         p("Trunk_wire = Trunk problem due to wires around trunk"),
                                         p("Trunk_light = Trunk problem due to lightning installed"),
                                         p("Trunk_other = Another trunk problem")
                            ),        
                            mainPanel(plotOutput("plot1")))
                   )
        )































