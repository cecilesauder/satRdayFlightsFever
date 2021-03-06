library(shiny)
library(readxl)
library(dplyr)
geocode<-dismo::geocode
library(budflights)
library(leaflet)
library(magrittr)
library(ggplot2)
library(ggvis)

country5<-flights %>%
    group_by(country) %>%
    summarise(npassengers = sum(passengers), nflights= sum(flights),
              capacity=sum(capacity), weight=sum(weight)) %>%
    arrange( desc(nflights)) %>%
    head(n=5) %$% country

nmonths <- nrow(distinct(flights, year, month))
min_date <- as.Date( "2007/01/01" )
max_date <- as.Date( "2012/06/01" )


shinyUI(fluidPage(
  includeCSS("www/styles.css"),
  navbarPage("SatRday flights fever : Flights from/to Budapest",

######################################################################################################     

tabPanel("Interactive map",
         div( class = "outer", 
              leafletOutput("map", height = "100%"), 
              
              absolutePanel(
                id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 400, height = "auto",
                
                h3("Summary data"), 
                
                textOutput("map_flights_count"), 
                textOutput("map_flights_count_details"),
                br(), 
                textOutput("map_flights_city_country"),
                hr(), 
                textOutput("map_passengers_count"), 
                textOutput("map_passengers_count_details"),
                hr(),
                br(),
                
                # h3("seasonnality (passengers)"), 
                # plotOutput("map_seasonnality_plot"), 
                # textOutput("map_slider_info")
                
                DT::dataTableOutput("map_table")
                
              ), 
              
              
              absolutePanel(id="map_slider_panel", fixed = TRUE, bottom = 20, left=20, right=20, width="auto", 
                            draggable = TRUE, 
                            
                            sliderInput("map_slider", label = "",  
                                        min=min_date, max = max_date, 
                                        value=c(min_date, max_date), 
                                        width= "100%", timeFormat = c( "%B %Y")
                            )
              )
              
         )
         
      ), 

###############################################################################################
     navbarMenu("Tables",
       tabPanel("Data explorer",
              h3("Filter and select data"), br(),
              
              fluidRow(
                column(2, 
                       wellPanel(
                    checkboxInput("country_checkbox", label = "All Countries", value = TRUE), 
                    conditionalPanel(
                      condition = 'input.country_checkbox == false', 
                      selectInput("country", label="Select countries:",
                                  unique(as.character(flights$country)),
                                  selected=NULL, multiple=TRUE)                 
                    )
                  
                )), 
                column(2, 
                       wellPanel(
                    checkboxInput("city_checkbox", label = "All Cities", value = TRUE),
                    conditionalPanel( 
                      condition = 'input.city_checkbox == false', 
                      selectInput("city", label="Select cities:",
                                  unique(as.character(flights$city)),
                                  selected=NULL, multiple=TRUE)  
                      )
                )),
                
                column(2, wellPanel(
                  selectInput("direction", label=NULL,
                            c("All directions", unique(as.character(flights$direction))),
                            selected="All directions", multiple=FALSE)
                )), 
                
                column(2, 
                       wellPanel(
                    checkboxInput("years_checkbox", label = "All Years", value = TRUE),    
                    conditionalPanel( 
                      condition = "input.years_checkbox == false", 
                      selectInput(
                        "year",label="Select year:",
                        unique(as.character(flights$year)),
                        selected=NULL, 
                        multiple=TRUE
                      )
                     
                  )
                )),
               
                column(2, 
                       wellPanel(
                    checkboxInput("variables_checkbox", label = "All Variables", value = TRUE), 
                    conditionalPanel( 
                      condition = "input.variables_checkbox == false", 
                      selectInput("selectVar", label = "Select Variable(s) :", 
                                  names(flights), selected = NULL, multiple = TRUE)    
                      )
                  
                )
            )),
          
          mainPanel(
            DT::dataTableOutput("table")
          )
        
     ),
     tabPanel("Summary",
              
                fluidRow(
                  column(5, 
                         wellPanel(
                         selectInput("groupV", label = "Select Variable(s) to group by :", 
                                     c("None", "year", "month", "country","city", "direction" ), 
                                     selected = "None", multiple = TRUE)
                         
                  )
                )
              ),
              mainPanel(
                DT::dataTableOutput("tabSummary")
              )
              
     )
     ),
     
     
######################################################################################################     
     

     
######################################################################################################     
     navbarMenu("Plots",
       tabPanel("Seasonality", 
                navlistPanel("Seasonality plot",
                             tabPanel("Design 1",
                                      h4("Number of passengers by month grouped by years"),
                                      plotOutput("map_seasonnality_plot"),
                                      downloadButton('downloadPlot', 'Download plot')
                                      ),
                             tabPanel("Design 2",
                                      h4("Number of passengers by month grouped by years"),
                                      plotOutput("seasonality_ggplot"),
                                      downloadButton('downloadPlot2', 'Download plot')
                                      ),
                             tabPanel("Design 3",                                      
                                      h4("Number of passengers by month grouped by years"),
                                      uiOutput("ggvis_ui"),
                                      ggvisOutput("ggvis")
                                      ))
               ),
       tabPanel("By country",
                h3("By country plot"),
                wellPanel(
                  fluidRow(
                    column(6,
                           
                           selectInput("selectCountry", label = "Select countries to display :", 
                                       c(unique(flights$country)), selected = country5, multiple = TRUE)
                           
                    ),
                    column(6, 
                           
                           sliderInput("plot_slider", label = "",  
                                       min=min_date, max = max_date, 
                                       value=c(min_date, max_date), 
                                       width= "100%", timeFormat = c( "%b %Y")
                           )
                           
                    )
                  )
                ),
                htmlOutput("plot"),
                br(),
                htmlOutput("plot2")
                
       )
     )
     
  )
)
)
