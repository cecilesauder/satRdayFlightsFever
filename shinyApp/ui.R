library(shiny)
library(readxl)
library(dplyr)
geocode<-dismo::geocode
library(budflights)
library(leaflet)
library(magrittr)

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
                width = 500, height = "auto",
                
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
              wellPanel(
              fluidRow(
                
                column(3, 
                              
            selectInput("country", label=NULL,
                        c("All countries", unique(as.character(flights$country))),
                        selected="All countries", multiple=TRUE),
            selectInput("city", label=NULL,
                        c("All cities", unique(as.character(flights$city))),
                        selected="All cities", multiple=TRUE)
                ),
            column(3,
            selectInput("direction", label=NULL,
                        c("All directions", unique(as.character(flights$direction))),
                        selected="All directions", multiple=FALSE),
            selectInput("year",label=NULL,
                        c("All years", unique(as.character(flights$year))),selected="All years", 
                        multiple=TRUE)
                ),
            column(3, 
              selectInput("selectVar", label = "Select Variable(s) :", 
                          c("All", names(flights)), selected = "All", multiple = TRUE)
            ),
            column(3, ""),
            column(3,
                   br(), 
                  actionButton("explorer_button", label = "SUBMIT")
            )
              
            )),
          
          mainPanel(
            DT::dataTableOutput("table")
          )
        
     ),
     tabPanel("Summary",
              wellPanel(
                fluidRow(
                  column(5,
                         selectInput("groupV", label = "Select Variable(s) to group by :", 
                                     c("None", names(flights)), selected = "None", multiple = TRUE)
                         
                  ),
                  column(4, ""),
                  column(3,
                         br(),
                         actionButton("summary_button", label = "SUBMIT")
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
                h3("Seasonality plot (number of passengers)"),
                #fluidRow(
                  plotOutput("map_seasonnality_plot", width= "100%")
                #)
                ),
       tabPanel("By country",
                h3("By country plot"),
                wellPanel(
                  fluidRow(
                    column(5,
                           
                           selectInput("selectCountry", label = "Select countries to display :", 
                                       c(unique(flights$country)), selected = country5, multiple = TRUE)
                           
                    ),
                    column(5, 
                           
                           sliderInput("plot_slider", label = "",  
                                       min=min_date, max = max_date, 
                                       value=c(min_date, max_date), 
                                       width= "100%", timeFormat = c( "%b %Y")
                           )
                           
                    ),
                    column(2,
                           br(), br(),
                           actionButton("plot_button", label = "SUBMIT")
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
