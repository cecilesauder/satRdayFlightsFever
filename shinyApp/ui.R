library(shiny)
library(readxl)
library(dplyr)
geocode<-dismo::geocode
library(budflights)
library(leaflet)

shinyUI(fluidPage(
  includeCSS("www/styles.css"),
  navbarPage("SatRday flights fever",
             
     tabPanel("Data explorer",
        sidebarLayout(
          sidebarPanel(
            titlePanel("Filter data"),
            selectInput("country", label=NULL,
                        c("All countries", unique(as.character(flights$country))),
                        selected="All countries", multiple=TRUE),
            selectInput("city", label=NULL,
                        c("All cities", unique(as.character(flights$city))),
                        selected="All cities", multiple=TRUE),
            selectInput("direction", label=NULL,
                        c("All directions", unique(as.character(flights$direction))),selected="All directions", multiple=FALSE),
            selectInput("year",label=NULL,
                        c("All years", unique(as.character(flights$year))),selected="All years", multiple=TRUE),
            selectInput("selectVar", label = "Select Variable(s) :", 
                        c("All", names(flights)), selected = "All", multiple = TRUE),
            actionButton("explorer_button", label = "submit")
          ),
          mainPanel(
            DT::dataTableOutput("table")
          )
        )
     ),
     
     
     
     tabPanel("Summary",
        sidebarLayout(
          sidebarPanel(
            selectInput("groupV", label = "Select Variable(s) to group by :", 
                        c("None", names(flights)), selected = "None", multiple = TRUE),
            actionButton("summary_button", label = "submit")
          ),
          mainPanel(
            DT::dataTableOutput("tabSummary")
          )
        )
              
     ),
     
     
     tabPanel("Plot", 
        plotOutput("plot")                      
     ),
     
     
     tabPanel("Interactive map",
        div( class = "outer", 
             leafletOutput("map", height = "100%"), 
             
             absolutePanel(
               id = "controls", class = "panel panel-default", fixed = TRUE,
               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
               width = 330, height = "auto",
                       
               textOutput("coords")
             )
        )
              
     )
  )
)
)
