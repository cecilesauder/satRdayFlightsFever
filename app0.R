#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(dplyr)
geocode<-dismo::geocode
library(budflights)
library(leaflet)


# Define UI for application
ui <- shinyUI(
  fluidPage(
  includeCSS("styles.css"),
  navbarPage("SatRday flights fever",

             tabPanel("Data explorer",
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Filter data"),
                          selectInput("country", label=NULL,
                                      c("All countries", unique(as.character(flights$country))),selected="All countries", multiple=TRUE),
                          selectInput("city", label=NULL,
                                      c("All cities", unique(as.character(flights$city))),selected="All cities", multiple=TRUE),
                          selectInput("direction", label=NULL,
                                      c("All directions", unique(as.character(flights$direction))),selected="All directions", multiple=FALSE),
                          selectInput("year",label=NULL,
                                      c("All years", unique(as.character(flights$year))),selected="All years", multiple=TRUE),
                          selectInput("selectVar", label = "Select Variable(s) :", 
                                      c("All", names(flights)), selected = "All", multiple = TRUE),
                          submitButton("SUBMIT")
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
                          submitButton("SUBMIT")
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
                      div( class = "outer", leafletOutput("map", height = "100%") )
             )
  )
  )
)


# Define server 
server <- shinyServer(function(input, output) {
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- flights
    if (!identical(input$country,"All countries")) {
      data <- data %>%
        filter(country %in% input$country)
    }
    if (!identical(input$city, "All cities")) {
      data <- data %>%
        filter(city %in% input$city)
    }
    if (!identical(input$year, "All years")) {
      data <- data %>%
        filter(year %in% input$year)
    }
    if (!identical(input$direction, "All directions")) {
      data <- data %>%
        filter(direction %in% input$direction)
    }
    if (!identical(input$selectVar, "All")) {
      data <-data %>%
        select_( .dots = input$selectVar)
    }
    data
  }))
  
  #Summary table
  output$tabSummary <- DT::renderDataTable(DT::datatable({
    data <- flights
    if(!identical(input$groupV, "None")){
      data <- data %>%
        group_by_(.dots = input$groupV) %>%
        summarise(npassengers = sum(passengers), nflights= sum(flights), capacity=sum(capacity), weight=sum(weight))
    }
    data
  }))
  
  #plot
  output$plot <- renderPlot({
    plot(data)
  }
    
  )
  # Create the map
  output$map <- renderLeaflet({
    m<-leaflet() %>%
      addTiles( ) %>%
      addCircles(cities$longitude, cities$latitude, popup=paste(cities$city, cities$country, sep=", "))
    m
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

