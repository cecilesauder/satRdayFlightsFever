library(shiny)
library(readxl)
library(dplyr)
geocode<-dismo::geocode
library(budflights)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
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
