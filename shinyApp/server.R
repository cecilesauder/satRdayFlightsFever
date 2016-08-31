library(shiny)
library(readxl)
library(dplyr)
geocode<-dismo::geocode
library(budflights)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  explorer_city <- reactive({
    countries <- input$country
    data <- if( "All countries" %in% countries ){
      unique( cities$city )
    } else {
      cities %>% 
        filter( country %in% countries ) %$%
        city %>%
        unique
    }
    c( "All cities", data )
  })
  
  observe({
    updateSelectInput(session, "city", choices = explorer_city(), selected =  "All cities" )
  })
  
  # the table to display on the data explorer tab. 
  # the ui is only updated when the submit button for this tab is clicked
  explorer_table <- reactive({
    input$explorer_button
    
    # isolate to avoide dependency on selection controls
    isolate({
      data <- flights
      if (!identical(input$country,"All countries")) {
        data <- data %>%
          filter(country %in% input$country)
      }
      cities <- explorer_city()
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
    })
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable(explorer_table()))

  # the table to display on the summary tab. 
  # the ui is only updated when the submit button for this tab is clicked
  summary_table <- reactive({
    input$summary_button
    
    # using isolate to avoid dependency on this tab controls
    isolate({
      data <- flights
      if(!identical(input$groupV, "None")){
        data <- data %>%
          group_by_(.dots = input$groupV) %>%
          summarise(npassengers = sum(passengers), nflights= sum(flights), capacity=sum(capacity), weight=sum(weight))
      }
      data  
    })
  })
  
  #Summary table
  output$tabSummary <- DT::renderDataTable(DT::datatable(summary_table()))

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
  
  dataInBounds <- reactive({
    if( is.null(input$map_bounds ) ){
      "foo"
    } else {
      "bar"
    }
  })
  
  output$coords <- renderText({
    dataInBounds()
  })
  
  
  
})
