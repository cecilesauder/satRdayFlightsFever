library(shiny)
library(readxl)
library(dplyr)
geocode<-dismo::geocode
library(budflights)
library(leaflet)
library(googleVis)
library(RColorBrewer)

dates <- distinct( flights, year, month ) %>%
  arrange( year, month) %>%
  as.data.frame()


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
  output$table <- DT::renderDataTable(DT::datatable(explorer_table(), rownames = FALSE))

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
          summarise(npassengers = sum(passengers), nflights= sum(flights), 
                    capacity=sum(capacity), weight=sum(weight))
      }
      data  
    })
  })
  
  #Summary table
  output$tabSummary <- DT::renderDataTable(DT::datatable(summary_table(), rownames = FALSE))

  #plot
  countryPlot <- reactive({
    input$plot_button

    
    isolate({
      countries<-input$selectCountry
      date_range <- input$plot_slider
      date_year  <- as.numeric(substr( date_range, 1, 4))
      date_month <- as.numeric(substr( date_range, 6, 7))
      df<-flights %>%
        filter(country %in% countries,
                 year > date_year[1] | ( year == date_year[1] & month >= date_month[1] ), 
                 year < date_year[2] | ( year == date_year[1] & month <= date_month[2] )
               ) %>%
        group_by(country) %>%
        summarise(Passengers = sum(passengers), nflights= sum(flights), 
                  Seats=sum(capacity), weight=sum(weight),
                  Incoming = sum(flights[direction=="Incoming"]), 
                  Outgoing = sum(flights[direction=="Outgoing"])) %>%
        arrange( desc(nflights))
      df
    })
  })
##############PLOT###################################################################################  
  output$plot <- renderGvis({
    df<-countryPlot()
    df<-df %>% select(country, Passengers, Seats)
    Bar<-gvisBarChart(df,
                      options=list(
                      title="Number of passengers and seats by country"
                      )
                      )
    Bar
    #plot(Bar)
  })
  
  
  output$plot2 <- renderGvis({
    df<-countryPlot()
    df<-df %>% 
       select(country, Incoming, Outgoing)
      
    #   flights %>%
    #   group_by(country) %>%
    #   summarise(npassengers = sum(passengers), nflights= sum(flights), 
    #             capacity=sum(capacity), weight=sum(weight)) %>%
    #   arrange( desc(nflights)) %>%
    #   head(n=5)
    # # %>%
    #   mutate(filling_rate = npassengers/capacity) %>%
    #   select(country, direction, npassengers)
    # df
    # df<-df %>%
    #   summarise(npassInc = sum(npassengers[direction=="Incoming"]), 
    #             npassOut = sum(npassengers[direction=="Outgoing"])) %>%
    #   select(country, npassengers, capacity) %>%
    #   arrange( desc(npassInc)) %>%
    #   head(n=5)
    # 
    Bar<-gvisBarChart(df,                     
                      options=list(
                      title="Number incoming and outgoing flights by country"
    ))
    Bar
    #plot(Bar)
  })
  
##########################################################################################################"  
  # Create the map
  output$map <- renderLeaflet({
    m<-leaflet() %>%
      addTiles( ) %>%
      addCircles(cities$longitude, cities$latitude, popup=paste(cities$city, cities$country, sep=", "))
    m
  })
  
  dataInBounds <- reactive({
    bounds <- input$map_bounds 
    
    date_range <- input$map_slider
    date_year  <- as.numeric(substr( date_range, 1, 4))
    date_month <- as.numeric(substr( date_range, 6, 7))
    
    data <- left_join( flights, cities, by = c("country", "city") )
    if( is.null(bounds) ){
      data
    } else {
      data %>% 
        filter( 
          xmin > bounds$west, xmax < bounds$east, ymin > bounds$south, ymax < bounds$north,
          year > date_year[1] | ( year == date_year[1] & month >= date_month[1] ), 
          year < date_year[2] | ( year == date_year[1] & month <= date_month[2] )
        ) %>% 
        group_by( country, city, direction ) %>%
        summarise( passengers = sum(passengers), flights = sum(flights), 
                   longitude = first(longitude), 
                   latitude = first(latitude) )
    }
  })
  
  
  # Create the map
  output$map <- renderLeaflet({
    m <- leaflet() %>%
      addTiles( ) %>%
      addCircles(cities$longitude, cities$latitude,
                  popup = paste(cities$city, cities$country, sep=", ") )
    m
  })
  
  observe({
    data <- dataInBounds() %>%
      group_by(country, city)  %>%
      summarise( 
        longitude = first(longitude), 
        latitude = first(latitude), 
        incoming_passengers = sum(passengers[direction == "Incoming"]), 
        outgoing_passengers = sum(passengers[direction == "Outgoing"]), 
        all_passengers      = incoming_passengers + outgoing_passengers,
        
        incoming_flights = sum(flights[direction == "Incoming"]), 
        outgoing_flights = sum(flights[direction == "Outgoing"]), 
        all_flights      = incoming_flights + outgoing_flights
        )
    maxFlights <- max(data$all_flights, na.rm = TRUE)
    data <- data %>%
      mutate( radius = pmax( 4, all_flights / maxFlights * 30 ) )
    
    makePopup <- function(city, country, incoming_passengers, outgoing_passengers, all_passengers, incoming_flights, outgoing_flights, all_flights){
      as.character(tagList( 
        tags$strong(sprintf( "%s (%s)", city, country)), 
        tags$br(), tags$br(), 
        sprintf( "%d passengers (%d / %d)", all_passengers, incoming_passengers, outgoing_passengers ), 
        tags$br(), 
        sprintf( "%d flights (%d / %d)", all_flights, incoming_flights, outgoing_flights ) 
      ))
    }
    
    popups <- data %>% 
      rowwise() %>% 
      mutate( popup = makePopup(city, country, 
                                incoming_passengers, outgoing_passengers, all_passengers,
                                incoming_flights, outgoing_flights, all_flights
                                ) ) %$% popup
    leafletProxy("map") %>%
      clearShapes() %>%
      clearPopups() %>%
      addCircleMarkers(
        data$longitude, data$latitude,
        radius = data$radius, 
        popup = popups, 
        stroke = FALSE
      )
  })
  
  output$map_flights_count <- renderText({
    data <- dataInBounds()
    sprintf( "%d flights", sum(data$flights) )
  })
  output$map_passengers_count <- renderText({
    data <- dataInBounds()
    sprintf( "%d passengers", sum(data$passengers) )
  })
  
  output$map_flights_count_details <- renderText({
    data <- dataInBounds()
    directions <- data %>% 
      group_by( direction ) %>%
      summarise( n = sum(flights) )
    
    sprintf( "%s incoming, %s outgoing", 
             format(directions$n[ directions$direction == "Incoming" ]),
             format(directions$n[ directions$direction == "Outgoing" ])
    )
  })
  output$map_passengers_count_details <- renderText({
    data <- dataInBounds()
    directions <- data %>% 
      group_by( direction ) %>%
      summarise( n = sum(passengers) )
    
    sprintf( "%d incoming, %d outgoing", 
             directions$n[ directions$direction == "Incoming" ],
             directions$n[ directions$direction == "Outgoing" ]
    )
  })
  
  output$map_flights_city_country <- renderText({
    data <- dataInBounds()
    
    cities_in_bounds <- unique( data$city )
    countries_in_bounds <- unique( data$country )
    
    sprintf( "%d cities, %d countries" , 
             length(cities_in_bounds), 
             length(countries_in_bounds)
             )
  })
  
  output$map_seasonnality_plot <- renderPlot({
    data <- flights %>%
      group_by(year, month) %>%
      summarise( passengers = sum(passengers) )
  
    colors <- brewer.pal( length(unique(data$year)), "Accent" )
    
    plot( 0, 0, type = "n", xlim = c(.5,12.5), ylim=extendrange(data$passengers), axes = FALSE, ann = FALSE)
    axis( 1, 1:12, substr(month.abb, 1, 1) )
    axis( 2, axTicks(2), las = 2)
    abline( h = axTicks(side=2), col = "lightgrey")
    years <- unique(data$year)
    for( i in seq_along(years) ){
      d <- filter(data, year == years[i])
      lines( d$month, d$passengers, col = colors[i], lwd = 3 )
    }
    legend("topleft", legend=2007:2012,  col=colors[1:6], lwd=rep(2,6))
    
  }, width = 1200, height = 800 )
  
  map_table <- reactive({
    data <- dataInBounds() %>%
      arrange( desc(passengers) ) %>%
      select(country, city, direction, passengers, flights)
  })
  
  output$map_table <- DT::renderDataTable(DT::datatable(map_table(), rownames = FALSE))
  
})
