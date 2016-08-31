#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(googleVis)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        htmlOutput("distPlot")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$distPlot <- renderGvis({
      

   
   ###graph bar incoming/outgoing
   df<-flights %>%
     group_by(country, direction) %>%
     summarise(npassengers = sum(passengers), nflights= sum(flights), capacity=sum(capacity), weight=sum(weight)) %>%
     mutate(filling_rate = npassengers/capacity) %>%
     select(country, direction, npassengers)
   df
   df<-df %>%
     summarise(npassInc = sum(npassengers[direction=="Incoming"]), 
               npassOut = sum(npassengers[direction=="Outgoing"])) %>%
     select(country, npassInc, npassOut) %>%
     arrange( desc(npassInc)) %>%
     head(n=5)
   
   gvisBarChart(df)
   
   
})
   
})

# Run the application 
shinyApp(ui = ui, server = server)

