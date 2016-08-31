#install.packages("shiny")
library(shiny)

# Define UI for DairyCowPin application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Choix des paramètres de lissage"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("dataset", "Choix du jeu de données:", 
                choices = c("Toutes", "Holstein", "Normande")),

    selectInput("variable", "Choix de la variable :", 
                choices = c("Production laitière", "Taux Butyreux", "Taux protéique", "Poids vif", "Note d'état corporel")),
    
    # Sidebars with a slider input for number of observations
    sliderInput("lambda", "Paramètre de lissage lambda:", 
                 min=0.001,
                 max=100,
                 value=1),

    sliderInput("nbsplines", "Nombre de splines pour la base:", 
                 min=2,
                 max=50,
                 value=20),
    
    numericInput("id", "Numéro de ligne d'un individu:", 1)
    
    #actionButton(inputId="id", "Suivant")
    
    #downloadButton('downloadData', 'Download')

    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("fdaPlot"),
    plotOutput("fdaPlot2")

  )
))