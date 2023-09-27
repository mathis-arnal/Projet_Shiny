library(shiny)
library(fresh)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #Permet de sélectionner les données que l'utilisateur veut télécharger
  datasetInput <- reactive({
    switch(input$dataset,
           "Données météo" = météo,##données météo
           "Données indice qualité de l'air" = quality,##données qualité de l'air
           )
  })
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  # Affichage de la carte
  output$map <- renderPlot({## si fonction plot
    #mettre lignes de codes pour créer le graphique
    #plot() 
  })
  
  # Input pour tracer les graphiques 
  
  # Input$searchTown #sélection de la ville
  # Input$idDateRange
  # Input$idCheckair
  # Affichage du graphique représentant l'évolution de la pluie et de la température
  output$plotRainTemp <- renderPlot({
    #mettre lignes de codes pour créer le graphique
    #plot() 
    })
  
  # Affichage du graphique représentant l'évolution de la pression
  output$plotPres <- renderPlot({
    #mettre lignes de codes pour créer le graphique
    #plot() 
  })
  
  # Affichage du graphique représentant l'évolution du vent
  output$plotWind <- renderPlot({
    #mettre lignes de codes pour créer le graphique
    #plot() 
  })
  
  })

