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
      paste("Rapport", sep = ".")
    },
    content = function(file) {
      src <- normalizePath("Rmd.Rmd")
    }
  )
  
  #Téléchargement du fichier Rmd 
  output$downloadRmd <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(input$dataset, ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(data(), file)
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

