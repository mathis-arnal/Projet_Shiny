library(shiny)
library(fresh)
library(data.table)
library(rmarkdown)
library(tufte)
library(fontawesome)
library(ggmap)
library(leaflet)
library(dygraphs)
library(xts)



shinyServer(function(input, output) {
  
  #Nos jeux de données 
  meteostat_data <- fread("data/meteostat_data.csv", header=TRUE)
  meteostat_data$time <- as.Date(meteostat_data$time, format="YYYY-MM-DD")
  air_quality <- fread("data/quality_index_rennes.csv")
  
  #Lien indice ATMO
  output$lien <- renderUI({
    url <- a("Indice ATMO", href="https://www.atmo-france.org/article/lindice-atmo")
    tagList(url)})
  

  #Permet de sélectionner les données que l'utilisateur veut télécharger
  datasetInput <- reactive({
    switch(input$dataset,
           "Données météo" = meteostat_data,##données météo
           "Données indice qualité de l'air" = air_quality,##données qualité de l'air
           )
  })
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = ",")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(datasetInput(), file)
    }
  )
  
  #Téléchargement du fichier Rmd ne fonctionne pas
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
    rmarkdown::render( "Qualite_air.Rmd", 
                      output_file = file,
                      envir = new.env(parent = globalenv()))
    })
  
  # Affichage de la carte
  output$carte <- renderLeaflet({
        m <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng = -1.6788, lat = 48.1123, popup = "Rennes") %>%
        addMarkers(lng = -4.4886, lat = 48.3917, popup = "Brest") %>%
        addMarkers(lng = -4.1035, lat = 47.9959, popup = "Quimper") %>%
        addMarkers(lng = -2.7617, lat = 48.5144, popup = "Saint- Brieuc") %>%
        addMarkers(lng = -2.7576, lat = 47.6572, popup = "Saint- Brieuc") 
        m
  })
  
  # Input$idDateRange
  # Input$idCheckair
  # Affichage du graphique représentant l'évolution des températures
  output$plotRainTemp <- renderDygraph({
    Temp<-cbind(meteostat_data$tavg,meteostat_data$tmax,meteostat_data$tmin)
    Temp_series <- xts(x = Temp, order.by = meteostat_data$time)
    colnames(Temp_series) <- c("tavg", "tmax", "tmin")
    dygraph(Temp_series, main = "Suivi des  températures")
    })
  
  # Affichage du graphique représentant l'évolution de la pression
  output$plotPres <- renderDygraph({
    precipitation<-xts(x=meteostat_data$prcp,order.by = meteostat_data$time)
    colnames(precipitation)<-"precipitation(mm)"
    dygraph(precipitation, main = "Suivi des précipitations")%>%
      dySeries("precipitation(mm)",stepPlot = TRUE, fillGraph = TRUE, color = "blue")
  })
?dygraph
  # Affichage du graphique représentant l'évolution du vent
  output$plotWind <- renderPlot({
    #mettre lignes de codes pour créer le graphique
    #plot() 
  })
  
  })

