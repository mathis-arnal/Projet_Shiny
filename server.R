shinyServer(function(input, output) {
  
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
  output$rapport <- renderUI({
      url <- a("Rapport", href = "https://stackoverflow.com/questions/62708534/extract-download-link-from-html-in-r-shiny-app")
      tagList(url)
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
  
  # l'objet v permet de maîtriser l'affichage des graphiques
  v <- reactiveValues(doPlot = FALSE)
  
  # doPlot peut prendre la valeur TRUE ou FALSE 
  # lorsque l'on clique sur Afficher les graphiques
  # doPlot prend la valeur TRUE
  observeEvent(input$go, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <- input$go
  })
  
  # si on modifie le paramètre ville, les dates alors doPlot 
  # prend la valeur FALSE, il faut dont re-cliquer pour afficher 
  # les graphiques actualisés
  observeEvent(input$ville, {
    v$doPlot <- FALSE
  })

  observeEvent(input$idDateRange, {
    v$doPlot <- FALSE
  }) 
  
  # Le graphique concernant la qualité de l'air s'affiche automatiquement 
  output$plotAir <- renderPlot({
     if (input$idCheckair == T){
      plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Indice qualité de l'airT", xlab="Évolution dans le temps")
      ##plot Évolution qualité de l'air
    } else {
      plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Indice qualité de l'air", xlab="Évolution dans le temps")
    }
      
  })
  # Affichage du graphique représentant l'évolution des températures
  output$plotRainTemp <- renderDygraph({
    if (v$doPlot == FALSE) {return()
    } else {
    if (input$go==T){# tant que l'utilisateur n'a pas validé ses paramètres aucun graphique ne s'affiche
      date_start <- input$idDateRange[1]#affiche  la date de départ sous la forme [1] "2020-01-01"
      date_end <- input$idDateRange[2]#affiche  la date de départ sous la forme [1] "2023-09-04"
      Temp<-cbind(meteostat_data$tavg,meteostat_data$tmax,meteostat_data$tmin)
      Temp_series <- xts(x = Temp, order.by = meteostat_data$time)
      colnames(Temp_series) <- c("tavg", "tmax", "tmin")
      dygraph(Temp_series, main = "Suivi des  températures")
    } else {
      plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Indice qualité de l'air", xlab="Évolution dans le temps")
    }}
    
    })
  
  # Affichage du graphique représentant l'évolution de la pression
  output$plotPres <- renderDygraph({
    if (input$go==T){
    precipitation<-xts(x=meteostat_data$prcp,order.by = meteostat_data$time)
    colnames(precipitation)<-"precipitation(mm)"
    dygraph(precipitation, main = "Suivi des précipitations")%>%
      dySeries("precipitation(mm)",stepPlot = TRUE, fillGraph = TRUE, color = "blue")
    } else {## mettre un graph par défaut
      plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Indice qualité de l'air", xlab="Évolution dans le temps")
    }
  })

  # Affichage du graphique représentant l'évolution du vent
  output$plotWind <- renderPlot({
    if (input$go==T){
      plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Indice qualité de ventT", xlab="Évolution dans le temps")
    } else {
    plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Indice qualité de l'air", xlab="Évolution dans le temps")
    }
    #mettre lignes de codes pour créer le graphique
    #plot() 
  })
  
  # Création du modèle par l'utilisateur et affichage de la matrice de confusion
  output$modelsumr <- renderPrint({
    
  })
  

  
  })

