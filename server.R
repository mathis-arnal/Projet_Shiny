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
  
  # l'objet r permet de maîtriser l'affichage de l'analyse
  r <- reactiveValues(doAnalyse = FALSE)
  
  observeEvent(input$allez, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    r$doAnalyse <- input$allez
  })
  
  observeEvent(input$prcp, {
    r$doAnalyse <- FALSE
  })
  observeEvent(input$wdir, {
    r$doAnalyse <- FALSE
  })
  observeEvent(input$tavg, {
    r$doAnalyse <- FALSE
  })
  observeEvent(input$tmin, {
    r$doAnalyse <- FALSE
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
  output$modele <- renderPrint({
    if (r$doAnalyse == FALSE) {print("...")
    } else {
      
      set.seed(45L)
      dt_qualite_air <- air_quality 
      
      summary(dt_qualite_air[,23:31]) # résumé pour les données meteo, pour verifier les classes des colonnes
      
      # on enlève les NA:
      dt_qualite_air <- dt_qualite_air[which(! is.na(dt_qualite_air$wpgt)),]
      dt_qualite_air <- dt_qualite_air[which(! is.na(dt_qualite_air$prcp)),]
      
      # creation de la colonnes qualite_air_groupe
      dt_qualite_air[, qualite_air_groupe := ifelse(code_qual %in% c(1, 2), "Groupe 1-2",
                                                    ifelse(code_qual %in% c(3, 4), "Groupe 3-4", "Other"))]
      
      dt_qualite_air$qualite_air_groupe<- as.factor(dt_qualite_air$qualite_air_groupe)
      
      ## Convertir les degrés en radians
      dt_qualite_air <- dt_qualite_air %>%
        mutate(Angle_radians = wdir * pi / 180)
      
      # Calculer les composantes x et y de la direction du vent
      dt_qualite_air <- dt_qualite_air %>%
        mutate(Vent_x_est = cos(Angle_radians),
               Vent_y_nord = sin(Angle_radians))
      
      
      # ---------------------------------------------------------------------------#
      # creation du dataframe:
      # on ajoute directement la colonne d'interet de groupe de qualité de l'air:
      datamodele=data.frame(dt_qualite_air$qualite_air_groupe)
      colnames(datamodele)<-"qualite_air_groupe"
      
      # en fonction des variables selectionnées, on crée un nouveau dataframe:
      ## récupération des inputs choisis par l'utilisateur 
      prcp = input$prcp
      wdir = input$wdir
      tmin = input$tmin
      tmax = input$tmax
      wspd = input$wspd
      wpgt = input$wpgt
      pres = input$pres
      tavg = input$tavg
      
      if (prcp == T){ # si l'utilisateur a selectionne precipitation
        datamodele$prcp <-dt_qualite_air$prcp } # on ajoute la col au dataframe
      if (wdir == T){
        datamodele$wdir <-dt_qualite_air$wdir}
      if (tmin == T){
        datamodele$tmin <-dt_qualite_air$tmin}
      if (tmax == T){
        datamodele$tmax <-dt_qualite_air$tmax}
      if (wspd == T){
        datamodele$wspd <-dt_qualite_air$wspd}
      if (wpgt == T){
        datamodele$wpgt <-dt_qualite_air$wpgt}
      if (pres == T){
        datamodele$pres <-dt_qualite_air$pres}
      if (tavg == T){
        datamodele$tavg <-dt_qualite_air$tavg}
      
      ## Creation des data train (80%) et test (20%)
      # séparation en train et en test
      n.train <- round(nrow(datamodele)*0.80,0)
      train_indices <- sample(1:nrow(datamodele), n.train)
      
      # Create the training dataset (data.train)
      datamodele.train <- datamodele[train_indices, ]
      
      # Create the testing dataset (data.test)
      datamodele.test <- datamodele[-train_indices, ]
      
      ### ensuite on constuit le modèle et on le test
      # Model estimations
      fitControl.LGOCV <- trainControl(
        method = "LGOCV",
        number=10,
        p=0.6
      )
      
      mod.glm.LGOCV <- train(
        qualite_air_groupe ~ .,
        data=datamodele.train,
        method="glm",
        trControl = fitControl.LGOCV
      )
      
      pred.glm.grid <- predict(mod.glm.LGOCV,newdata=datamodele.test) # glm prediction
      
      #mod.glm.LGOCV$results
      
      pred.glm <- predict(mod.glm.LGOCV)
      ###############
      # joli rendu:
      conf_matrix <- confusionMatrix(data = pred.glm, reference = datamodele.train$qualite_air_groupe)
      
      # un joli plot
      
      x<-data.frame(conf_matrix$table)
      TClass <- factor(x[,2])
      PClass <- factor(x[,1])
      Y      <- (x[,3])
      
      
      ggplot(data =  x, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
        scale_fill_gradient(low="white", high="#009194") +
        theme_bw() + theme(legend.position = "none")
      
      # petite morale sur l'accuracy, la sensi et la spéci:
      
      #Accuracy:
      if (conf_matrix$overall[1]>0.9){
        accuracytxt="Votre accuracy est très bonne, bravo c'est un modèle qui semble bien prédire la qualité de l'air !"
      }else if (conf_matrix$overall[1]>0.70){
        accuracytxt="Votre accuracy est bonne, votre modèle semble prédire correctement la qualité de l'air !"
      }else if (conf_matrix$overall[1]>0.50){
        accuracytxt="Votre accuracy est moyenne, votre modèle semble ne predire que partiellement la qualité de l'air."
      }else{
        accuracytxt="Votre accuracy est mauvaise, le modèle n'est pas bien adapté pour prédire la qualité de l'air."
      }
      
      #Sensitivité:
      if (conf_matrix$byClass[1]>0.9){
        sensitxt="Votre sensibilité est très bonne, bravo vous predisez très bien les jours de bonne voire très bonne qualité de l'air (le groupe 1-2)"
      }else if (conf_matrix$byClass[1]>0.70){
        sensitxt="Votre sensibilité est bonne, votre modèle semble prédire correctement les jours de bonne voire très bonne qualité de l'air  (le groupe 1-2)"
      }else if (conf_matrix$byClass[1]>0.50){
        sensitxt="Votre sensibilité est moyenne, votre modèle semble ne predire que partiellement les jours de bonne voire très bonne qualité de l'air  (le groupe 1-2)."
      }else{
        sensitxt="Votre sensibilité est mauvaise, le modèle n'est pas bien adapté pour prédire les jours de bonne voire très bonne qualité de l'air  (le groupe 1-2)."
      }
      
      
      # Spécificité:
      if (conf_matrix$byClass[2]>0.9){
        specitxt="Votre spécificité est très bonne, bravo vous predisez très bien les jours où la qualité de l'air est degradé ou mauvaise, donc le groupe 3-4"
      }else if (conf_matrix$byClass[2]>0.70){
        specitxt="Votre spécificité est bonne, votre modèle semble prédire correctement les jours où la qualité de l'air est degradé ou mauvaise, donc le groupe 3-4"
      }else if (conf_matrix$byClass[2]>0.50){
        specitxt="Votre spécificité est moyenne, votre modèle semble ne predire que partiellement les jours où la qualité de l'air est degradé ou mauvaise, donc le groupe 3-4."
      }else{
        specitxt="Votre spécificité est mauvaise, le modèle n'est pas bien adapté pour prédire les jours où la qualité de l'air est degradé ou mauvaise, donc le groupe 3-4."
      }
      
      
      
      phrase=paste("L'accuracy est de",round(conf_matrix$overall[1],3),", la sensibilité est de", round(conf_matrix$byClass[1],3),
                   ", et la spécificité est de", round(conf_matrix$byClass[2],3))
      print(phrase)
      
      fulltxtacc<- (paste("Votre accuracy = ",round(conf_matrix$overall[1],3),".",accuracytxt))
      fulltxtsensi<-(paste("Votre sensibilité (capacité à donner un résultat positif lorsqu'une hypothèse est vérifiée) =",
                           round(conf_matrix$byClass[1],3),".",sensitxt))
      fulltxtspeci<-(paste("Votre spécificité (capacité à donner un résultat négatif lorsqu'une hypothèse est vérifiée) =",
                           round(conf_matrix$byClass[2],3),".",specitxt))
      print(fulltxtacc)
      print(fulltxtsensi)
      print(fulltxtspeci)
    }
  })
  
  output$CM <- renderPlot({
    ###############
    # joli rendu:
    conf_matrix <- confusionMatrix(data = pred.glm, reference = datamodele.train$qualite_air_groupe)
    
    # un joli plot
    
    x<-data.frame(conf_matrix$table)
    TClass <- factor(x[,2])
    PClass <- factor(x[,1])
    Y      <- (x[,3])
    
    
    print(ggplot(data =  x, mapping = aes(x = TClass, y = PClass)) +
            geom_tile(aes(fill = Y), colour = "white") +
            geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
            scale_fill_gradient(low="white", high="#009194") +
            theme_bw() + theme(legend.position = "none"))
    
  })
  
  # Création du modèle par l'utilisateur et affichage de la matrice de confusion
  output$CM <- renderPlot({
    if (r$doAnalyse == FALSE) {print("...")
    } else {
      
      set.seed(45L)
      dt_qualite_air <- air_quality 
      
      summary(dt_qualite_air[,23:31]) # résumé pour les données meteo, pour verifier les classes des colonnes
      
      # on enlève les NA:
      dt_qualite_air <- dt_qualite_air[which(! is.na(dt_qualite_air$wpgt)),]
      dt_qualite_air <- dt_qualite_air[which(! is.na(dt_qualite_air$prcp)),]
      
      # creation de la colonnes qualite_air_groupe
      dt_qualite_air[, qualite_air_groupe := ifelse(code_qual %in% c(1, 2), "Groupe 1-2",
                                                    ifelse(code_qual %in% c(3, 4), "Groupe 3-4", "Other"))]
      
      dt_qualite_air$qualite_air_groupe<- as.factor(dt_qualite_air$qualite_air_groupe)
      
      ## Convertir les degrés en radians
      dt_qualite_air <- dt_qualite_air %>%
        mutate(Angle_radians = wdir * pi / 180)
      
      # Calculer les composantes x et y de la direction du vent
      dt_qualite_air <- dt_qualite_air %>%
        mutate(Vent_x_est = cos(Angle_radians),
               Vent_y_nord = sin(Angle_radians))
      
      
      # ---------------------------------------------------------------------------#
      # creation du dataframe:
      # on ajoute directement la colonne d'interet de groupe de qualité de l'air:
      datamodele=data.frame(dt_qualite_air$qualite_air_groupe)
      colnames(datamodele)<-"qualite_air_groupe"
      
      # en fonction des variables selectionnées, on crée un nouveau dataframe:
      ## récupération des inputs choisis par l'utilisateur 
      prcp = input$prcp
      wdir = input$wdir
      tmin = input$tmin
      tmax = input$tmax
      wspd = input$wspd
      wpgt = input$wpgt
      pres = input$pres
      tavg = input$tavg
      
      if (prcp == T){ # si l'utilisateur a selectionne precipitation
        datamodele$prcp <-dt_qualite_air$prcp } # on ajoute la col au dataframe
      if (wdir == T){
        datamodele$wdir <-dt_qualite_air$wdir}
      if (tmin == T){
        datamodele$tmin <-dt_qualite_air$tmin}
      if (tmax == T){
        datamodele$tmax <-dt_qualite_air$tmax}
      if (wspd == T){
        datamodele$wspd <-dt_qualite_air$wspd}
      if (wpgt == T){
        datamodele$wpgt <-dt_qualite_air$wpgt}
      if (pres == T){
        datamodele$pres <-dt_qualite_air$pres}
      if (tavg == T){
        datamodele$tavg <-dt_qualite_air$tavg}
      
      ## Creation des data train (80%) et test (20%)
      # séparation en train et en test
      n.train <- round(nrow(datamodele)*0.80,0)
      train_indices <- sample(1:nrow(datamodele), n.train)
      
      # Create the training dataset (data.train)
      datamodele.train <- datamodele[train_indices, ]
      
      # Create the testing dataset (data.test)
      datamodele.test <- datamodele[-train_indices, ]
      
      ### ensuite on constuit le modèle et on le test
      # Model estimations
      fitControl.LGOCV <- trainControl(
        method = "LGOCV",
        number=10,
        p=0.6
      )
      
      mod.glm.LGOCV <- train(
        qualite_air_groupe ~ .,
        data=datamodele.train,
        method="glm",
        trControl = fitControl.LGOCV
      )
      
      pred.glm.grid <- predict(mod.glm.LGOCV,newdata=datamodele.test) # glm prediction
      
      #mod.glm.LGOCV$results
      
      pred.glm <- predict(mod.glm.LGOCV)
      ###############
      # joli rendu:
      conf_matrix <- confusionMatrix(data = pred.glm, reference = datamodele.train$qualite_air_groupe)
      
      # un joli plot
      
      x<-data.frame(conf_matrix$table)
      TClass <- factor(x[,2])
      PClass <- factor(x[,1])
      Y      <- (x[,3])
      
      
      ggplot(data =  x, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
        scale_fill_gradient(low="white", high="#009194") +
        theme_bw() + theme(legend.position = "none")
      
    }
  })
  
  
  
  
  
})
