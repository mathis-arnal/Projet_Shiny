shinyServer(function(input, output) {
  
  ## MISE A DISPOSITION DES RESSOURCES : LIEN ET TELECHARGEMENT DONNEES
  ##################################################################################
  #Lien indice ATMO
  output$lien <- renderUI({
    url <- a("Indice ATMO", href="https://www.atmo-france.org/article/lindice-atmo")
    tagList(url)})
  
  
  #Permet de sélectionner les données que l'utilisateur veut télécharger
  datasetInput <- reactive({
    switch(input$dataset,
           "Données brutes" = all_raw_data,##données brutes
    )
  })
  # Téléchargement dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = ",")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(datasetInput(), file)
    }
  )

  ## CARTE INTERACTIVE
  ##################################################################################
  output$map <- renderLeaflet({## creation de la carte de base 
    ### creations de la map
      m<- leaflet() %>%
          addTiles()%>%# Ajoute noms par défaut de la carte
        addMarkers(lng = -1.6788, lat = 48.1123, popup = "Rennes") %>%
        addMarkers(lng = -4.4886, lat = 48.3917, popup = "Brest") %>%
        addMarkers(lng = -4.1035, lat = 47.9959, popup = "Quimper") %>%
        addMarkers(lng = -2.7617, lat = 48.5144, popup = "Saint- Brieuc") %>%
        addMarkers(lng = -2.7576, lat = 47.6572, popup = "Saint- Brieuc") 
  })
  
  observe ({# utilisation de observe pour modifier la carte lorsque la date change 
    date <- input$Date
    cond <- input$map
    
    ######## WIND DIRECTION
    
    # recuperation de l'orientation du vent pour chaque ville pour une date donnée:
    # attention l'ordre doit etre toujours le meme:
    # rennes, brest, quimper, saint-brieuc, vannes
  

    if (cond == "Vent"){
        # recuperation
      VectwindDir= as.vector(all_raw_data[which(all_raw_data$time==date),grep("wdir", colnames(all_raw_data))])
      # VectwindDir<-as.vector(VectwindDir$wdir)
      
      # a partir de la direction du vent en degre, on va approximer la direction
      # a un des huit point carnidaux le plus proche. Si l'angle est de 5 on approxime
      # a 0 et on dit que la direction générale est Nord

      # creation d'une fonction qui va recuperer les coordonnees les plus proches
      get_wind_direction <- function(x){
        if (x >=337.5 ||x < 22.5){
          winddir="0N"
        } else if (x >=22.5 && x < 67.5){
          winddir="45NE"
        }else if (x >=67.5 && x<112.5){
          winddir="90E"
        }else if (x >=112.5 && x<157.5){
          winddir="135SE"
        }else if (x >=157.5 && x<202.5){
          winddir="180S"
        }else if (x >=202.5 && x<247.5){
          winddir="225SO"
        }else if (x >=247.5 && x<292.5){
          winddir="270O"
        }else if (x >=292.5 && x<337.5){
          winddir="315NO"
        }else if (is.na(x)){
          winddir="NA"
        }
        return(winddir)}

      # on applique la fonction a notre vecteur
      winddirall<- paste("WindIcon", sapply(VectwindDir, get_wind_direction), sep="")

      # on recupere l'icon correspondante pour chacune des villes
      WindRennes <- makeIcon(
        iconUrl = paste("Icons/", winddirall[1],".PNG",sep=""),
        iconWidth = 50, iconHeight = 60
      )
      WindBrest <- makeIcon(
        iconUrl = paste("Icons/",winddirall[2],".PNG",sep=""),
        iconWidth = 50, iconHeight = 60
      )
      WindQuimper <- makeIcon(
        iconUrl = paste("Icons/",winddirall[3],".PNG",sep=""),
        iconWidth = 50, iconHeight = 60
      )
      WindSaintBrieuc <- makeIcon(
        iconUrl = paste("Icons/",winddirall[4],".PNG",sep=""),
        iconWidth = 50, iconHeight = 60
      )
      WindVannes <- makeIcon(
        iconUrl = paste("Icons/",winddirall[5],".PNG",sep=""),
        iconWidth = 50, iconHeight = 60
      )


      ### creation de la map
      leafletProxy("map") %>% # utilisation de leafletProxy pour ne pas regénérer toute la carte
        clearMarkers() %>%
        addMarkers(lng = -1.6788, lat = 48.1123,
                   icon = WindRennes,
                   popup = paste( "Rennes: ",VectwindDir[1], "°")) %>%

        addMarkers(lng = -4.4886, lat = 48.3917,
                   icon = WindBrest,
                   popup = paste("Brest: ",VectwindDir[2], "°")) %>%

        addMarkers(lng = -4.1035, lat = 47.9959,
                   icon = WindQuimper,
                   popup = paste("Quimper: ",VectwindDir[3], "°")) %>%

        addMarkers(lng = -2.7617, lat = 48.5144,
                   icon = WindSaintBrieuc,
                   popup = paste("Saint-Brieuc: ",VectwindDir[4], "°")) %>%

        addMarkers(lng = -2.7576, lat = 47.6572,
                   icon = WindVannes,
                   popup = paste("Vannes: ",VectwindDir[[5]], "°"))
    }
    
    ######## RAIN
    else if(cond == "Pluie"){
      Vectprcp= as.vector(all_raw_data[which(all_raw_data$time==date),grep("prcp", colnames(all_raw_data))])
      # Vectprcp<-as.vector(Vectprcp$prcp)
      # creation d'une fonction classe la quantité de precipitation
      get_prcp <- function(x){
        if (is.na(x)){
          prcp="NA"
        } else if (x >0 && x < 1.3){
          prcp="peu"
        }else if (x >=1.3 && x<25.4){
          prcp="moyen"
        }else if (x >=25.4 && x<54){
          prcp="bcp"
        }else if (x ==0){
          prcp="pas"
        }
        return (prcp)
      }

      # on applique la fonction a notre vecteur
      prcpall<- paste(sapply(Vectprcp, get_prcp),"prcpIcon", sep="")

      # on recupere les incones pour chacune des villes
      prcpRennesIcon <- makeIcon(
        iconUrl = paste("Icons/",prcpall[1],".PNG",sep=""),
        iconWidth = 110, iconHeight = 100
      )
      prcpBrestIcon <- makeIcon(
        iconUrl = paste("Icons/",prcpall[2],".PNG",sep=""),
        iconWidth = 110, iconHeight = 100
      )
      prcpQuimperIcon <- makeIcon(
        iconUrl = paste("Icons/",prcpall[3],".PNG",sep=""),
        iconWidth = 110, iconHeight = 100
      )
      prcpSaintBrieucIcon <- makeIcon(
        iconUrl =  paste("Icons/",prcpall[4],".PNG",sep=""),
        iconWidth = 110, iconHeight = 100
      )
      prcpVannesIcon <- makeIcon(
        iconUrl =  paste("Icons/",prcpall[5],".PNG",sep=""),
        iconWidth = 110, iconHeight = 100
      )

      ### creation de la map
      leafletProxy("map") %>% # utilisation de leafletProxy pour ne pas regénérer toute la carte
        clearMarkers() %>%
        addMarkers(lng = -1.6788, lat = 48.1123,
                   icon = prcpRennesIcon,
                   popup = paste("Rennes: ",Vectprcp[1], "mm")) %>%

        addMarkers(lng = -4.4886, lat = 48.3917,
                   icon = prcpBrestIcon,
                   popup = paste("Brest: ",Vectprcp[2], "mm")) %>%

        addMarkers(lng = -4.1035, lat = 47.9959,
                   icon = prcpQuimperIcon,
                   popup = paste("Quimper: ",Vectprcp[3], "mm")) %>%

        addMarkers(lng = -2.7617, lat = 48.5144,
                   icon = prcpSaintBrieucIcon,
                   popup = paste("Saint-Brieuc: ",Vectprcp[4], "mm")) %>%

        addMarkers(lng = -2.7576, lat = 47.6572,
                   icon = prcpVannesIcon,
                   popup = paste("Vannes: ",Vectprcp[5], "mm"))
    }
    
    ######## INDICE ATMO
     else if (cond == "ATMO"){
      VectQualair= as.vector(all_raw_data[which(all_raw_data$time==date),grep("code_qual", colnames(all_raw_data))])
      #VectQualair<-as.vector(VectQualair$code_qual)

      # creation d'une fonction classe la qualité de l'air
      get_qualair <- function(x){
        if (is.na(x)){
          qualair="NA"}
        if (x==1){
          qualair="Bon"}
        if (x==2){
          qualair="Moyen"}
        if (x==3){
          qualair="Degrade"}
        if (x==4){
          qualair="Mauvais"}
        if (x==4){
          qualair="Tresmauvais"}
        return (qualair)}

      # on applique la fonction a notre vecteur
      qualairall<- paste(sapply(VectQualair, get_qualair),"_qualairIcon", sep="")

      qualairRennesIcon <- makeIcon(
        iconUrl = paste("Icons/",qualairall[1],".jpg",sep=""),
        iconWidth = 60, iconHeight = 40)

      qualairBrestIcon <- makeIcon(
        iconUrl = paste("Icons/",qualairall[2],".jpg",sep=""),
        iconWidth = 60, iconHeight = 40)

      qualairQuimperIcon <- makeIcon(
        iconUrl = paste("Icons/",qualairall[3],".jpg",sep=""),
        iconWidth = 60, iconHeight = 40)

      qualairSaintBrieucIcon <- makeIcon(
        iconUrl =  paste("Icons/",qualairall[4],".jpg",sep=""),
        iconWidth = 60, iconHeight = 40)

      qualairVannesIcon <- makeIcon(
        iconUrl =  paste("Icons/",qualairall[5],".jpg",sep=""),
        iconWidth = 60, iconHeight = 40)

      ### creation de la map
      leafletProxy("map") %>% # utilisation de leafletProxy pour ne pas regénérer toute la carte
        clearMarkers() %>%
        addMarkers(lng = -1.6788, lat = 48.1123,
                   icon = qualairRennesIcon,
                   popup = paste("Rennes: ",VectQualair[1])) %>%

        addMarkers(lng = -4.4886, lat = 48.3917,
                   icon = qualairBrestIcon,
                   popup = paste("Brest: ",VectQualair[2])) %>%

        addMarkers(lng = -4.1035, lat = 47.9959,
                   icon = qualairQuimperIcon,
                   popup = paste("Quimper: ",VectQualair[3])) %>%

        addMarkers(lng = -2.7617, lat = 48.5144,
                   icon = qualairSaintBrieucIcon,
                   popup = paste("Saint-Brieuc: ",VectQualair[4])) %>%

        addMarkers(lng = -2.7576, lat = 47.6572,
                   icon = qualairVannesIcon,
                   popup = paste("Vannes: ",VectQualair[5], "mm"))
     }
  })

  ## DONNEES METEO
  ##################################################################################
  
  # l'objet v permet de maîtriser l'affichage des graphiques
  v <- reactiveValues(doPlot = FALSE)
  
  # doPlot peut prendre la valeur TRUE ou FALSE 
  # lorsque l'on clique sur Afficher les graphiques
  # doPlot prend la valeur TRUE
  observeEvent(input$go, {
    # 0 = FALSE
    # 1 = TRUE
    v$doPlot <- input$go
  })
  
  # si on modifie le paramètre ville et les dates alors doPlot 
  # prend la valeur FALSE, il faut dont re-cliquer pour afficher 
  # les graphiques actualisés
  observeEvent(input$ville, {
    v$doPlot <- FALSE
  })

  observeEvent(input$idDateRange, {
    v$doPlot <- FALSE
  }) 
  
  # Affichage du graphique représentant l'évolution des températures
  # utilisation de renderDygraph qui permet d'afficher les graphiques 
  # fait avec le package dygraph
  output$plotTemp <- renderDygraph({
    if (v$doPlot == FALSE) {return(NULL)
    } else {
      raw_data_spec <- raw_data[Localisation == input$ville]
      Temp<-cbind(raw_data_spec$tavg,raw_data_spec$tmax,raw_data_spec$tmin)
      Temp_series <- xts(x = Temp, order.by = time_range)
      
      colnames(Temp_series) <- c("tavg", "tmax", "tmin")
      
      dygraph(Temp_series, main = "Temperature (°C)",
              ylab = "Température") %>%
        dySeries(c("tmin", "tavg", "tmax"), label = "Temp (°C)") %>%
        dyRangeSelector(input$idDateRange) %>%
        dyOptions(axisLabelFontSize = 12) %>%
        dyLegend(show = "follow") %>%
        dyCrosshair(direction = "vertical")
    }
  })

  
  # Affichage du graphique représentant l'évolution des précipitations
  output$plotPres <- renderDygraph({
    if (v$doPlot == FALSE) {return(NULL)
    } else {
      raw_data_spec <- raw_data[Localisation == input$ville]
      precipitation<-xts(x=raw_data_spec$prcp,order.by =time_range)
      colnames(precipitation)<-"precipitation(mm)"
      dygraph(precipitation, main = "Suivi des précipitations (mm)")%>%
        dyRangeSelector(input$idDateRange) %>%
        dyOptions(axisLabelFontSize = 12) %>%
        dyLegend(show = "follow") %>%
        dyCrosshair(direction = "vertical") %>%
        dySeries("precipitation(mm)",stepPlot = TRUE, fillGraph = TRUE, color = "blue")
    }
  })

  # Affichage du graphique représentant l'évolution du vent
  output$plotWind <- renderDygraph({
    if (v$doPlot == FALSE) {return(NULL)
    } else {
      raw_data_spec <- raw_data[Localisation == input$ville]
      Wind<-cbind(raw_data_spec$wspd,raw_data_spec$wpgt)
      wind_series <- xts(x = Wind, order.by = time_range)
      colnames(wind_series) <- c("wspd","wpgt")
      dygraph(wind_series, main = "Vitesse du Vent (km/h)",
              ylab = "km/h") %>%
        dySeries("wspd", label = "Wind Speed") %>%
        dySeries("wpgt", label = "Pic de Rafale") %>%
        dyRangeSelector(input$idDateRange) %>%
        dyLegend(show = "follow")
    }
  })
  
  # Affichage du grahique représentant la qualité de l'air
  output$plotAir <- renderDygraph({
    if (v$doPlot == FALSE) {
      return(NULL)  # Do not render unless the "Afficher les graphiques" button is clicked
    }
    
    raw_data_spec <- raw_data[Localisation == input$ville]
    
    air_series <- xts(x = raw_data_spec$code_qual, order.by = time_range)
    colnames(air_series) <- "Code_Qualite"
    
    dygraph(air_series, main = "Qualité de l'air (de 1 à 4)") %>%
      dyRangeSelector(input$idDateRange) %>%
      dySeries("Code_Qualite", stepPlot = TRUE,color = "green") %>%
      dyLegend(show = "follow")
  })
  
  

# Observe Event pour les comparaisons 
c <- reactiveValues(doComp = FALSE)
observeEvent(input$go_comp, {
  # 0 will be coerced to FALSE
  # 1+ will be coerced to TRUE
  c$doComp <- input$go_comp
  print(input$go_comp) # Add this line for debugging
})

observeEvent(input$ville_comp, {
  c$doComp <- FALSE
  #print(input$ville_comp)
  #print(list(input$ville_comp))
})

observeEvent(input$idDateRange_comp, {
  c$doComp <- FALSE
  print(input$idDateRange_comp)
})

output$compPres <- renderDygraph({
  if (!c$doComp) {
    return(NULL)
  } else {
    
    selected_ville <- as.list(input$ville_comp)
    
    # Use sapply to extract precipitation data for each town in selected_ville
    prec_data_list <- sapply(selected_ville, function(ville) {
      raw_data[raw_data$Localisation == ville, "prcp"]
    }, simplify = FALSE)
    
    # Combine the precipitation data into a data frame
    prec_data <- do.call(cbind, prec_data_list)
    
    
    prec_ts <- xts(x = prec_data, order.by = time_range)
    colnames(prec_ts) <- selected_ville
    
    
    dy <- dygraph(prec_ts, main = "Comparaison_Precipitation_Ville")
    dy <- dy %>% dyRangeSelector(input$idDateRange)
    
    # Add dySeries for each town in selected_ville
    for (ville in selected_ville) {
      dy <- dy %>% dySeries(ville, label= ville)
    }
    
    dy
    
  }
})


output$compTemp <- renderDygraph({
  if (!c$doComp) {
    return(NULL)
  } else {
    
    selected_ville <- as.list(input$ville_comp)
    
    # Use sapply to extract precipitation data for each town in selected_ville
    prec_data_list <- sapply(selected_ville, function(ville) {
      raw_data[raw_data$Localisation == ville, "tavg"]
    }, simplify = FALSE)
    
    # Combine the precipitation data into a data frame
    prec_data <- do.call(cbind, prec_data_list)
    
    
    prec_ts <- xts(x = prec_data, order.by = time_range)
    colnames(prec_ts) <- selected_ville
    
    
    dy <- dygraph(prec_ts, main = "Comparaison_Temperature_Ville")
    dy <- dy %>% dyRangeSelector(input$idDateRange)
    
    # Add dySeries for each town in selected_ville
    for (ville in selected_ville) {
      dy <- dy %>% dySeries(ville, label= ville)
    }
    
    dy
    
  }
})


output$compWind <- renderDygraph({
  if (!c$doComp) {
    return(NULL)
  } else {
    
    selected_ville <- as.list(input$ville_comp)
    
    # Use sapply to extract precipitation data for each town in selected_ville
    prec_data_list <- sapply(selected_ville, function(ville) {
      raw_data[raw_data$Localisation == ville, "wspd"]
    }, simplify = FALSE)
    
    # Combine the precipitation data into a data frame
    prec_data <- do.call(cbind, prec_data_list)
    
    
    prec_ts <- xts(x = prec_data, order.by = time_range)
    colnames(prec_ts) <- selected_ville
    
    
    dy <- dygraph(prec_ts, main = "Comparaison_VitesseDuVent_Ville")
    dy <- dy %>% dyRangeSelector(input$idDateRange)
    
    # Add dySeries for each town in selected_ville
    for (ville in selected_ville) {
      dy <- dy %>% dySeries(ville, label= ville)
    }
    
    dy
    
  }
})

output$compAir <- renderDygraph({
  if (!c$doComp) {
    return(NULL)
  } else {
    
    selected_ville <- as.list(input$ville_comp)
    
    # Use sapply to extract precipitation data for each town in selected_ville
    prec_data_list <- sapply(selected_ville, function(ville) {
      raw_data[raw_data$Localisation == ville, "code_qual"]
    }, simplify = FALSE)
    
    # Combine the precipitation data into a data frame
    prec_data <- do.call(cbind, prec_data_list)
    
    
    prec_ts <- xts(x = prec_data, order.by = time_range)
    colnames(prec_ts) <- selected_ville
    
    
    dy <- dygraph(prec_ts, main = "Comparaison_QualitéAir_Ville")
    dy <- dy %>% dyRangeSelector(input$idDateRange)
    
    # Add dySeries for each town in selected_ville
    for (ville in selected_ville) {
      dy <- dy %>% dySeries(ville, label= ville)
    }
    
    dy
    
  }
})


  ## ANALYSE INDICE ATMO 
  ##################################################################################
  
  # l'objet r permet de maîtriser l'affichage de l'analyse
  r <- reactiveValues(doAnalyse = FALSE)
  
  observeEvent(input$allez, {
    r$doAnalyse <- input$allez
  })
  
  # dès que l'utilisateur va modifier les paramètres sélectionnés alors
  # r va prendre la valeur FALSE, l'utilisateur devra donc re-cliquer sur 
  # le bouton pour relancer l'analyse
  
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
  observeEvent(input$tmax, {
    r$doAnalyse <- FALSE
  })
  observeEvent(input$wspd, {
    r$doAnalyse <- FALSE
  })
  observeEvent(input$pres, {
    r$doAnalyse <- FALSE
  })
  observeEvent(input$wpgt, {
    r$doAnalyse <- FALSE
  })
  observeEvent(input$lag1, {
    r$doAnalyse <- FALSE
  })
  observeEvent(input$lag2, {
    r$doAnalyse <- FALSE
  })
  observeEvent(input$lag3, {
    r$doAnalyse <- FALSE
  })
  
  # Création du modèle par l'utilisateur et affichage de la matrice de confusion
  output$text <- renderPrint({
    if (r$doAnalyse == FALSE) {print("...")## tant que l'utilisateur n'a pas validé son modèle rien ne s'affiche
    } else {
    
    set.seed(45L)
    dt_qualite_air <- air_quality ## recupération du jeu de données
    
    summary(dt_qualite_air[,23:31]) # résumé pour les données meteo, pour verifier les classes des colonnes
    
    # on enlève les NA:
    dt_qualite_air <- dt_qualite_air[which(! is.na(dt_qualite_air$wpgt)),]
    dt_qualite_air <- dt_qualite_air[which(! is.na(dt_qualite_air$prcp)),]
    
    # creation de la colonnes qualite_air_groupe
    dt_qualite_air[, qualite_air_groupe := ifelse(code_qual %in% c(1, 2), "Groupe_1-2",
                                                  ifelse(code_qual %in% c(3, 4), "Groupe_3-4", "Other"))]
    
    dt_qualite_air$qualite_air_groupe<- as.factor(dt_qualite_air$qualite_air_groupe)
    
    ## Convertir les degrés en radians
    dt_qualite_air <- dt_qualite_air %>%
      mutate(Angle_radians = wdir * pi / 180)
    
    # Calculer les composantes x et y de la direction du vent
    dt_qualite_air <- dt_qualite_air %>%
      mutate(Vent_x_est = cos(Angle_radians),
             Vent_y_nord = sin(Angle_radians))
    
    # creation du dataframe
    # on ajoute directement la colonne d'interet de groupe de qualité de l'air:
    datamodele=data.frame(dt_qualite_air$qualite_air_groupe)
    colnames(datamodele)<-"qualite_air_groupe"
    
    # en fonction des variables selectionnées, on crée un nouveau dataframe
    # récupération des inputs choisis par l'utilisateur 
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
    
    # Creation des data train (80%) et test (20%)
    # séparation en train et en test
    n.train <- round(nrow(datamodele)*0.80,0)
    train_indices <- sample(1:nrow(datamodele), n.train)
    
    # Creation du data.train
    datamodele.train <- datamodele[train_indices, ]
    
    # Creation du data.test 
    datamodele.test <- datamodele[-train_indices, ]
    
    # Define the control parameters for cross-validation
    ctrl <- trainControl(
      method = "cv",             # Cross-validation method
      number = 10,               # Number of folds
      summaryFunction = twoClassSummary,  # For binary classification
      classProbs = TRUE          # For obtaining class probabilities
    )
    
    datamodele.train$qualite_air_groupe <- make.names(datamodele.train$qualite_air_groupe)
    
    # Fit the logistic regression model with cross-validation on the training data
    mod.glm_cv <- train(
      qualite_air_groupe ~ .,   # Formula for your model
      data = datamodele.train,        # Training data
      method = "glm",           # Use glm for logistic regression
      family = "binomial",      # Binomial family for logistic regression
      trControl = ctrl          # Use the control parameters defined earlier
    )
    
    # On voit la prediction sur les donnees train 
    
    scores.glm_test <- predict(mod.glm_cv, newdata = datamodele.train, type = "prob")
    positive_class_probs <- scores.glm_test[, 1]
    
    # Create an ROC object using the positive class probabilities
    roc_obj <- roc(datamodele.train$qualite_air_groupe, positive_class_probs)
    
    # Plot the ROC curve for the train dataset
    plot(roc_obj)
    
    # Get the coordinates of the ROC curve
    optimal_cutoff <- coords(roc_obj, "best")$threshold
    
    # Classify using the optimal cutoff
    pred_CV <- ifelse(positive_class_probs > optimal_cutoff, "Groupe_1.2", "Groupe_3.4")
    pred_CV <- as.factor(pred_CV)
    
    # Evaluate the model on the test dataset
    accuracy <- mean(pred_CV == datamodele.train$qualite_air_groupe)
    datamodele.train$qualite_air_groupe <- as.factor(datamodele.train$qualite_air_groupe)
    
    cat("Accuracy on Test Set:", accuracy, "\n")
    
    conf_matrix <- confusionMatrix(data = pred_CV,
                                   reference = datamodele.train$qualite_air_groupe)
    conf_matrix
    
    
    # un joli plot
    
    x<-data.frame(conf_matrix$table)
    TClass <- factor(x[,2])
    PClass <- factor(x[,1])
    Y      <- (x[,3])
    
    ##
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
    
    fulltxtacc<- (paste("Votre accuracy = ",round(conf_matrix$overall[1],3),".", accuracytxt))
    fulltxtsensi<-(paste("Votre sensibilité (capacité à donner un résultat positif lorsqu'une hypothèse est vérifiée) =",
                         round(conf_matrix$byClass[1],3),".",sensitxt))
    fulltxtspeci<-(paste("Votre spécificité (capacité à donner un résultat négatif lorsqu'une hypothèse est vérifiée) =",
                         round(conf_matrix$byClass[2],3),".",specitxt))
    
      print(phrase)
      print(fulltxtacc)
      print(fulltxtsensi)
      print(fulltxtspeci)
      }
    })

  output$CM <- renderPlot({
  if (r$doAnalyse == FALSE) {print("...")## tant que l'utilisateur n'a pas validé son modèle rien ne s'affiche
  } else {
    
    set.seed(45L)
    dt_qualite_air <- air_quality ## recupération du jeu de données
    
    summary(dt_qualite_air[,23:31]) # résumé pour les données meteo, pour verifier les classes des colonnes
    
    # on enlève les NA:
    dt_qualite_air <- dt_qualite_air[which(! is.na(dt_qualite_air$wpgt)),]
    dt_qualite_air <- dt_qualite_air[which(! is.na(dt_qualite_air$prcp)),]
    
    # creation de la colonnes qualite_air_groupe
    dt_qualite_air[, qualite_air_groupe := ifelse(code_qual %in% c(1, 2), "Groupe_1-2",
                                                  ifelse(code_qual %in% c(3, 4), "Groupe_3-4", "Other"))]
    
    dt_qualite_air$qualite_air_groupe<- as.factor(dt_qualite_air$qualite_air_groupe)
    
    ## Convertir les degrés en radians
    dt_qualite_air <- dt_qualite_air %>%
      mutate(Angle_radians = wdir * pi / 180)
    
    # Calculer les composantes x et y de la direction du vent
    dt_qualite_air <- dt_qualite_air %>%
      mutate(Vent_x_est = cos(Angle_radians),
             Vent_y_nord = sin(Angle_radians))
    
    # creation du dataframe
    # on ajoute directement la colonne d'interet de groupe de qualité de l'air:
    datamodele=data.frame(dt_qualite_air$qualite_air_groupe)
    colnames(datamodele)<-"qualite_air_groupe"
    
    # en fonction des variables selectionnées, on crée un nouveau dataframe
    # récupération des inputs choisis par l'utilisateur 
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
    
    # Creation des data train (80%) et test (20%)
    # séparation en train et en test
    n.train <- round(nrow(datamodele)*0.80,0)
    train_indices <- sample(1:nrow(datamodele), n.train)
    
    # Creation du data.train
    datamodele.train <- datamodele[train_indices, ]
    
    # Creation du data.test 
    datamodele.test <- datamodele[-train_indices, ]
    
    # Define the control parameters for cross-validation
    ctrl <- trainControl(
      method = "cv",             # Cross-validation method
      number = 10,               # Number of folds
      summaryFunction = twoClassSummary,  # For binary classification
      classProbs = TRUE          # For obtaining class probabilities
    )
    
    datamodele.train$qualite_air_groupe <- make.names(datamodele.train$qualite_air_groupe)
    
    # Fit the logistic regression model with cross-validation on the training data
    mod.glm_cv <- train(
      qualite_air_groupe ~ .,   # Formula for your model
      data = datamodele.train,        # Training data
      method = "glm",           # Use glm for logistic regression
      family = "binomial",      # Binomial family for logistic regression
      trControl = ctrl          # Use the control parameters defined earlier
    )
    
    # On voit la prediction sur les donnees train 
    
    scores.glm_test <- predict(mod.glm_cv, newdata = datamodele.train, type = "prob")
    positive_class_probs <- scores.glm_test[, 1]
    
    # Create an ROC object using the positive class probabilities
    roc_obj <- roc(datamodele.train$qualite_air_groupe, positive_class_probs)
    
    # Plot the ROC curve for the train dataset
    plot(roc_obj)
    
    # Get the coordinates of the ROC curve
    optimal_cutoff <- coords(roc_obj, "best")$threshold
    
    # Classify using the optimal cutoff
    pred_CV <- ifelse(positive_class_probs > optimal_cutoff, "Groupe_1.2", "Groupe_3.4")
    pred_CV <- as.factor(pred_CV)
    
    # Evaluate the model on the test dataset
    accuracy <- mean(pred_CV == datamodele.train$qualite_air_groupe)
    datamodele.train$qualite_air_groupe <- as.factor(datamodele.train$qualite_air_groupe)
    
    cat("Accuracy on Test Set:", accuracy, "\n")
    
    conf_matrix <- confusionMatrix(data = pred_CV,
                                   reference = datamodele.train$qualite_air_groupe)
    conf_matrix
    
    
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
