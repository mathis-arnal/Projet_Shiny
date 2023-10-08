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
  
  #Téléchargement du fichier Rmd 
  output$rapport <- renderUI({
      url <- a("Rapport", href = "https://stackoverflow.com/questions/62708534/extract-download-link-from-html-in-r-shiny-app")
      tagList(url)
    })

  
  # Affichage de la carte
  output$carte <- renderLeaflet({
    ## CARTE INTERACTIVE
    # importation du jeu de donnes
    dataMeteo <- meteostat_data
    # importation du jeu de donnée de la qualité de l'air pour recuperer la colonne qui nous interesse
    #datatemp<-air_quality
    #dataMeteo$qualite_air<-datatemp$code_qual # colonne récupérée
    #zrm(datatemp) # on supprime le jeu de données
    
    f <- function(map){
      m <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng = -1.6788, lat = 48.1123, popup = "Rennes") %>%
        addMarkers(lng = -4.4886, lat = 48.3917, popup = "Brest") %>%
        addMarkers(lng = -4.1035, lat = 47.9959, popup = "Quimper") %>%
        addMarkers(lng = -2.7617, lat = 48.5144, popup = "Saint- Brieuc") %>%
        addMarkers(lng = -2.7576, lat = 47.6572, popup = "Saint- Brieuc") 
        m
    }
    
    f(map)
    
    ##################################################################################
    ######## WIND DIRECTION
    
    # recuperation de l'orientation du vent pour chaque ville pour une date donnée:
    # attention l'ordre doit etre toujours le meme:
    # rennes, brest, quimper, saint-brieuc, vannes
    date=input$Date
    
    if (input$idCheckGroupMap == "Vent"){
        # recuperation
        VectwindDir= as.vector(dataMeteo[which(dataMeteo$time==date),7])
        #####
        # a partir de la direction du vent en degre, on va approximer la direction
        # a un des huit points carnidaux le plus proche. Si l'angle est de 5 on approxime
        # a 0 et on dit qu ela direction generale est Nord
        
        # creation d'une fonction qui va recuperer les coordonners les plus proches
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
          }}
        
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
        
        windIconB <- makeIcon(
          iconUrl = "https://icon-library.com/images/wind-direction-icon/wind-direction-icon-4.jpg",
          iconWidth = 25, iconHeight = 30
        )
        
        windIconQ <- makeIcon(
          iconUrl = "https://icon-library.com/images/wind-direction-icon/wind-direction-icon-4.jpg",
          iconWidth = 25, iconHeight = 30
        )
        
        windIconS <- makeIcon(
          iconUrl = "https://icon-library.com/images/wind-direction-icon/wind-direction-icon-4.jpg",
          iconWidth = 25, iconHeight = 30
        )
        
        windIconV <- makeIcon(
          iconUrl = "https://icon-library.com/images/wind-direction-icon/wind-direction-icon-4.jpg",
          iconWidth = 25, iconHeight = 30
        )
        
        ### creations de la map
        m <- leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
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
                     popup = paste("Vannes: ",VectwindDir[5], "°")) 
        
        
        # Add markers with custom sun icons for each city
        m <- m %>%
          addMarkers(
            lng = -1.6788, lat = 48.1123, 
            icon = sunIcon, 
            popup = "Rennes"
          )
        
        m <- m %>%
          addMarkers(
            lng = -4.4886, lat = 48.3917, 
            icon = sunIcon, 
            popup = "Brest"
          )
        
        m <- m %>%
          addMarkers(
            lng = -4.1035, lat = 47.9959, 
            icon = sunIcon, 
            popup = "Quimper"
          )
        
        m <- m %>%
          addMarkers(
            lng = -2.7617, lat = 48.5144, 
            icon = sunIcon, 
            popup = "Saint-Brieuc"
          )
        
        m <- m %>%
          addMarkers(
            lng = -2.7576, lat = 47.6572, 
            icon = sunIcon, 
            popup = "Vannes"
          )
        
        # Display the map
        m
        }
    
    if (input$idCheckGroupMap == "Pluie"){
      
      ##################################################################"
      ####################  PRECIPITATIONS  ###########"
      
      # d'après les donneés météo, on a cherche les tritiles
      # pasprcpIcon: 0
      # peuprcpIcon: 0-1,3
      # moyenprcpIcon: 1,3- 25.4
      # bcpprcpIcon: 25.4-53.6
      
      #  terciles
      #   0%  66%  99% 
      #  0.0  1.3 25.4
      
      # recuperation
      Vectprcp= as.vector(dataMeteo[which(dataMeteo$time==date),5])
      
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
      }
      
      # on applique la fonction a notre vecteur
      temp=sapply(Vectprcp, get_prcp)
      prcpall<- paste(sapply(Vectprcp, get_prcp),"prcpIcon", sep="")
      
      
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
      
      
      ### creations de la map
      m <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
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
      
      m}
    
      
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
