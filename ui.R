# creation du theme de l'application
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#7AC5CD"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#9BCD9B",
    dark_hover_bg = "#7AC5CD",
    dark_color = "#53868B"
  ),
  adminlte_global(
    content_bg = "#E0EEE0",
    box_bg = "#FFF5EE", 
    info_box_bg = "#CDC673"
  )
)

# Define UI 
shinyUI(
  dashboardPage(skin="blue",
    dashboardHeader(title = "Météo Bretagne", titleWidth = 250
                    ),
    dashboardSidebar(width = 250, ## création du menu
                     sidebarMenu(id  = "menu",
                     menuItem("Notre projet", tabName = "Projet"),
                     menuItem("Carte interactive", tabName = "Carte", icon = icon("map")), 
                     menuItem("Données météo", tabName = "Météo", icon = icon("cloud-rain")),
                     menuItem("Qualité de l'air", tabName = "Rapport", icon = icon("searchengin")),
                     
                     conditionalPanel('input.menu == "Météo"',
                     tags$style("p {font-size: 18px;font-weight: bold}"),
                     div(style = "position:relative; left:calc(25%);", br(), p("Fonctionnalités")),
                     radioButtons("ville", "Choisissez la ville de votre choix :",
                                  choiceNames = list(
                                    "Rennes",
                                    "Saint-Brieuc",
                                    "Vannes",
                                    "Brest",
                                    "Quimper"
                                  ),
                                  choiceValues = list(
                                    "text", "text", "text", "text", "text"
                                  )),
                       dateRangeInput(inputId = "idDateRange", label = "Sélectionner la période qui vous intéresse : ",
                                      start = "2020-01-01", end = "2023-09-22", format = "yyyy-mm-dd",
                                      language = "fr", separator = " to "),
                       checkboxInput(inputId = "idCheckair", label = "Indice de qualité de l'air"),
                      ##Bouton poour lancer l'analyse 
                       div(style = "position:relative; left:calc(15%);", actionButton( "go", "Afficher les graphiques")),
                       selectInput("dataset", "Choisissez le jeu de données que vous souhaitez télécharger :",
                                 choices = c("Données météo", "Données indice qualité de l'air")),
                     
                        # Bouton
                       div(style = "position:relative; left:calc(25%);",downloadButton("downloadData","Télécharger"))))
                     
                     ),
    dashboardBody(chooseSliderSkin("Flat"),
      
      use_theme(mytheme),
      
      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: system-ui;
        font-weight: bold;
        font-size: 24px;
      }
    '),
      tags$head(tags$style(type='text/css', 
                           ".slider-animate-button { font-size: 20pt !important; }")))),
      
      # navbarPage
      navbarPage("Des bottes et un ciré ?", 
                 # premier onglet présentation
                 tabItems(
                   # First tab content
                   tabItem("Projet", 
                          fluidRow(
                            box(
                              title = "Notre objectif", width = 8, solidHeader = TRUE,
                              "Apprécié, moqué ou envié, le temps breton est un phénomène intéressant à étudier. Cette application permet de visualiser l'évolution des différents
                              évènements météorologiques en Bretagne entre 2021 et 2023.", br(), 
                              "Nous avons sélectionné les données météorologiques de 5 villes bretonnes afin de simplifier le traitement des données.", br(), 
                              "Un premier onglet « Carte interactive » vous donne la possibilité de sélectionner un phénomène (« pluie » ou « vent ») et de 
                              visualiser son évolution dans le temps entre le 22/09/2021 et le 22/01/2023. L’interface permet s’arrêter à une date T ou bien de visualiser son 
                              évolution de manière automatique.", br(), 
                              "L’onglet « Données météo », permet de visualiser de manière quantitative les différents paramètres météorologiques pour une ville sélectionnée 
                              ainsi que pour une période donnée. Il est aussi possible de comparer les villes entre elles. Enfin, les fichiers de données utilisés sont téléchargeables 
                              pour que vous puissiez y avoir accès.", br(), 
                              "L’onglet « Qualité de l’air », présente notre étude statistique concernant l'indice de qualité de l'air*. Elle a permis de mettre en lumière les 
                              paramètres permettant d'expliquer au mieux cet indicateur. Vous pouvez aussi vous-même créer votre modèle en choisissant différents paramètres afin 
                              d’essayer de prédire au mieux l’indicateur de qualité de l’air en Bretagne.", br(), 
                              "Notre objectif était de permettre une visualisation simple et interactive des différents phénomènes météorologiques en Bretagne. 
                              Ainsi qu’une analyse de ces données pour pouvoir expliquer l’indice de qualité de l’air observé en Bretagne dans 5 villes sélectionnées.", br(), 
                              "Nos pistes d’améliorations sont :" , br(), 
                              "-	Mettre à jour de manière automatique les données (actualisation tous les jours).", br(),
                              "-	Améliorer et affiner la visualisation en sélectionnant un nombre de ville plus important.", br(),
                              "-	Permettre à l’utilisateur de télécharger son analyse sous un Rmarkdown.", br(),
                              "-	Améliorer notre modèle prédictif. ", br(), 
                              "Ce projet, nous a beaucoup plus et nous espérons que l’utilisation de l’application vous plaira également.", br(), br(),
                              "Renée, Mathis et Lou", 
                              br(), br(), br(),
                              "*Pour plus d'information sur l'indice de qualité de l'air vous pouvez vous référez au lien suivant :",
                              uiOutput("lien")
                              
                            )
                          )
                 ),
                 # deuxième onglet carte
                  tabItem("Carte",
                          fluidRow(
                            column(width = 10,
                            box(title = "Météo en bretagne du 22/09/2021 au 22/09/2023", leafletOutput("map",
                                                                                height = 500, width = 950), width = 12)), 
                            column(width = 2, selectInput(inputId = "map", label = "Que voulez-vous observer ?",
                                                                 choices = c("Vent", "Pluie", "ATMO"))), 
                            div(style = "position:relative; left:calc(25%);", sliderInput("Date",
                                                                                          "Date sélectionnée",
                                                                                          min = as.Date("2021-09-22","%Y-%m-%d"),
                                                                                          max = as.Date("2023-09-22","%Y-%m-%d"),
                                                                                          value = as.Date("2021-09-22"), 
                                                                                          timeFormat="%Y-%m-%d", 
                                                                                          width = "50%",
                                                                                          animate = animationOptions(interval = 300,
                                                                                                                     playButton = icon('play', "fa-2x"),
                                                                                                                     pauseButton = icon('pause', "fa-2x"))))
                                
                                
                                         
                                        )
                          
                          
                 ),
                 
                 # troisième onglet "Données météo"
                  tabItem("Météo", 
                          "Ici, vous pouvez observer les données météo de la ville qui vous intéresse sur une période souhaitée
                          (entre le 22/09/2021 et 22/09/2023), une fois que vous avez sélectionner vos 
                          paramètres, cliquez sur -Afficher les graphiques-." ,
                          fluidRow(
                            box(title = "La température ...", dygraphOutput("plotTemp")
                          ),
                          box(title = "La pluie ... ",dygraphOutput("plotPres")
                          ),
                          box(title = "Le vent ... ", "Box content", dygraphOutput("plotWind")
                          )
                          )),
                 
                 # troisème onglet "Qualité de l'air"
                 tabItem(
                   "Rapport", 
                   navbarPage( "" ,
                              tabPanel("Notre rapport", "Veuillez trouver ci-dessous notre analyse statistique des données permettant d'expliquer la variable -qualité de l'air-.",
                             includeMarkdown("Qualite_air.md")),
                              tabPanel("A vous de jouer !", "Sélectionner les paramètres que vous voulez prendre en compte dans votre modèle expliquant 
                                       l'indice ATMO :",
                                       checkboxInput(inputId = "prcp", label = "précipitations quotidienne"),
                                       checkboxInput(inputId = "wdir", label = "direction moyenne du vent"),
                                       checkboxInput(inputId = "tavg", label = "température moyenne"),
                                       checkboxInput(inputId = "tmin", label = "température minimale"),
                                       checkboxInput(inputId = "tmax", label = "température maximale"),
                                       checkboxInput(inputId = "wspd", label = "vitesse moyenne du vent"),
                                       checkboxInput(inputId = "pres", label = "pression"),
                                       checkboxInput(inputId = "wpgt", label = "raffale maximale"),
                                       checkboxInput(inputId = "lag1", label = "lag 1"),
                                       checkboxInput(inputId = "lag2", label = "lag 2"),
                                       checkboxInput(inputId = "lag3", label = "lag 3"),
                                       ##Bouton poour lancer l'analyse 
                                       div(style = "position:relative; left:calc(15%);", 
                                           actionButton( "allez", "Lancer mon analyse")),
                                       br(),
                                       box(title = "Votre analyse", width = 12, solidHeader = TRUE,
                                           verbatimTextOutput("modele"), plotOutput("CM")
                                           ) 
                                       )
                 ))
      )
    )
    )))



