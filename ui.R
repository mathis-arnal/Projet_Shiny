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
                dashboardSidebar(width = 250,
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
                dashboardBody(
                  
                  chooseSliderSkin("Flat"),
                  
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
                                           "Le temps breton a un réputation connu, plus ou moins apprécié, il n'en reste pas 
                              moins un sujet d'étude intéressant. Cette application permet de visualiser l'évolution des
                              différents évènements météorologiques entre 2021 et 2023. De plus, une étude statistique 
                              concernant l'indice de qualité* de l'air a permis de mettre en lumière les paramètres permettant
                              d'expliquer au mieux cet indice.",
                                           br(), br(),
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
                                                box(title = "Météo en bretagne du 22/09/2021 au 22/09/2023", leafletOutput("carte",
                                                                                                                           height = 500, width = 950), width = 12)), 
                                         column(width = 2, checkboxGroupInput(inputId = "idCheckGroupMap", label = "Que voulez-vous observer ?", selected = 3,
                                                                              choiceNames =
                                                                                list(icon("cloud-rain"), icon("wind"),
                                                                                     icon("temperature-half"), "Indice ATMO"),
                                                                              choiceValues =
                                                                                list("Pluie", "Vent", "Températures", "ATMO"))), 
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
                                         box(title = "La température ...", dygraphOutput("plotRainTemp")
                                         ),
                                         box(title = "La pluie ... ",dygraphOutput("plotPres")
                                         ),
                                         box(title = "Titre graph évolution du vent", "Box content", plotOutput("plotWind")
                                         ),
                                         box(title = "Évolution de l'indice de qualité de l'air", "Box content", plotOutput("plotAir")
                                         )
                                       )),
                               
                               # troisème onglet "Qualité de l'air"
                               tabItem(
                                 "Rapport", 
                                 navbarPage( "" ,
                                             tabPanel("Notre rapport", "Ici nous allons afficher une analyse statistique des données permettant d'expliquer la varible qualité de l'air.",
                                                      uiOutput("rapport"),
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

