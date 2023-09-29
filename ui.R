library(shiny)
library(colourpicker)
library(shinydashboard)
library(knitr)
library(fresh)
library(leaflet)
library(dygraphs)
library(shinyWidgets)

rmdfiles <- c("Qualite_air.Rmd")
sapply(rmdfiles, knit, quiet = T)

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
                     menuItem("Carte interactive", tabName = "Carte"), 
                     menuItem("Données météo", tabName = "Météo"),
                     menuItem("Qualité de l'air", tabName = "Rapport"),
                     
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
    '))),
      
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
                            box(title = "Météo en bretagne du 22/09/2021 au 22/09/2023", leafletOutput("carte",
                                                                                height = 500, width = 950), width = 10), 
                            div(style = "position:relative; left:calc(25%);", sliderInput("Date",
                                                                                          "Date sélectionnée",
                                                                                          min = as.Date("2021-09-22","%Y-%m-%d"),
                                                                                          max = as.Date("2023-09-22","%Y-%m-%d"),
                                                                                          value = as.Date("2021-09-22"), 
                                                                                          timeFormat="%Y-%m-%d", 
                                                                                          width = "50%")
                                         
                                        )
                          
                          )
                 ),
                 
                 # troisième onglet "Données météo"
                  tabItem("Météo", 
                          "Ici nous allons afficher les données météo.",
                          fluidRow(
                            box(title = "La température ...", dygraphOutput("plotRainTemp")
                          ),
                          box(title = "La pluie ... ",dygraphOutput("plotPres")
                          ),
                          box(title = "Titre graph évolution du vent", "Box content", plotOutput("plotWind")
                          )
                          )),
                 
                 # troisème onglet "Qualité de l'air"
                 tabItem("Rapport", 
                          "Ici nous allons afficher une analyse statistique des données permettant d'expliquer la varible qualité de l'air.",
                         downloadButton("report", "Generate report"),
                         includeMarkdown("Qualite_air.md")
                 ))
      )
    )
    )
)
  