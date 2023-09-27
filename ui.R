library(shiny)
library(colourpicker)
library(shinydashboard)
library(knitr)
library(fresh)
library(shinyWidgets)


# rmdfiles <- c("RMarkdownFile.rmd")
# sapply(rmdfiles, knit, quiet = T)

# creation du theme de l'application
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#FFD700",
    dark_hover_bg = "#D8DEE9",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
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
                       sidebarSearchForm(textId = "searchTown", buttonId = "searchButton",
                                         label = "Rechercher une ville", icon = shiny::icon("search")),
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
                          "Ici nous allons présenter notre projet.",
                          fluidRow(
                            box(
                              title = "Notre objectif", width = 8, solidHeader = TRUE,
                              "Box content"
                            )
                          )
                 ),
                 # deuxième onglet carte
                  tabItem("Carte", 
                          "Ici nous allons afficher la carte interactive de la Bretagne.",
                          fluidRow(
                            box(title = "Titre carte interactive", "Box content", plotOutput("map")), 
                            div(style = "position:relative; left:calc(25%);", sliderInput("Dates",
                                        "Date sélectionnée:", 
                                        min = as.Date("2021-09-22","%Y-%m-%d"),
                                        max = as.Date("2023-09-22","%Y-%m-%d"),
                                        value=as.Date("2021-09-22"),
                                        timeFormat="%Y-%m-%d", 
                                        width = "50%") 
                                        )
                          
                          )
                 ),
                 
                 # troisième onglet "Données météo"
                  tabItem("Météo", 
                          "Ici nous allons afficher les données météo.",
                          fluidRow(
                            box(title = "Titre graph évolution pluie et température", "Box content", plotOutput("plotRainTemp")
                          ),
                          box(title = "Titre graph évolution de la pression", "Box content", plotOutput("plotPres")
                          ),
                          box(title = "Titre graph évolution du vent", "Box content", plotOutput("plotWind")
                          )
                          )),
                 
                 # troisème onglet "Qualité de l'air"
                 tabItem("Rapport", 
                          "Ici nous allons afficher une analyse statistique des données permettant d'expliquer la varible qualité de l'air."
                          # withMathJax(includeMarkdown("RMarkdownFile.md")
                 ))
      )
    )
    )
)
  