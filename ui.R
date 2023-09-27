library(shiny)
library(colourpicker)
library(shinydashboard)
library(knitr)
library(fresh)

# rmdfiles <- c("RMarkdownFile.rmd")
# sapply(rmdfiles, knit, quiet = T)

# creation du theme de l'application
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#404040",
    dark_hover_bg = "#81A1C1",
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
  dashboardPage(skin="green",
    dashboardHeader(title = "Météo Bretagne", titleWidth = 250
                    ),
    dashboardSidebar(width = 250,
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
                       div(style = "position:relative; left:calc(25%);",downloadButton("downloadData","Télécharger"))
                     ),
    dashboardBody(
      use_theme(mytheme),
      
      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: system-ui;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
      
      # navbarPage
      navbarPage("Notre projet", 
                 
                 # premier onglet présentation
                 tabPanel("Présentation", 
                          "Ici nous allons présenter notre projet.",
                          fluidRow(
                            box(
                              title = "Notre objectif", width = 8, solidHeader = TRUE,
                              "Box content"
                            )
                          )
                 ),
                 # deuxième onglet carte
                 tabPanel("Carte interactive", 
                          "Ici nous allons afficher la carte interactive de la Bretagne.",
                          fluidRow(
                            box(title = "Titre carte interactive", "Box content", plotOutput("map"))
                          )
                 ),
                 
                 # troisième onglet "Données météo"
                 tabPanel("Données météo", 
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
                 tabPanel("Qualité de l'air", 
                          "Ici nous allons afficher une analyse statistique des données permettant d'expliquer la varible qualité de l'air."
                          # withMathJax(includeMarkdown("RMarkdownFile.md")
                 )
      )
    )
    )
)
  