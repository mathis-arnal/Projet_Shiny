## appel des packages nécessaires

library(shiny)
library(colourpicker)
library(shinydashboard)
library(knitr)
library(fresh)
library(leaflet)
library(dygraphs)
library(shinyWidgets)
library(data.table)
library(rmarkdown)
library(tufte)
library(fontawesome)
library(ggmap)
library(xts)
library(dplyr)
library(car)
library(FactoMineR)

## ouverture de nos jeux de données 

rmdfiles <- c("scripts/Qualite_air.Rmd")
sapply(rmdfiles, knit, quiet = T)

meteostat_data <- fread("data/meteostat_data.csv", header=TRUE)
meteostat_data$time <- as.Date(meteostat_data$time, format="YYYY-MM-DD")
air_quality <- fread("data/quality_index_rennes.csv")