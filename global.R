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
library(caret)
library(dplyr)
library(car)
library(FactoMineR)
library(ROCR)
library(pROC)


## ouverture de nos jeux de données 

rmdfiles <- c("scripts/Qualite_air.Rmd")
sapply(rmdfiles, knit, quiet = T)

meteostat_data <- fread("data/meteostat_data.csv", header=TRUE)
meteostat_data$time <- as.Date(meteostat_data$time, format="YYYY-MM-DD")
air_quality <- fread("data/quality_index_rennes.csv")
all_raw_data <- read.csv("data/all_raw_data.csv")
data_path <- file.path("..", "data")
raw_data <- fread("data/all_raw_data.csv", header=TRUE)
raw_data$time <- as.Date(raw_data$time, format="YYYY-MM-DD")

start_date <- as.Date("2021-09-22")
end_date <- as.Date("2023-09-22")
time_range <- seq(from = start_date, to = end_date, by = "1 day")
