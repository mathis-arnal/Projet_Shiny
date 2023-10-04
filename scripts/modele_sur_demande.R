# chargement des packages
library("data.table")
library("dplyr")
library("car")
library("FactoMineR")
# ---------------------------------------------------------------------------#
######
# importation des données

df <- read.csv("C:/Users/renax/Desktop/ACO/S9/Programmation_R/Projet_meteo/Projet_Shiny/data/quality_index_rennes.csv", 
               header=TRUE)
set.seed(45L)
dt_qualite_air <- data.table(df) # transformation en datatable

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
######



#######
# creation du dataframe:
# on ajoute directement la colonne d'interet de groupe de qualité de l'air:
datamodele=data.frame(dt_qualite_air$qualite_air_groupe)
colnames(datamodele)<-"qualite_air_groupe"


# Ajouter un bouton "valider"
# On récupere les variables selectionnees par lutilisateur:
prcp = TRUE
wdir = TRUE
tavg = TRUE
tmin = FALSE
tmax = FALSE
wspd = TRUE   
wpgt = FALSE            
pres = TRUE

# en fonction des variables selectionnées, on cree un nouveau dataframe:

if (prcp){ # si l'utilisateur a selectionne precipitation
  datamodele$prcp <-dt_qualite_air$prcp } # on ajoute la col au dataframe
if (wdir){
  datamodele$wdir <-dt_qualite_air$wdir}
if (tmin){
  datamodele$tmin <-dt_qualite_air$tmin}
if (tmax){
  datamodele$tmax <-dt_qualite_air$tmax}
if (wspd){
  datamodele$wspd <-dt_qualite_air$wspd}
if (wpgt){
  datamodele$wpgt <-dt_qualite_air$wpgt}
if (pres){
  datamodele$pres <-dt_qualite_air$pres}
if (tavg){
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
library("caret")

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

mod.glm.LGOCV$results

pred.glm <- predict(mod.glm.LGOCV)
####
# ce qui nous permet de qualifier la modele
confusionMatrix(data=pred.glm,reference=datamodele.train$qualite_air_groupe)




