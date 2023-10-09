# chargement des packages
library("data.table")
library("dplyr")
library("car")
library("FactoMineR")
library("ggplot2")
library("ROCR")
library("pROC") 
# ---------------------------------------------------------------------------#
######
# importation des données
df <- read.csv(file.path("data/quality_index_rennes.csv"), 
               header=TRUE)
set.seed(45L)
dt_qualite_air <- data.table(df) # transformation en datatable

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
## LAG 
lag1=TRUE
lag2=FALSE
lag3=TRUE

# en fonction des variables selectionnées, on cree un nouveau dataframe:

if (prcp){ # si l'utilisateur a selectionne precipitation
  datamodele$prcp <-dt_qualite_air$prcp } # on ajoute la col au dataframe
if (wdir){
  datamodele$vent_x <-dt_qualite_air$Vent_x_est
  datamodele$vent_y <-dt_qualite_air$Vent_y_nord}
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

## en fonction de ce data frame, on fait les lag:

donnees_lagues1 <- datamodele %>%
  mutate_all(~lag(.))
colnames(donnees_lagues1) <- c("qualite_air_groupe_lag1", "prcp_lag1", "vent_x_lag1","vent_y_lag1","wspd_lag1", "pres_lag1","tavg_lag1")             

donnees_lagues2 <- donnees_lagues1 %>%
  mutate_all(~lag(.))
colnames(donnees_lagues2) <- c("qualite_air_groupe_lag2", "prcp_lag2", "vent_x_lag2","vent_y_lag2","wspd_lag2", "pres_lag2","tavg_lag2")             


donnees_lagues3 <- donnees_lagues2 %>%
  mutate_all(~lag(.))
colnames(donnees_lagues3) <- c("qualite_air_groupe_lag3", "prcp_lag3", "vent_x_lag3","vent_y_lag3","wspd_lag3", "pres_lag3","tavg_lag3")             


# on juxtapose les lag s'ils sont selectionnés:
if (lag1){
  datamodele <- cbind(datamodele, donnees_lagues1[,-1])} 
if (lag2){
  datamodele <- cbind(datamodele, donnees_lagues2[,-1])}
if (lag3){
  datamodele <- cbind(datamodele, donnees_lagues3[,-1])}

# en fonction des lags choisis, on enlèves les premières lignes
if (lag3){
  datamodele <- datamodele[-c(1:3),]
} else if (lag2){  datamodele <- datamodele[-c(1:2),]
}else if (lag1){
  datamodele <- datamodele[-c(1),]
}


## -----------------------------------------------------##
######
## Test du modeles avec les variables de l'utilisateur
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
#fitControl.LGOCV <- trainControl(
#  method = "LGOCV",
#  number=10,
#  p=0.6
#)

#mod.glm.LGOCV <- train(
#  qualite_air_groupe ~ .,
#  data=datamodele.train,
#  method="glm",
#  trControl = fitControl.LGOCV
#)

# pred.glm.grid <- predict(mod.glm.LGOCV,newdata=datamodele.test) # glm prediction

#mod.glm.LGOCV$results

#pred.glm <- predict(mod.glm.LGOCV)

###############

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

##############
# On fait une première matrice de confusion 

# pred_Bayes <- ifelse(positive_class_probs > 0.5, "Groupe_1.2", "Groupe_3.4")
# pred_Bayes <- as.factor(pred_Bayes)
# conf_matrix <- confusionMatrix(data = pred_Bayes,
#                                reference = as.factor(datamodele.train$qualite_air_groupe))
# conf_matrix
# print("Bonne accuracy juste 
#       On se rend compte que la spécificité est pas ouf")
# print("On regarde sur test si on a la même acc")
# 
# print("Essayons sur les données test")

### ----- matrice de confusion sur les données test:

# scores.glm_train <- predict(mod.glm_cv, newdata = datamodele.test, type = "prob")
# positive_class_probs_test <- scores.glm_train[, 1]
# pred_Bayes_test <- ifelse(positive_class_probs_test > 0.5, "Groupe_1-2", "Groupe_3-4")
# pred_Bayes_test <- as.factor(pred_Bayes_test)
# conf_matrix <- confusionMatrix(data = pred_Bayes_test,
#                                reference =as.factor( datamodele.test$qualite_air_groupe))
# conf_matrix
# print ("Globalemnt même résultat, donc à première vue pas trop de surajustement")
# 
# print ("on va essayer d'améliorer ça, avec un différent cutoff,
#        en faisant la courbe roc avec les données train")
#########################


## On fait donc la courbe ROC http://127.0.0.1:20533/graphics/a5d997c0-25c4-481f-87cd-10149d79a810.png
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






