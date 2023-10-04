## CARTE INTERACTIVE

##### Chargement des packages
library(fontawesome)
library(ggmap)
library(leaflet)
library(ggmap)
# importation du jeu de donnes
dataMeteo <- read.table("data/meteostat_data.csv",sep=",", header=TRUE)
# importation du jeu de donnée de la qualité de l'air pour recuperer la colonne qui nous interesse
datatemp<-read.csv("data/quality_index_rennes.csv")
dataMeteo$qualite_air<-datatemp$code_qual # colonne récupérée
rm(datatemp) # on supprime le jeu de données

<<<<<<< HEAD
f <- function(map){
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng = -1.6788, lat = 48.1123, popup = "Rennes") %>%
    addMarkers(lng = -4.4886, lat = 48.3917, popup = "Brest") %>%
    addMarkers(lng = -4.1035, lat = 47.9959, popup = "Quimper") %>%
    addMarkers(lng = -2.7617, lat = 48.5144, popup = "Saint- Brieuc") %>%
    addMarkers(lng = -2.7576, lat = 47.6572, popup = "Saint- Brieuc") 
  
  print(m)
}

f(map)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = -1.6788, lat = 48.1123,icon = windIcon, popup = "Rennes") %>%
  addMarkers(lng = -4.4886, lat = 48.3917, popup = "Brest") %>%
  addMarkers(lng = -4.1035, lat = 47.9959, popup = "Quimper") %>%
  addMarkers(lng = -2.7617, lat = 48.5144, popup = "Saint- Brieuc") %>%
  addMarkers(lng = -2.7576, lat = 47.6572, popup = "Vannes") %>%
  leafletOptions(dragging=FALSE)

m

# Define a custom icon with HTML
sunIcon <- makeIcon(
  iconUrl = "http://rosatubes.r.o.pic.centerblog.net/o/a27bca1d.png",
  iconWidth = 100, iconHeight = 60
)
<<<<<<< HEAD


=======
# premiere carte "test" avec juste les pointeurs
# m <- leaflet() %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   addMarkers(lng = -1.6788, lat = 48.1123, popup = "Rennes") %>%
#   addMarkers(lng = -4.4886, lat = 48.3917, popup = "Brest") %>%
#   addMarkers(lng = -4.1035, lat = 47.9959, popup = "Quimper") %>%
#   addMarkers(lng = -2.7617, lat = 48.5144, popup = "Saint- Brieuc") %>%
#   addMarkers(lng = -2.7576, lat = 47.6572, popup = "Vannes") 
# 
# m
>>>>>>> b63bd21441e2f1b308a4f82689c1edc370ab382c
##################################################################################
######## WIND DIRECTION

# recuperation de l'orientation du vent pour chaque ville pour une date donnée:
# attention l'ordre doit etre toujours le meme:
# rennes, brest, quimper, saint-brieuc, vannes
date="2021-09-30"
# recuperation
VectwindDir= as.vector(dataMeteo[which(dataMeteo$time==date),7])
VectwindDir


#####
# a partir de la direction du vent en degr?, on va approximer la direction
# a un des huit point carnidaux le plus proche. Si l'angle est de 5? on approxime
# a 0 et on dit qu ela direction g?n?r?le est Nord

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
<<<<<<< HEAD

windIconB <- makeIcon(
  iconUrl = "https://icon-library.com/images/wind-direction-icon/wind-direction-icon-4.jpg",
  iconWidth = 25, iconHeight = 30
=======
sunIcon
MathisIcon <- makeIcon(
  iconUrl = "https://media.licdn.com/dms/image/C4E03AQE_YlB1pE1FCw/profile-displayphoto-shrink_800_800/0/1640697833497?e=2147483647&v=beta&t=eSoQ1C8vC1I-dqnt1uZhqITq4DDwl6Q3pKO2zL2-jiI",
  iconWidth = 150, iconHeight = 60
>>>>>>> lou
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
=======
>>>>>>> b63bd21441e2f1b308a4f82689c1edc370ab382c


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

<<<<<<< HEAD
=======
m
# Add markers with custom sun icons for each city
m <- m %>%
  addMarkers(
    lng = -1.6788, lat = 48.1123, 
    icon = sunIcon, 
    popup = "Rennes"
  )
m
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
>>>>>>> lou
m
 
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



####### creation des Icons pour le vent a partir d'un imagne internet
date="2021-09-25"
# recuperation
Vectprcp= as.vector(dataMeteo[which(dataMeteo$time==date),5])
Vectprcp

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

m
##################################################################################
# QUALITE DE L'AIR'

# rennes, brest, quimper, saint-brieuc, vannes
# date="2021-09-25"
# recuperation
# VectQualAir= as.vector(dataMeteo[which(dataMeteo$qual==date),13])
# VectQualAir
