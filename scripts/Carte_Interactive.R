
#####
library(fontawesome)
library(ggmap)
library(leaflet)
library(ggmap)

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


##################################################################################
######## WIND

# récupération de l'orientation du vent pour chaque ville:
VectwindDir= c(40,15,16,30,52)

windRennes=as.character(VectwindDir[1])
windBrest=as.character(VectwindDir[2])
windQuimper=as.character(VectwindDir[3])
windSaintBrieuc=as.character(VectwindDir[4])
windVannes=as.character(VectwindDir[5])

####### creation des Icons pour le vent à partir d'un imagne internet
windIconR <- makeIcon(
  iconUrl = "https://icon-library.com/images/wind-direction-icon/wind-direction-icon-4.jpg",
  iconWidth = 25, iconHeight = 30
  )

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


### creations de la map
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = -1.6788, lat = 48.1123, 
             icon = windIconR, 
             popup = paste("Rennes: ",windRennes, "°")) %>%
  
  addMarkers(lng = -4.4886, lat = 48.3917,
             icon = windIconB,
             popup = paste("Brest: ",windBrest, "°")) %>%
  
  addMarkers(lng = -4.1035, lat = 47.9959, 
             icon = windIconQ,
             popup = paste("Quimper: ",windQuimper, "°")) %>%
  
  addMarkers(lng = -2.7617, lat = 48.5144,
             icon = windIconS,
             popup = paste("Saint-Brieuc: ",windSaintBrieuc, "°")) %>%
  
  addMarkers(lng = -2.7576, lat = 47.6572, 
             icon = windIconV,
             popup = paste("vannes: ",windVannes, "°")) %>%
  leafletOptions(dragging=FALSE)

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

