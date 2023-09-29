
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
  addMarkers(lng = -1.6788, lat = 48.1123, popup = "Rennes") %>%
  addMarkers(lng = -4.4886, lat = 48.3917, popup = "Brest") %>%
  addMarkers(lng = -4.1035, lat = 47.9959, popup = "Quimper") %>%
  addMarkers(lng = -2.7617, lat = 48.5144, popup = "Saint- Brieuc") %>%
  addMarkers(lng = -2.7576, lat = 47.6572, popup = "Saint- Brieuc") 

m

library(fontawesome)


# Define a custom icon with HTML
sunIcon <- makeIcon(
  iconUrl = "http://rosatubes.r.o.pic.centerblog.net/o/a27bca1d.png",
  iconWidth = 100, iconHeight = 60
)
sunIcon
MathisIcon <- makeIcon(
  iconUrl = "https://media.licdn.com/dms/image/C4E03AQE_YlB1pE1FCw/profile-displayphoto-shrink_800_800/0/1640697833497?e=2147483647&v=beta&t=eSoQ1C8vC1I-dqnt1uZhqITq4DDwl6Q3pKO2zL2-jiI",
  iconWidth = 150, iconHeight = 60
)

# Create a basic leaflet map
m <- leaflet() %>%
  addTiles()  # Add default OpenStreetMap map tiles

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
m

