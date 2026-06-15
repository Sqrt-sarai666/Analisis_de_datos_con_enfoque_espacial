# Geo-spatial analisys 06/15/26

# Librarys

library(tidyverse)
library(leaflet)
library(rsconnect)

# csv

fgj = read.csv("data csv/victimasFGJ_2024.csv")

# Exploring data

fgj %>%count(categoria_delito,sexo) # count(var) count registers of var
unique(fgj$delito) # unique(var) values of var
unique(fgj$categoria_delito)

# My first map :D

base_1 = fgj %>% 
  filter(categoria_delito == "HOMICIDIO DOLOSO")

base_2 = fgj %>% 
  filter(categoria_delito == "VIOLACIÓN")

base_1 %>% 
  select(delito, longitud,latitud)

# Plots

ggplot()+
  geom_point(data = base_1,
             aes( x= longitud,
             y = latitud))

leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addCircles(data = base_1,
             lng = base_1$longitud,
             lat = base_1$latitud)


# Publish in Posit Cloud --------------------------------------------------

# rsconnect::connectCloudUser()
rsconnect::deployDoc("maps/firstmap.html")

