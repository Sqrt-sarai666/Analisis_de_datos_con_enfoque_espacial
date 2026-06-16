# Library -----------------------------------------------------------------

library(rsconnect)
library(leaflet)
library(tidyverse)
library(leaflet.extras) # Heatmap
library(h3jsr) # H3 uber
library(sf)

# data --------------------------------------------------------------------

fgj = read_csv("data csv/victimasFGJ_2024.csv")

d1 = fgj %>% filter(categoria_delito== "HOMICIDIO DOLOSO")

d2 = fgj %>%  
  filter(categoria_delito=="VIOLACIÓN") %>%
  filter(!is.na(longitud)) %>% filter(!is.na(latitud))

# Cluster ----------------------------------------------------------------

leaflet() %>%  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(data=d1, lng= d1$longitud,lat=d1$latitud,clusterOptions = T)

# Heatmap -----------------------------------------------------------------

leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addHeatmap(data= d2, lng= d2$longitud ,lat= d2$latitud, 
             minOpacity = 0.5 , radius = 50)

# Spacial object ----------------------------------------------------------

d2_esp = d2 %>% select(delito, longitud, latitud) %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

# res = n (n = {0,...,15}) size of hexagon 
# where 0 is the max. size and 15 is the min.
d2_esp_final = d2_esp %>% mutate(id_h3 = point_to_cell(geometry, res = 8)) %>% 
  as_tibble() %>%  count(id_h3)

ggplot()+
  geom_density(aes(d2_esp_final$n), linewidth = 1, fill = "red")
# in this plot, we can observate the density of rapes by zone

# My first spatial object :D
d2_h3 = d2_esp_final %>% mutate(geometry = cell_to_polygon(id_h3)) %>% 
  st_as_sf()


# Polygon plot ------------------------------------------------------------

leaflet() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolygons(data = d2_h3, weight = 1, color = "white")


# Color map ---------------------------------------------------------------

pltt = colorNumeric(palette = "viridis", domain = d2_h3$n) # Palette

leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolygons(data = d2_h3, weight = 1, color =~ pltt(n),
              opacity = 1, fillOpacity = 1) %>% 
  addLegend(pal = pltt, values = d2_h3$n, title = "Frecuency")

