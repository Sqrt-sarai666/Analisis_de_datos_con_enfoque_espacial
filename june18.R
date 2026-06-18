# Librarys ----------------------------------------------------------------

library(mapedit)
library(nngeo)
library(tidygeocoder)
library(osmdata)
library(mapview)
library(mapboxapi)

library(sfdep)
library(tidyverse)
library(sf)

# library(sfnetwork)

# Database ----------------------------------------------------------------

# Checar AMAI

conapo = readxl::read_excel("data cvs/IMM_2020.xlsx", sheet = "IMM_2020")
mun = sf::st_read("data cvs/data s/00mun.shp")
hid = mun %>% filter(CVE_ENT == 13)

conapo %>% glimpse()

mun_final = mun %>% left_join(conapo, c("CVEGEO" = "CVE_MUN") )

hgo = mun_final %>% filter(CVE_ENT.x == 13)

# Using the same "unity"
mun = mun %>% st_transform(crs = 4326)

dots %>% st_join(mun, left =T) # know the info of that dots

# Dots --------------------------------------------------------------------

dots = mapedit::editMap() 

dots

st_write(obj = dots, "puntos.gpkg", append = FALSE )


# Lines -------------------------------------------------------------------

way = mapedit::editMap()

cruza = mun %>% st_join( y = way, join = st_intersects) %>% 
  janitor::clean_names() %>% filter(!is.na(leaflet_id))

mapview::mapview(cruza)+mapview(way)
mapview::mapview(cruza)|mapview(way)

# Polygons ----------------------------------------------------------------

uaeh = mapedit::editMap() 

st_write(uaeh, "uaeh.gpkg" )

mf2_3 =  mapedit::editMap()

st_write(mf2_3, "mf2_3.gpkg" )

# Buffer ------------------------------------------------------------------

mapview::mapview(dots)

# comun error

b1 = st_buffer(x= dots, dist = 10)  

mapview::mapview(b1)

dots2 = dots %>% st_transform(32614) %>%  st_buffer(dist = 100) %>% 
  st_transform(crs = 4326)

mapview::mapview(dots2)

# Anexo tecnico epsg

# Geocoding ---------------------------------------------------------------

initpoint = editMap()

endpoint = editMap()

token = ""

initpoint

iso1 = mapboxapi::mb_isochrone(initpoint, profile = "cycling", time = c(5,10,15), 
                               access_token = token)

mapview(iso1, z= "time")

# ruta 

formato = rbind(initpoint, endpoint)
formato

ruta1 = mb_optimized_route(input_data = formato, profile = "driving", 
                           output = "sf", roundtrip = F, access_token = token)
mapview(ruta1)


street = "Veracruz, Poza Rica, Mango"

street_point = mb_geocode(search_text = street, country = "MX", language = "ES", output = "sf", access_token = token)

street_point
mapview(street_point)
