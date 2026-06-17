# Library -----------------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(leaflet)
library(bivariateLeaflet)
library(sf)
library(forcats)

# Invst

# Using a INEGI denue token  ----------------------------------------------

tk = "775f6398-df7a-4e73-a625-5e3c4d6706b0"
lat_lng = "20.119537693309756,-98.73809109723558"
link_base = "https://www.inegi.org.mx/app/api/denue/v1/consulta/Buscar/todos"
radius = "750"
link_final = paste0(link_base, "/", lat_lng, "/", radius, "/", tk)

# JSON --------------------------------------------------------------------

denue = jsonlite::fromJSON(link_final) # Using library:: is a good practice :D 
                                       # Also serves to call only that function
denue = denue %>% dplyr::as_tibble() 

denue %>% dplyr::glimpse() # A glimpse of data

denue = denue %>% dplyr::mutate(Longitud = as.numeric(Longitud), 
                                Latitud = as.numeric(Latitud))
leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircles(data = denue, lng = denue$Longitud, lat = denue$Latitud,
             color = "red")

# Excel -------------------------------------------------------------------

denue_as_excel = writexl::write_xlsx(denue, "negocios_denue.xlsx")

conapo = readxl::read_excel("data cvs/IMM_2020.xlsx", sheet = "IMM_2020")


# Working with Hidalgo data ------------------------------------------------

mun = sf::st_read("data s/00mun.shp")
hid = mun %>% filter(CVE_ENT == 13)
ggplot()+ 
  geom_sf(data = hid)

conapo %>% glimpse()

mun_final = mun %>% left_join(conapo, c("CVEGEO" = "CVE_MUN") )

hgo = mun_final %>% filter(CVE_ENT.x == 13)

ggplot()+geom_sf(data = hgo, aes(fill = POB_TOT)) # Poblation

ggplot()+geom_sf(data = hgo, aes(fill = ANALF)) # Analphabetism

hgo %>% ggplot(aes(ANALF))+ geom_density(fill = "red")

hgo %>% ggplot()+ geom_sf(aes(fill = GM_2020)) # Margination

# Margination with good desing --------------------------------------------

hgo %>% mutate(GM_2020 = forcats::fct_reorder(GM_2020, IMN_2020)) %>% 
  ggplot() + geom_sf(aes(fill = GM_2020)) + 
  viridis::scale_fill_viridis( discrete = T ) 

hgo %>% mutate(GM_2020 = forcats::fct_reorder(GM_2020, IMN_2020)) %>% 
  as_tibble() %>% group_by(GM_2020) %>% summarise(total = sum(POB_TOT)) %>% 
  ungroup() %>% mutate(pcr = total/sum(total)) %>% 
  mutate(prc = scales::percent(pcr))

# Save the maps!! ---------------------------------------------------------

map_mx = mun_final %>% mutate(GM_2020 = forcats::fct_reorder(GM_2020, IMN_2020)) %>% 
  ggplot() + geom_sf(aes(fill = GM_2020)) + 
  viridis::scale_fill_viridis( discrete = T ) 

map_hgo = hgo %>% mutate(GM_2020 = forcats::fct_reorder(GM_2020, IMN_2020)) %>% 
  ggplot() + geom_sf(aes(fill = GM_2020)) + 
  viridis::scale_fill_viridis( discrete = T ) 

ggsave(filename = "mapa_mexico.png", plot = map_mx, width=12, height = 8)
ggsave(filename = "mapa_hgo.png", plot = map_hgo, width=10, height = 10)

# Use bivariateLeaflet ----------------------------------------------------

# SBASC = Elementary Education

create_bivariate_map(data = hgo, var_1 = "ANALF", var_2 = "SBASC")
create_bivariate_map(data = mun_final, var_1 = "ANALF", var_2 = "SBASC")





