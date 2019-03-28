library(tidyverse)
library(sf)
library(leaflet)
library(rlang)

# Get Shapefile for provinces ----------------------------------------

link_istat <- paste0("http://www.istat.it/storage/cartografia/",
                     "confini_amministrativi/archivio-confini/",
                     "generalizzati/2016/Limiti_2016_WGS84_g.zip")


municip_path <- "data/istat_municipalities"

if(!file.exists(municip_path)) {
  
  temp <- tempfile()
  
  link_istat %>% 
    download.file(destfile = temp)
  
  temp %>% unzip(exdir = municip_path)
  rm(temp)
  
}

prov_ita <- 
  paste0("data/istat_municipalities/Limiti_2016_WGS84_g/",
         "CMProv2016_WGS84_g/CMprov2016_WGS84_g.shp") %>% 
  sf::st_read() %>% 
  janitor::clean_names()


# plot shapes -------------------------------------------------------------

library(ggspatial)

prov_ita %>%
  ggplot() +
  geom_sf() +
  # scale_fill_viridis_c(trans = "log") +
  # theme_bw()
  theme_minimal()


# load production istat ---------------------------------------------------

# generated in get-prod-istat.R
load("data/mais-prod-istat.Rdata") 


production_tidy <- 
  mais_istat %>% 
  # three methods: "r" "t" "s", why?
  # looks like t are regions
  filter(metodo != "t",
         provincia != "ITALIA")

# merge by "provincia
# no_match <- 
#   production_tidy %>% 
#   filter(! provincia %in% prov_ita$provincia) %>% 
#   pull(provincia)
# 
# no_match2 <- 
#   prov_ita %>% 
#   filter(! provincia %in% production_tidy$provincia) %>% 
#   pull(provincia)

prov

production_tidy$provincia %in% prov_ita$provincia
prov_ita$provincia %in% production_tidy$provincia 


