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

pro_ita <- 
  paste0("data/istat_municipalities/Limiti_2016_WGS84_g/",
         "CMProv2016_WGS84_g/CMprov2016_WGS84_g.shp") %>% 
  sf::st_read()


# plot shapes -------------------------------------------------------------

library(ggspatial)

pro_ita %>%
  ggplot() +
  geom_sf() +
  # scale_fill_viridis_c(trans = "log") +
  # theme_bw()
  theme_minimal()
