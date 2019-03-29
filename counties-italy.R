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
  janitor::clean_names() %>% 
  # sometime "provincia" is missing
  mutate(provincia = case_when(provincia == "-" ~ den_cmpro,
                               TRUE ~ provincia))


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
         provincia != "ITALIA") %>% 
  # fix some manually
  mutate(provincia = case_when(provincia == "Forlì-Cesena" ~ "Forli'-Cesena",
                               provincia == "Bolzano/Bozen" ~ "Bolzano",
                               provincia == "Massa-Carrara" ~ "Massa Carrara",
                               TRUE ~ provincia)) %>% 
  # 3 variables as character, why?
  mutate_at(vars(superficie, produzione_totale, produzione_raccolta), as.integer)
  

# merge by "provincia
no_match <-
  production_tidy %>%
  filter(! provincia %in% prov_ita$provincia) #%>%
  # pull(provincia)
# 
no_match2 <-
  prov_ita %>%
  filter(! provincia %in% production_tidy$provincia) %>%
  pull(provincia)

# merge 
production_shape <- 
  production_tidy %>% 
  full_join(prov_ita)


# plot --------------------------------------------------------------------

bg <-   "grey90" # "white"

p <- 
  production_shape %>%
  ggplot() +
  # theme_bw()
  theme_void() +
  theme(plot.background = element_rect(fill = bg) , panel.grid = element_line(colour = bg))

p_fill <- 
  p +
  scale_fill_viridis_c(guide = FALSE) 

p_log <- 
  p + 
  scale_fill_viridis_c(trans = "log10")


# load regions ------------------------------------------------------------

reg_ita <- 
  paste0("data/istat_municipalities/Limiti_2016_WGS84_g/",
         "Reg2016_WGS84_g/Reg_2016_WGS84_g.shp") %>% 
  sf::st_read()


png(filename = "every-border",
    res = 300,
    height = 2000, width = 1500)
p_fill +
  labs(title = "Show every border") +
  geom_sf(aes(fill = produzione_raccolta),
          colour = bg,
          # size = 0) +
          size = .3) 
dev.off()

png(filename = "high-level-border",
    res = 300,
    height = 2000, width = 1500)
p_fill + 
  labs(title = "Better? Show borders at higher level") +
  geom_sf(aes(fill = produzione_raccolta),
          colour = bg,
          size = 0) +
  geom_sf(data = reg_ita,
          fill = NA,
          colour = bg, 
          size = .5)
dev.off()
 