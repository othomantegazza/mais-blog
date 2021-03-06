library(tidyverse)
library(sf)
library(leaflet)
library(rlang)

# MAKE A DIRECTORY NAMED DATA to STORE DATA!!!!!

# Rice Production - Download files ---------------------------------------

links <- c(Bergamo = "https://www.dati.lombardia.it/api/views/sp2m-8pyv/rows.csv?accessType=DOWNLOAD",
           Milano = "https://www.dati.lombardia.it/api/views/8t7w-8tqw/rows.csv?accessType=DOWNLOAD",
           Pavia = "https://www.dati.lombardia.it/api/views/7n6i-5dyc/rows.csv?accessType=DOWNLOAD",
           Lecco = "https://www.dati.lombardia.it/api/views/gf5m-a45v/rows.csv?accessType=DOWNLOAD",
           Brescia = "https://www.dati.lombardia.it/api/views/usvu-2evr/rows.csv?accessType=DOWNLOAD",
           Mantova = "https://www.dati.lombardia.it/api/views/dk23-emij/rows.csv?accessType=DOWNLOAD",
           Varese = "https://www.dati.lombardia.it/api/views/nkmt-xhn4/rows.csv?accessType=DOWNLOAD",
           Lodi = "https://www.dati.lombardia.it/api/views/hvwv-fgj3/rows.csv?accessType=DOWNLOAD",
           Sondrio = "https://www.dati.lombardia.it/api/views/dnyh-ygvh/rows.csv?accessType=DOWNLOAD",
           Como = "https://www.dati.lombardia.it/api/views/v4dh-ebfc/rows.csv?accessType=DOWNLOAD",
           Cremona = "https://www.dati.lombardia.it/api/views/4kuj-9vhh/rows.csv?accessType=DOWNLOAD",
           Monza = "https://www.dati.lombardia.it/api/views/mche-usbq/rows.csv?accessType=DOWNLOAD")

lom_agri_path <- "data/lom_agri.Rdata"

if(!file.exists(lom_agri_path)) {
  lom_agri <-
    links %>% 
    map(read_csv)
  
  save(lom_agri, file = lom_agri_path)
} else {
  load(lom_agri_path)
}

# Merge production datasets ----------------------------------------------------------

# this dataset stores the surface dedicated to maize production
# by type
lom_mais <- 
  lom_agri %>% 
  reduce(rbind) %>% 
  filter(str_detect(UTILIZZO, pattern = "MAIS")) %>% #%>% pull(UTILIZZO) %>% unique()
  group_by(UTILIZZO) %>%
  summarize(sup_used = sum(SUP_UTILIZZATA)) %>% 
  mutate(square_km = sup_used*10^-6)

# plot --------------------------------------------------------------------

lom_mais %>% 
  ggplot(aes(x = reorder(UTILIZZO, square_km),
             y = square_km)) +
  geom_bar(stat = "identity",
           fill = "#3752C3DD") +
  geom_text(data = . %>% 
              filter(square_km > 1000),
            aes(label = square_km %>% round(1)),
            colour = "white",
            hjust = 1,
            nudge_y = -20) +
  geom_text(data = . %>% 
              filter(square_km <= 1000),
            aes(label = square_km %>% round(1)),
            colour = "#3752C3DD",
            hjust = 0,
            nudge_y = 20) +
  labs(y = "km2",
       x = "") +
  coord_flip() +
  theme_bw()
  

# aggregate by municipality -----------------------------------------------

# this dataset stores the surface dedicated to maize production
# by municipality
lom_mais_shape <- 
  lom_agri %>% 
  reduce(rbind) %>% 
  filter(str_detect(UTILIZZO, pattern = "^MAIS")) %>% # pull(UTILIZZO) %>% unique()
  group_by(COMUNE, UTILIZZO) %>%
  summarize(sup_used = sum(SUP_UTILIZZATA))

# This object is heavy!
rm(lom_agri)


# Get Shapefile for municipalities ----------------------------------------

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

lombardy_shapes <- 
  paste0(municip_path,
         "/Limiti_2016_WGS84_g/Com2016_WGS84_g/",
         "Com2016_WGS84_g.shp") %>% 
  sf::st_read() %>% 
  # the regional code for Lombardia is 3
  filter(COD_REG == 3) %>% 
  # Need municipality names in upper case
  # for the joining
  mutate(COMUNE = COMUNE %>%
           as.character() %>% 
           toupper(),
         # This is for plot compatibility with openstreetmap
         geometry = geometry %>% st_transform("+init=epsg:4326"))



# Merge shape and use ----------------------------------------------------

lom_mais_shape <- 
  lom_mais_shape %>% 
  group_by(COMUNE) %>% 
  summarise(sup_used = sum(sup_used)) %>% 
  # full_join(lom_istat, by = "COMUNE") %>%
  full_join(lombardy_shapes, by = "COMUNE") %>%
  # In map, better plot densities
  mutate(mais_dens = sup_used/SHAPE_Area) %>%
  # exclude municipalities that did't match
  # if any
  filter(!geometry %>% map_lgl(is.null)) %>% 
  # estimate area in km2 from shape
  # the area in the dataset have no units
  # I'm using this column to guess that they are
  # in square meters
  mutate(shape_area_km2 = geometry %>%
           sf::st_area() %>% 
           units::set_units(value = km^2)) %>% 
  distinct(COMUNE, .keep_all = TRUE) %>%
  as.data.frame()


# plot shapes -------------------------------------------------------------

# not sure
lom_mais_shape %>% 
  ggplot() +
  geom_sf() +
  coord_sf()


# leaflet -----------------------------------------------------------------

# Tokyo Palette, It has green colors,
# looks good for agricolture ;)
pal <-
  scico::scico(n = 100,
               palette = "tokyo",
               direction = -1) %>%
  colorNumeric(NULL,
               na.color = rgb(0, 0, 0, 0, maxColorValue = 255))#"#E0E0D2")

leaflet() %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.TonerBackground) %>%
  addPolygons(data = lom_mais_shape$geometry,
              fillColor = pal(lom_mais_shape$mais_dens),
              # some style, feel free to modify it
              stroke = TRUE, weight = 2,
              # fillColor =  "#00BB29", #"#A0CF9A", #"#46AEF9",
              color = "#5867A6", #"#D9F6FF", #"#95BBC6", #"#2D408F",
              fillOpacity = 1)

# try static --------------------------------------------------------------

lom_mais_shape %>%
  # ALL PROJECTIONS HERE!!!! 
  # https://download.osgeo.org/proj/OF90-284.pdf
  mutate(geometry = geometry %>%
           sf::st_transform(crs = "+proj=lcc +lat_1=20n +lat_2=60n")) %>% 
  ggplot() +
  geom_sf(aes(fill = sup_used),
          size = 0) +
  scale_fill_viridis_c(trans = "log10") +
  coord_sf()

