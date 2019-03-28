library(tidyverse)
library(sf)
library(leaflet)
library(rlang)

agr_url <- paste0("https://www.politicheagricole.it/",
                  "opendata/files/2017-12-01T095029/",
                  "superfici_coltivate_2011_2013_csv.zip")
agr_path <- "data/regions-italy-production.Rdata"


if(!file.exists(agr_path)) {
  temp <- tempfile()
  temp2 <- tempfile()
  download.file(agr_url,
                destfile = temp)
  
  unzip(zipfile = temp, exdir = temp2)
  
  list.files(temp2, full.names = T)
  agr_dat <- read_csv(list.files(temp2, full.names = T))
  
  save(agr_dat, file = agr_path)
} else {
  load(agr_path)
}


# explore -----------------------------------------------------------------

agr_dat %>% 
  filter(str_detect(Coltura, "[Mm]ais") |
           str_detect(Coltura, "[Gg]ranoturco")) %>% 
  ggplot(aes(x = anno,
             y = Valore)) +
  geom_point() +
  facet_grid(Coltura ~ AreaGeografica, scales = "free") +
  scale_y_log10()

# better
agr_dat %>% 
  filter(str_detect(Coltura, "[Mm]ais") |
           str_detect(Coltura, "[Gg]ranoturco")) %>% 
  filter(!str_detect(Coltura, "totale"),
         AreaGeografica != "ITALIA") %>% 
  ggplot(aes(x = anno,
             y = reorder(AreaGeografica, Valore),
             fill = Valore)) +
  geom_tile() + 
  facet_grid(. ~ Coltura,
             labeller = label_wrap_gen(width = 20)) +
  scale_fill_viridis_c(trans = "log10")+
  labs(x = "",
       y = "",
       fill = "Ettari?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = .5))
  


# Get Shapefile for regions ----------------------------------------

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

reg_ita <- 
  paste0("data/istat_municipalities/Limiti_2016_WGS84_g/",
         "Reg2016_WGS84_g/Reg_2016_WGS84_g.shp") %>% 
  sf::st_read()



# merge -------------------------------------------------------------------

agr_dat2 <- 
  agr_dat %>% 
  rename(REGIONE = "AreaGeografica") %>% 
  filter(str_detect(Coltura, "[Mm]ais") |
           str_detect(Coltura, "[Gg]ranoturco")) %>% 
  filter(!str_detect(Coltura, "totale"),
         REGIONE != "ITALIA") %>%
  filter(anno == max(anno))
  

prod_ita <- 
  reg_ita %>% 
  mutate(REGIONE = REGIONE %>% as.character() %>% toupper(),
         REGIONE = REGIONE %>% str_replace_all("-", " ")) %>% 
  left_join(agr_dat2)

# plot shapes -------------------------------------------------------------

library(ggspatial)

prod_ita %>%
  filter(str_detect(Coltura, "granella")) %>% 
  ggplot() +
  geom_sf(aes(fill = Valore)) +
  scale_fill_viridis_c(trans = "log") +
  theme_bw()

