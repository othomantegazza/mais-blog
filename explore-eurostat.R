library(tidyverse)
library(eurostat)
library(sf)
library(lubridate)


# Maize area in europe ----------------------------------------------------

m_info <- 
  search_eurostat("maize")

# get first table
maize_eu <- 
  m_info %>% 
  pull(code) %>% .[1] %>%  
  # get the data
  get_eurostat() %>%
  # use descriptive labels
  label_eurostat() %>% 
  # fix names
  mutate(geo = as.character(geo),
         geo = case_when(str_detect(geo , "Germany") ~ "Germany",
                         TRUE ~ geo))

maize_eu$croparea %>% unique()
maize_eu$indic_ef %>% unique()
maize_eu$agrarea %>% unique()

# Keep only maize records
maize_eu <- 
  maize_eu %>% 
  filter(indic_ef == "ha: Grain maize")

# explore
maize_eu %>% 
  filter(croparea == "Total") %>% 
  # should I sum these values?
  group_by(geo, time) %>% 
  summarise(values = sum(values)) %>% 
  ggplot(aes(x = values,
             y = reorder(geo, values))) +
  geom_point(colour = "#3752C3DD") +
  # geom_boxplot() +
  # scale_x_log10() +
  facet_grid(. ~ time) +
  labs(x = "hectares [ha]",
       y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 270,
                                   vjust = .5))


maize_eu %>% 
  filter(croparea == "Total") %>% 
  # should I sum these values?
  group_by(geo, time) %>% 
  summarise(values = sum(values)) %>% 
  ggplot(aes(x = time,
             y = values)) +
  geom_point(alpha = .3) +
  facet_wrap(facets = "geo") +
  theme_bw()

maize_eu %>% 
  filter(croparea == "Total") %>% 
  # should I sum these values?
  group_by(geo, time) %>% 
  summarise(values = sum(values)) %>%
  ggplot(aes(x = time %>% as.character %>% as_factor(),
             y = geo,
             fill = values)) +
  geom_tile()



# Maize production (corn?) ------------------------------------------------

# how to use eurostat systematically?

toc <- get_eurostat_toc()

toc %>% 
  # t_apro_cp --- Table Agricultural Production 
  filter(str_detect(code, "t_apro_cp")) 
# nothing, folder

# brutal copy and paste from t_agro_cp at https://ec.europa.eu/eurostat/web/agriculture/data/main-tables
 
# Utilised agricultural area by categories (tag00025) 	 
# Cereals for the production of grain (including seed) by area, production and humidity (tag00027) 	 
# Wheat and spelt by area, production and humidity (tag00047) 	 
# Rye and winter cereal mixtures by area, production and humidity (tag00049) 	 
# Barley by area, production and humidity (tag00051) 	 
# Oats and spring cereal mixtures by area, production and humidity (tag00053) 	 
# Grain maize and corn-cob-mix by area, production and humidity (tag00093) 	 
# Dry pulses and protein crops for the production of grain (including seed and mixtures of cereals and pulses) by area, production and humidity (tag00094) 	 
# Rape, turnip rape, sunflower seeds and soya by area (tag00100) 	 
# Green maize by area, production and humidity (tag00101) 	 
# Root crops and plants harvested green from arable land by area (tag00103) 	 
# Fresh vegetables and strawberries by area (tag00115) 	 
# Permanent crops for human consumption by area (tag00120) 	 
# Grapes by production (tag00121) 	 
# Olives by production (tag00122) 	

maize_code <- 
  toc %>% 
  filter(str_detect(code, "tag00093")) %>% 
  pull(code)

maize_prod <- 
  maize_code %>% 
  get_eurostat() %>% 
  mutate_if(.predicate = is.factor, as.character)



# get euro spatial --------------------------------------------------------

# NUTS --------------------------------------------------------------------

# sp_link <- "https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2016-60m.shp.zip"
# sp_dest <- "data/eurostat_shp"
# 
# temp <- tempfile()
# 
# download.file(sp_link, destfile = temp)
# 
# unzip(temp, exdir = sp_dest)
# 
# list.files(sp_dest)
# 
# temp2 <- tempfile()
# 
# unzip("data/eurostat_shp/NUTS_BN_60M_2016_3035_LEVL_0.shp.zip",
#       exdir = "data/eurostat_shp/NUTS_BN_60M_2016_3035_LEVL_0")
# 
# 
# 
# unzip("data/eurostat_shp/NUTS_BN_60M_2016_3035_LEVL_1.shp.zip",
#       exdir = "data/eurostat_shp/NUTS_BN_60M_2016_3035_LEVL_1")
# 
# euro_spatial <- sf::st_read(paste0("data/eurostat_shp/NUTS_BN_60M_2016_3035_LEVL_1/",
#                                    "NUTS_BN_60M_2016_3035_LEVL_1.shp"))
# 
# euro_spatial %>% label_eurostat_vars()

# euro_spatial <- 
#   get_eurostat_geospatial(output_class = "sf simple features", nuts_level = 0)
  # as_tibble() %>% 
  # # country level
  # filter(LEVL_CODE == 0) %>% 
  # select(geo, geometry) 


# Countries ---------------------------------------------------------------

temp <- tempfile()

download.file("https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/ref-countries-2016-60m.shp.zip",
              destfile = temp)

temp2 <- tempfile()

unzip(temp, exdir = temp2)
list.files(temp2)

unzip(temp, exdir = "data/ref-countries-2016-60m")

unzip("data/ref-countries-2016-60m/CNTR_BN_60M_2016_3035.shp.zip",
      exdir = "data/ref-countries-2016-60m/CNTR_BN_60M_2016_3035")

eu_c_sf <- sf::read_sf("data/ref-countries-2016-60m/CNTR_BN_60M_2016_3035/CNTR_BN_60M_2016_3035.shp")

eu_c_sf %>% 
  ggplot() +
  geom_sf()

# plots -------------------------------------------------------------------

maize_prod2 <- 
  maize_prod %>% 
  left_join(euro_spatial, by = c("geo" = "geo"))
  label_eurostat()

maize_prod$strucpro %>% table()

# Area (cultivation/harvested/production) (1000 ha) 
# 424 
# EU standard humidity (%) 
# 426 
# Harvested production in EU standard humidity (1000 t) 
# 426 


# explore production ------------------------------------------------------

maize_prod %>% 
  label_eurostat() %>% 
  filter(str_detect(strucpro, "Harvested"),
         !str_detect(geo, "European Union")) %>% 
  mutate(geo = as.character(geo),
         geo = case_when(str_detect(geo, "Germany") ~ "Germany",
                         str_detect(geo, "Kosovo") ~ "Kosovo",
                         TRUE ~ geo)) %>% 
  ggplot(aes(x = values,
             y = reorder(geo, values))) +
  geom_point() +
  labs(x = "Harvested production in\nEU standard humidity\n(1000 t)",
       y = "") +
  theme(axis.title = element_text(hjust = 1)) +
  facet_grid(. ~ time)

# scatterplot -------------------------------------------------------------

to_plot <- 
  maize_prod %>% 
  label_eurostat() %>% 
  # filter(time == "2018-01-01") %>% 
  filter(!str_detect(geo, "European Union")) %>% 
  mutate(geo = as.character(geo),
         geo = case_when(str_detect(geo, "Germany") ~ "Germany",
                         str_detect(geo, "Kosovo") ~ "Kosovo",
                         TRUE ~ geo)) %>% 
  spread(strucpro, values) %>% 
  janitor::clean_names() %>% 
  # 2019 is incomplete
  filter(time != "2019-01-01")
  
to_plot %>% 
  ggplot(aes(x = area_cultivation_harvested_production_1000_ha,
             y = harvested_production_in_eu_standard_humidity_1000_t,
             colour = geo,
             size = time,
             alpha = time)) +
  geom_point() 
  scale_y_log10() +
  scale_x_log10()
 
top_10_18 <-
  to_plot %>% 
  filter(time == "2018-01-01") %>% 
  top_n(10, harvested_production_in_eu_standard_humidity_1000_t) %>% 
  pull(geo)

to_plot %>% 
  filter(geo %in% top_10_18) %>%
  ggplot(aes(x = area_cultivation_harvested_production_1000_ha,
             y = harvested_production_in_eu_standard_humidity_1000_t,
             colour = geo,
             size = time,
             alpha = time,
             group = geo)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  scale_x_log10()
  
y5 <- to_plot$time %>%  unique() %>% sort() %>% tail(5)
ylast <- to_plot$time %>%  unique() %>% sort() %>% tail(1)

trail <- 
  to_plot %>% 
  # select top 10
  filter(geo %in% top_10_18) %>%
  # this name is too long
  mutate(prod_kton = harvested_production_in_eu_standard_humidity_1000_t,
         area_kha = area_cultivation_harvested_production_1000_ha) %>%
  group_by(geo) 

trail_prod <- 
  list(points_10 = trail %>% summarise(med_10 = median(prod_kton)),
       points_5 = trail %>% filter(time %in% y5) %>% summarise(med_5 = median(prod_kton)),
       points_1 = trail %>% filter(time == ylast) %>% summarise(var_1 = prod_kton)) %>% 
  reduce(full_join) %>% 
  gather(med_10:var_1, key = "key", value = "med_prod") 
  

trail_area <- 
  list(points_10 = trail %>% summarise(med_10 = median(area_kha)),
       points_5 = trail %>% filter(time %in% y5) %>% summarise(med_5 = median(area_kha)),
       points_1 = trail %>% filter(time == ylast) %>% summarise(var_1 = area_kha)) %>% 
  reduce(full_join) %>% 
  gather(med_10:var_1, key = "key", value = "med_area")


trail_full <- 
  full_join(trail_prod, trail_area) %>% 
  mutate(key = case_when(key == "med_10" ~ 1,
                         key == "med_5" ~ 2,
                         key == "var_1" ~ 3))

trail_full %>% 
  ggplot(aes(x = med_area,
             y = med_prod,
             colour = geo,
             group = geo)) +
  geom_point(aes(size = key,
                 alpha = key)) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10()


# what about shapes? ------------------------------------------------------

  
