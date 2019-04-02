library(tidyverse)
library(eurostat)


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
  get_eurostat() 


euro_spatial <- 
  get_eurostat_geospatial() 

maize_prod2 <- 
  maize_prod %>% 
  full_join(euro_spatial)
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


# what about shapes? ------------------------------------------------------

  
