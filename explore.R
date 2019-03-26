library(tidyverse)
library(eurostat)

# get maize production in europe
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
