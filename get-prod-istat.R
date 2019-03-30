library(tidyverse)
library(readxl)



# Download data -----------------------------------------------------------

istat_path <- "data/mais-prod-istat.Rdata"

#  they do not provide https
istat_url <- "http://agri.istat.it/excel/dwplC02aa00000100000130.xls"

# la superficie è in ettari e la produzione è in quintali.

# http://agri.istat.it/excel/dwplC02aa00000100000130.xls


if(!file.exists(istat_path)) {
 keep <- "_" %>% paste0(6:9) %>%   paste(collapse = "|")

 mais_istat <- 
   read_excel(istat_url,
              skip = 2) %>% 
   janitor::clean_names() %>% 
   select(x1, matches(keep)) %>% 
   rename(provincia = "x1") %>% 
   rename_all(~str_remove_all(., "_[6789]"))
 
 save(mais_istat, file = istat_path)
} else {
  load(istat_path)
}

