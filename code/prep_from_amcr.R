# joining tables exported from AMCR
# export: 
#   - projekt
#   - akce
#   - pian
#   - dokumentacni jednotka
# and save to data/input/

library(tidyverse)


# read data ---------------------------------------------------------------

datestamp <- list.files(here::here("data/input"), pattern = "^export") %>% 
  str_extract("\\d{4}-\\d{2}-\\d{2}") %>% 
  lubridate::as_date() %>% 
  max()

proj <- read_delim(here::here("data/input", 
                        paste0("export_", datestamp, "_projekt.csv")),
                   delim = "#",
                   col_types = paste0(rep("c", 48), collapse = ""),
                   quote = "") %>% 
  select(ident_cely, id_rok, stav_popis, typ_projektu, 
         organizace_prihlaseni, starts_with("geometry"))

akce <- read_delim(here::here("data/input", 
                        paste0("export_", datestamp, "_akce.csv")), 
                   delim = "#", 
                   col_types = paste0(rep("c", 40), collapse = ""), 
                   quote = "") %>% 
  select(ident_cely, organizace, hlavni_typ, pristupnost, datum_ukonceni_v)

pian <- read_delim(here::here("data/input", 
                        paste0("export_", datestamp, "_pian.csv")), 
                   delim = "#", 
                   col_types = paste0(rep("c", 13), collapse = ""), 
                   quote = "") %>% 
  select(ident_cely, geom_wkt, starts_with("centroid"))

dj <- read_delim(here::here("data/input", 
                      paste0("export_", datestamp, "_dokumentacni_jednotka.csv")), 
                 delim = "#", 
                 col_types = paste0(rep("c", 7), collapse = ""), 
                 quote = "") %>% 
  select(ident_cely, parent, pian, negativni_jednotka)


# filtering ---------------------------------------------------------------

# only last 5 years used
akce_five_y <- akce %>% 
  mutate(date = lubridate::year(lubridate::ymd(datum_ukonceni_v)),
         five_y = date >= lubridate::year(lubridate::today()) - 5) %>% 
  filter(five_y)

proj_five_y <- proj %>% 
  mutate(five_y = lubridate::year(as.Date(id_rok, format = "%Y")) >= 
           lubridate::year(lubridate::today()) - 5) %>% 
  filter(five_y)


# joining tables ----------------------------------------------------------

# akce > dok. j. > pian
akce_pian <- inner_join(akce_five_y, dj, 
                        by = c("ident_cely" = "parent"), 
                        suffix = c("akce", ".dj")) %>% 
  rename(dj = ident_cely.dj) %>% 
  inner_join(pian, 
             by = c("pian" = "ident_cely")) %>% 
  filter(!is.na(centroid_e), !is.na(centroid_n))

proj_pian <- proj_five_y %>% 
  filter(!is.na(geometry_e), !is.na(geometry_n))


# export data -------------------------------------------------------------

akce_pian %>% 
  write_csv(here::here("data/processed/", "pian_akce.csv"))

proj_pian %>% 
  write_csv(here::here("data/processed/", "pian_proj.csv"))



