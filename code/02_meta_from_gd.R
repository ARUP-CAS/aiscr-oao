# This script reads updated data from GD and creates metadata table

library(tidyverse)
# library(googledrive)
library(googlesheets4)


# paths -------------------------------------------------------------------

dir_data <- here::here("data")
dir_fin <- paste0(dir_data, "/final")


# data from GD ------------------------------------------------------------

gd_url <- "https://docs.google.com/spreadsheets/d/1knxDiUuCVqwgzgQkodhGg0vMe6w6LsKiGVqrsvi5dpw/edit#gid=0"

oao_gd <- read_sheet(gd_url, sheet = "oao_webapp") %>% 
  filter(app) %>% 
  select(
    amcr_id, label, app, web, 
    starts_with("mk"), starts_with("av"), amcr, 
    starts_with("is"), kraj, okres, katastr, note, spec_text
  )

# data from API -----------------------------------------------------------

oao_api <- list.files(paste0(dir_data, "/input/"), pattern = "org", full.names = TRUE) %>% 
  as_tibble() %>% 
  mutate(stamp = str_extract(value, "\\d{4}[-\\d{2}]*"),
         stamp = as.Date(stamp)) %>% 
  arrange(desc(stamp)) %>% 
  first() %>% 
  pull(value) %>% 
  read_csv(.)

# # data from API for GD table update
# oao_api %>%
#   arrange(nazev_zkraceny) %>%
#   select(nazev_zkraceny, nazev, amcr_id, oao) %>%
#   write_excel_csv(here::here("data/api4gd.csv"))


# join tables -------------------------------------------------------------

# oao_gd$amcr_id %in% oao_api$amcr_id %>% table()
# oao_gd[!oao_gd$amcr_id %in% oao_api$amcr_id, ]

# orgs labeled as OAO in AMCR, but not in App
# filter(oao_api, oao == TRUE) %>%
#   filter(!amcr_id %in% oao_gd$amcr_id) %>% View()
#   write_excel_csv(here::here("data/api_not_in_gd.csv"))

oao <- oao_gd %>% 
  left_join(oao_api, by = join_by("amcr_id")) %>% 
  mutate(adresa2 = str_remove(adresa, "(?<=\\d{3})\\s(?=\\d{2})")) %>% 
  arrange(nazev_zkraceny)


# checks ------------------------------------------------------------------

# # OAO bez mezery v PSČ
# oao %>%
#   select(amcr_id, nazev_zkraceny, adresa) %>%
#   mutate(psc_space = str_detect(adresa, "\\d{3}\\s\\d{2}")) %>%
#   filter(!psc_space) %>%
#   write_excel_csv(here::here("data/api_psc_missing_space.csv"))

# geocode address ---------------------------------------------------------

address <- oao %>%
  # slice(4:6, 20:22) %>% 
  pull(adresa2) %>%
  RCzechia::geocode()

# sf::st_drop_geometry(address) %>% View()

# oao %>%
#   select(amcr_id, nazev_zkraceny, adresa, adresa2) %>%
#   left_join(sf::st_drop_geometry(address), by = join_by("adresa2" == "address")) %>%
#   View()

# oao %>%
#   select(amcr_id, nazev_zkraceny, adresa, adresa2) %>%
#   left_join(sf::st_drop_geometry(address), by = join_by("adresa2" == "address")) %>% 
#   filter(is.na(result))


# check ARES names ---------------------------------------------------------

#' Get company name from ARES
#'
#' @param x IČO 
#'
#' @return Character
#' @export
#'
#' @examples
# ares_name <- function(ico) {
#   Sys.sleep(0.01)
#   url <- "https://ares.gov.cz/ekonomicke-subjekty-v-be/rest/ekonomicke-subjekty/"
#   
#   res <- jsonlite::read_json(paste0(url, ico))
#   res$obchodniJmeno
# }
# 
# oao_ares <- oao %>%
#   select(amcr_id, nazev_zkraceny, nazev, ico) %>% 
#   mutate(ico = str_pad(ico, width = 8, pad = "0", side = "left"),
#          nazev_ares = map_chr(ico, \(x) ares_name(x)),
#          ares = nazev != nazev_ares) %>% 
#   filter(ares)
# 
# oao_ares
# 
# oao_ares %>%
#   write_csv(here::here("data/api_names_ares.csv"))

# uzemi textem ------------------------------------------------------------

oao_out <- oao %>% 
  dplyr::mutate(
    # string with uzemi
    uzemi = if_else(is_rep, "Celé území ČR.", NA_character_),
    uzemi = if_else(is_kraj, paste0(kraj, "."), uzemi),
    uzemi = if_else(is_okres, paste0("Okres ", okres, "."), uzemi),
    uzemi = if_else(is_katastr, paste0("Kat. úz. ", katastr, "."), uzemi),
    uzemi = if_else(is_kraj & is_okres, paste0(kraj, " a okres ", okres, "."), uzemi),
    uzemi = if_else(is_kraj & is_katastr, paste0(kraj, " a kat. úz. ", katastr, "."), uzemi),
    uzemi = if_else(is_okres & is_katastr, paste0("Okres ", okres, " a kat. úz. ", katastr, "."), uzemi),
    uzemi = if_else(is_kraj & is_okres & is_katastr, paste0(kraj, ", okres ", okres, " a kat. úz. ", katastr, "."), uzemi),
    # proper dates
    across(ends_with(c("from", "to")), \(x) lubridate::ymd(x))
  ) %>% 
  select(amcr_id, label, ror, ico, nazev_zkraceny, nazev, spec_text, 
         adresa, adresa2, web, email, telefon, typ,
         zverejneni, pristupnost,
         starts_with(c("mk_", "av_")), amcr, note, uzemi)

oao_out <- oao_out %>%
  left_join(address, by = join_by(adresa2 == address)) %>% 
  select(-type, -result, -adresa2) %>% 
  sf::st_as_sf()

# oao_uzemi_up <- oao_uzemi %>% 
#   mutate(uzemi = if_else(is_republika, "Celé území ČR.", NA_character_),
#          uzemi = if_else(!is.na(kraj), paste0(kraj, "."), uzemi),
#          uzemi = if_else(!is.na(okres), paste0("Okres ", okres, "."), uzemi),
#          uzemi = if_else(!is.na(katastr), paste0("Kat. úz. ", katastr, "."), uzemi),
#          uzemi = str_replace_all(uzemi, ";", ",")) %>% 
#   select(nazev_zkraceny, uzemi)


# dohody a data -----------------------------------------------------------

# oao_dohoda_up <- oao_dohoda %>% 
#   select(nazev_zkraceny, web, dohoda_mk, starts_with("datum")) %>% 
#   mutate(across(starts_with("datum"), \(x) lubridate::ymd(x)))


# bind together -----------------------------------------------------------

# oao_out <- oao_nazvy %>% 
#   left_join(oao_address, by = c("nazev_zkraceny")) %>% 
#   left_join(oao_uzemi_up, by = c("nazev_zkraceny")) %>% 
#   left_join(oao_dohoda_up, by = c("nazev_zkraceny")) %>% 
#   select(-result) %>% 
#   sf::st_as_sf()


# data export -------------------------------------------------------------

if (file.exists(paste0(dir_fin, "/oao_meta.geojson"))) {
  file.remove(paste0(dir_fin, "/oao_meta.geojson"))
}

sf::write_sf(oao_out, paste0(dir_fin, "/oao_meta.geojson"))

file.copy(paste0(dir_fin, "/oao_meta.geojson"), 
          here::here("./app/data/oao_meta.geojson"), 
          overwrite = TRUE)

# playground --------------------------------------------------------------

# names to ascii
# oao_out %>% 
#   mutate(nazev_ascii = iconv(nazev_zkraceny, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
#          local_url = paste0("<a href=", "http://127.0.0.1:5774?org=", nazev_ascii, ">", "fuu", "</a>")) %>% 
#   select(local_url)
