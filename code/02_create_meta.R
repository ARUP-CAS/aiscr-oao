# This script reads updated data from GD and creates metadata table

library(tidyverse)
# library(googledrive)
library(googlesheets4)


# paths -------------------------------------------------------------------

dir_data <- here::here("data")
dir_fin <- paste0(dir_data, "/final")


# data from GD ------------------------------------------------------------

revised_gd_url <- "https://docs.google.com/spreadsheets/d/1knxDiUuCVqwgzgQkodhGg0vMe6w6LsKiGVqrsvi5dpw/edit#gid=0"

# updated_gd_url <- "https://docs.google.com/spreadsheets/d/1RXXRGpgkrgtBhF9taEtCVuHxVIJcbeATF9RBxBtORJY/edit?usp=sharing"
# gd_updated <- drive_get(updated_gd_url)

# original_gd_url <- "https://docs.google.com/spreadsheets/d/1oeUfIfMG2nA5vDvxYRnvWuKtUUEuO-SxKLEB9f05lW8/edit#gid=1589848804"
# gd_original <- drive_get(original_gd_url)


# revised ----------------------------------------

oao_gd <- read_sheet(revised_gd_url, sheet = "oao_webapp") %>% 
  # remove space from PSČ to geocode addresses properly
  mutate(adresa = str_remove(adresa, "(?<=\\d{3})\\s(?=\\d{2})")) %>% 
  filter(app)

# updated -----------------------------------------------------------------

# # oao s platnou dohodou
# oao_platne <- read_sheet(gd_updated, sheet = "Dohody_pracovni") %>% 
#   filter(dohoda_platna == 1) %>% 
#   pull(nazev_zkraceny)
# 
# oao_dohoda <- read_sheet(gd_updated, sheet = "Dohody_pracovni", ) %>% 
#   filter(nazev_zkraceny %in% oao_platne)
# 
# oao_kontakt <- read_sheet(gd_updated, sheet = "Kontakty_pracovni") %>% 
#   filter(nazev_zkraceny %in% oao_platne) %>% 
#   mutate(adresa = str_remove(adresa, "(?<=\\d{3})\\s(?=\\d{2})"))
# 
# oao_uzemi <- read_sheet(gd_updated, sheet = "Uzemi_pracovni") %>% 
#   filter(nazev_zkraceny %in% oao_platne) %>% 
#   select(-c_m, -uzemi, -is_praha)

# oao_platne %in% oao_kontakt$nazev_zkraceny %>% table()
# oao_platne[!oao_platne %in% oao_kontakt$nazev_zkraceny]
# oao_dohoda$nazev_zkraceny[oao_dohoda$nazev_zkraceny %>% duplicated()]

# # original ----------------------------------------------------------------
# 
# read_sheet(gd_original, sheet = "")


# geocode address ---------------------------------------------------------

address <- oao_gd %>%
  pull(adresa) %>%
  RCzechia::geocode()

# oao_gd %>% 
#   select(nazev_zkraceny, adresa) %>% 
#   left_join(address, by = c(adresa = "address")) %>% 
#   View()

# oao_address <- oao_kontakt %>%
#   pull(adresa) %>%
#   RCzechia::geocode() %>%
#   left_join(select(oao_kontakt, nazev_zkraceny, adresa),
#             by = c("address" = "adresa")) %>%
#   select(-type)

# oao_kontakt %>%
#   left_join(oao_address, by = c("adresa" = "address")) %>%
#   filter(is.na(result)) %>%
#   select(nazev_zkraceny, adresa, result, ico) %>% View()


# check ARES names ---------------------------------------------------------

#' Get company name from ARES
#'
#' @param x IČO 
#'
#' @return Character
#' @export
#'
#' @examples
# ares_name <- function(x) {
#   Sys.sleep(0.01)
#   url_ares <- "https://wwwinfo.mfcr.cz/cgi-bin/ares/darv_std.cgi?ico="
#   res <- xml2::read_xml(paste0(url_ares, x)) %>% 
#     xml2::xml_child(".//are:Obchodni_firma") %>% 
#     xml2::xml_text()
#   return(res)
# }
# 
# oao_nazvy <- oao_gd %>% 
#   select(nazev_zkraceny, nazev, ico) %>% 
#   mutate(nazev_ares = map_chr(ico, \(x) ares_name(x)),
#          ico = str_pad(ico, width = 8, pad = "0", side = "left"))
# 
# oao_nazvy %>% write_csv(paste0(dir_data, "/names_ares.csv"))

# uzemi textem ------------------------------------------------------------

oao_out <- oao_gd %>% 
  mutate(
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
  select(ico, nazev_zkraceny, nazev, spec_text, adresa, web, mail,
         starts_with(c("mk_", "av_")), note, uzemi)

oao_out <- oao_out %>%
  left_join(address, by = join_by(adresa == address)) %>% 
  select(-type, -result) %>% 
  st_as_sf()

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
