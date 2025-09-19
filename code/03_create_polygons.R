# This script reads updated data from GD and creates polygons for oao

library(tidyverse)


# funs --------------------------------------------------------------------

separate2longer <- function(x, col, ncol) {
  x %>% separate(sym(col), into = str_c("c", seq(1, ncol, 1)), sep = ";") %>% 
    pivot_longer(starts_with("c"), values_drop_na = TRUE) %>% 
    select(-name) %>% 
    mutate(value = str_trim(value, side = "both"))
}

add_polygon <- function(x) {
  x %>% select(amcr_id, geometry) %>% 
    group_by(amcr_id) %>% 
    nest() %>% 
    mutate(data = map(data, sf::st_as_sf),
           data = map(data, sf::st_union)) %>% 
    unnest(data)
}


# data --------------------------------------------------------------------

url <- "https://docs.google.com/spreadsheets/d/1knxDiUuCVqwgzgQkodhGg0vMe6w6LsKiGVqrsvi5dpw/edit#gid=0"

# bg data
kraje <- RCzechia::kraje()
okresy <- RCzechia::okresy()

# find file ku
unz_files <- list.files(here::here("data/input/ruian"), recursive = TRUE, full.names = TRUE)
p_ku <- unz_files[stringr::str_detect(unz_files, "KAT.+shp")]
katastry <- sf::st_read(p_ku)


# uzemni pusobnost --------------------------------------------------------

oao_uzemi <- googlesheets4::read_sheet(url, sheet = "oao_webapp") %>% 
  select(amcr_id, app, nazev_zkraceny, starts_with("is"), kraj, okres, katastr) %>% 
  filter(app) %>% 
  select(-app)

# oao_uzemi <- read_sheet(gd_updated, sheet = "Uzemi_pracovni") %>% 
#   filter(nazev_zkraceny %in% oao_platne) %>% 
#   select(-c_m, -uzemi, -is_praha)

# republika
oao_republika <- oao_uzemi %>% 
  filter(is_rep) %>% 
  select(amcr_id) %>% 
  bind_cols(RCzechia::republika()) %>% 
  select(amcr_id, data = geometry) %>% 
  sf::st_as_sf()

# kraje
oao_kraje <- oao_uzemi %>% 
  filter(is_kraj) %>% 
  select(amcr_id, kraj) %>% 
  separate2longer("kraj", 10) %>% 
  left_join(kraje, by = c("value" = "NAZ_CZNUTS3")) %>% 
  add_polygon() %>% 
  sf::st_as_sf()

# okresy
oao_okresy <- oao_uzemi %>% 
  filter(is_okres) %>% 
  select(amcr_id, okres) %>% 
  separate2longer("okres", 30) %>% 
  left_join(okresy, by = c("value" = "NAZ_LAU1")) %>% 
  add_polygon() %>% 
  sf::st_as_sf()

# write checks to control whether all values exist!!!
# x <- oao_uzemi %>% 
#   filter(is_katastr) %>% 
#   mutate(x = strsplit(katastr, split = "; ")) %>% 
#   pull(x) %>% 
#   unlist() %>% 
#   unique()
# table(x %in% katastry$NAZEV_KU)
# x[!x %in% katastry$NAZEV_KU]

# katastry
oao_katastry <- oao_uzemi %>% 
  filter(is_katastr) %>% 
  select(amcr_id, katastr) %>% 
  separate2longer("katastr", 400) %>% 
  left_join(katastry, by = c("value" = "NAZEV")) %>% 
  add_polygon() %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(4326)

# result
oao_uzemi_poly <- oao_republika %>% 
  bind_rows(oao_kraje) %>% 
  bind_rows(oao_okresy) %>% 
  bind_rows(oao_katastry) %>% 
  group_by(amcr_id) %>% 
  nest() %>% 
  mutate(
    data = map(data, sf::st_as_sf),
    data = map(data, sf::st_make_valid),
    data = map(data, sf::st_union),
    data = map(data, nngeo::st_remove_holes, max_area = 1e4)
  ) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  sf::st_as_sf()

# valid geometry?
sf::st_is_valid(oao_uzemi_poly) %>% all()

# missing polygons
all(oao_uzemi$amcr_id %in% oao_uzemi_poly$amcr_id)


# total covered area (arranging in tables) --------------------------------

oao_uzemi_poly <- oao_uzemi_poly %>% 
  mutate(area = as.numeric(sf::st_area(oao_uzemi_poly)))

# export resulting polygons -----------------------------------------------

if (file.exists(here::here("data/final", "oao_territory_poly.geojson"))) {
  file.remove(here::here("data/final", "oao_territory_poly.geojson"))
}

sf::st_write(oao_uzemi_poly, 
             here::here("data/final", "oao_territory_poly.geojson"))


# simplify polygons for the web use ---------------------------------------

oao_uzemi_poly_simple <- sf::st_simplify(oao_uzemi_poly, 
                                         dTolerance = 150, 
                                         preserveTopology = TRUE) %>% 
  nngeo::st_remove_holes(max_area = 1e6)

if (file.exists(here::here("data/final", "oao_territory_poly_simple.geojson"))) {
  file.remove(here::here("data/final", "oao_territory_poly_simple.geojson"))
}

sf::st_write(oao_uzemi_poly_simple, 
             here::here("data/final", "oao_territory_poly_simple.geojson"))


# update file for app -----------------------------------------------------

file.copy(here::here("data/final/oao_territory_poly_simple.geojson"),
          here::here("app/data/oao_scope.geojson"), overwrite = TRUE)



# playground --------------------------------------------------------------

# oao_uzemi_poly_simple <- sf::st_read(here::here("data/final", "oao_territory_poly_simple.geojson"))

# oao_katastry %>%
#   ggplot() +
#     geom_sf() +
#     facet_wrap(~ico)
#
# oao_uzemi_poly_simple %>%
#   filter(ico == "00065048") %>%
#   leaflet::leaflet() %>%
#   leaflet::addTiles() %>%
#   leaflet::addPolygons()
#
# oao_uzemi_poly %>%
#   filter(nazev_zkraceny == "Muzeum Vrchlabí") %>%
#   leaflet::leaflet() %>%
#   leaflet::addTiles() %>%
#   leaflet::addPolygons()
#
# # Searches through katastr names and plots their position.
# katastry[str_detect(katastry$NAZ_KU, "Střítež"), ] %>%
#   ggplot() +
#   geom_sf(data = RCzechia::republika()) +
#   geom_sf() +
#   geom_sf_text(aes(label = NAZ_KU)) +
#   theme_void()
#
# oao_republika %>%
#   sf::st_as_sf() %>%
#   ggplot() +
#   geom_sf() +
#   facet_wrap(~nazev_zkraceny)
