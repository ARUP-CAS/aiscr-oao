# create grid with intensity of actions of each oao

library(tidyverse)
library(sf)


# data  -------------------------------------------------------------------

# grid
grid <- RCzechia::KFME_grid("high")

# poly
poly <- st_read(here::here("data/final/oao_territory_poly.geojson"))

# akce
akce <- read_csv(here::here("data/processed", "pian_akce.csv"))
proj <- read_csv(here::here("data/processed", "pian_proj.csv"))

# heslar
# heslar <- read_csv(here::here("data/raw", "heslar_organizace.csv"))

# GD data
revised_gd_url <- "https://docs.google.com/spreadsheets/d/1knxDiUuCVqwgzgQkodhGg0vMe6w6LsKiGVqrsvi5dpw/edit#gid=0"

# oao s platnou dohodou
oao_platne <- googlesheets4::read_sheet(revised_gd_url, sheet = "oao_webapp") %>%
  filter(app) %>%
  pull(ico)

# updated_gd_url <- "https://docs.google.com/spreadsheets/d/1RXXRGpgkrgtBhF9taEtCVuHxVIJcbeATF9RBxBtORJY/edit?usp=sharing"
# gd_updated <- drive_get(updated_gd_url)

# mapping ico to heslar amcr
ico_mapping <- googlesheets4::read_sheet(revised_gd_url, sheet = "oao_heslar_amcr") %>% 
  filter(ico %in% oao_platne)
ico <- setNames(ico_mapping$ico, ico_mapping$amcr_nazev_zkraceny)

# data prep ---------------------------------------------------------------

# map names to ico
akce_clean <- akce %>% 
  select(ident = ident_cely,
         nazev_zkraceny = organizace,
         # typ = hlavni_typ,
         # pristup = pristupnost,
         # negativni = negativni_jednotka,
         # datum = datum_ukonceni_v,
         # dj,
         # pian,
         x = centroid_e,
         y = centroid_n) %>% 
  mutate(
    ico = unname(ico[nazev_zkraceny])
    # nazev_zkraceny = if_else(str_detect(nazev_zkraceny, "MU Brno"), 
    #                          "MU Brno", nazev_zkraceny),
    # nazev_zkraceny = if_else(str_detect(nazev_zkraceny, "NPÚ"), 
    #                          "NPÚ generální ředitelství", nazev_zkraceny),
    # nazev_zkraceny = if_else(str_detect(nazev_zkraceny, "Archeologický ústav Brno"), 
    #                          "Archeologický ústav Brno", nazev_zkraceny)
  ) %>% 
  filter(!is.na(ico)) %>% 
  select(-nazev_zkraceny)

proj_clean <- proj %>% 
  filter(!is.na(organizace_prihlaseni)) %>% 
  select(ident = ident_cely,
         nazev_zkraceny = organizace_prihlaseni,
         # typ = hlavni_typ,
         # pristup = pristupnost,
         # negativni = negativni_jednotka,
         # datum = datum_ukonceni_v,
         # dj,
         # pian,
         x = geometry_e,
         y = geometry_n) %>% 
  mutate(ico = unname(ico[nazev_zkraceny])) %>% 
  filter(!is.na(ico)) %>% 
  select(-nazev_zkraceny)

pian_clean <- akce_clean %>% bind_rows(proj_clean)

# check if all oao present
pian_clean$ico %in% ico %>% all()
# pian_clean$ico[!pian_clean$ico %in% ico]

# oao without NO information in amcr
ico_mapping$label[!ico %in% pian_clean$ico] %>% as_tibble()

# sf ----------------------------------------------------------------------

pian_sf <- pian_clean %>% 
  select(-ident) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(4326)


# remove pian outside polygon --------------------

ico_seq <- ico %>% factor() %>% levels()

res <- vector("list", length(ico_seq)) %>% setNames(ico_seq)

for (i in seq_along(ico_seq)) {
  poly_i <- poly %>% 
    filter(ico == ico_seq[i])
  pian_i <- pian_sf %>% 
    filter(ico == ico_seq[i])
  
  res[[i]] <- pian_i[as.vector(st_contains(poly_i, pian_i, sparse = FALSE)), ]
}

pian_valid <- res %>% bind_rows()

# x <- pian_clean %>% group_by(ico) %>% count()
# y <- pian_valid %>% st_drop_geometry() %>% group_by(ico) %>% count()
# 
# x %>% left_join(y, by = "ico") %>% 
#   mutate(fuu = n.x == n.y) %>% 
#   filter(!fuu) %>% View()

pian_nest <- pian_valid %>% 
  group_by(ico) %>% 
  nest()

# create grid -------------------------------------------------------------

#' Contained in grid
#'
#' @param x \code{sf} object of geometry type POINT
#' @param grid Grid definition.
#'
#' @return
#' @export
#'
#' @examples
contained_in_grid <- function(x, grid) {
  contains <- st_contains(grid, x)
  n <- contains[contains %>% lengths() > 0] %>% 
    lapply(length) %>% 
    lapply(as_tibble) %>% 
    bind_rows()
  grid %>% filter(contains %>% lengths() > 0) %>% 
    bind_cols(n) %>% 
    mutate(scaled = log10(value + 1))
}

grids <- pian_nest %>% 
  mutate(grid = map(data, contained_in_grid, grid)) %>% 
  select(-data) %>% 
  unnest(grid) %>% 
  st_as_sf()


# export grid -------------------------------------------------------------

if (file.exists(here::here("data/final", "oao_grid.geojson"))) {
  file.remove(here::here("data/final", "oao_grid.geojson"))
}

st_write(grids, here::here("data/final", "oao_grid.geojson"))

file.copy(here::here("data/final/oao_grid.geojson"),
          here::here("app/data/oao_grid.geojson"), overwrite = TRUE)


# playground --------------------------------------------------------------

# uapp <- "48511005"
# uappsc <- "49276433"
# npu <- "75032333"
# 
# x <- grids %>%
#   filter(ico == uapp)
# 
# x %>%
#   ggplot() +
#   geom_sf(data = RCzechia::republika(), fill = "white") +
#   geom_sf(aes(fill = scaled), alpha = 0.8, color = NA) +
#   geom_sf_text(aes(label = value)) +
#   scale_fill_gradient(low = "#E5F5E0", high = "#31A354") +
#   theme_void()
# 
# # leaflet map
# pal <- leaflet::colorNumeric(palette = "YlGnBu", domain = x$scaled)
# # YlGn YlGnBu Greens Blues
# 
# x %>%
#   leaflet::leaflet() %>%
#   leaflet::addTiles() %>%
#   leaflet::addProviderTiles(provider = leaflet::providers$CartoDB.Positron) %>%
#   leaflet::addPolygons(stroke = F, smoothFactor = 0.2,
#                        color = ~pal(scaled), fillOpacity = 0.6)
