# create grid with intensity of actions of each oao

library(tidyverse)
library(sf)


# data  -------------------------------------------------------------------

# grid
grid <- RCzechia::KFME_grid("high")

# poly
poly <- st_read(here::here("data/final/oao_territory_poly.geojson"))

# Organizace
# heslar
# heslar <- read_csv(here::here("data/raw", "heslar_organizace.csv"))
# # mapping amcr_id to nazev_zkraceny
# ident_cely <- setNames(oao_platne$amcr_id, oao_platne$nazev_zkraceny)
fname <- list.files(here::here("data/input/"), pattern = "api_orgs", full.names = TRUE) |> 
  as_tibble() |> 
  mutate(
    ymd = str_extract(value, "\\d{4}-\\d{2}-\\d{2}"),
    ymd = as_date(ymd)) |> 
  arrange(desc(ymd)) |> 
  first() |> 
  pull(value)
orgs <- read_csv(fname)
ident_cely <- setNames(orgs$amcr_id, orgs$nazev_zkraceny)

# akce/projekty
# Tohle byly data ze starého exportu, nahrazeno exportem přímo z webové AMČR
# akce <- read_csv(here::here("data/processed", "pian_akce.csv"))
# proj <- read_csv(here::here("data/processed", "pian_proj.csv"))

# Nově: Web AMČR > Projekty > Vyber > Typ = záchranný, Datum zahájení od 2021-01-01, Datum ukončení od 2026-01-01 > Stáhnout CSV
# https://amcr.aiscr.cz/projekt/vyber?typ_projektu=1136&stav=2&stav=3&stav=4&stav=5&stav=6&datum_zahajeni_after=1.1.2021&datum_ukonceni_after=1.1.2021&leaflet-base-layers_90=on
proj <- read_csv(here::here("data/input/export_amcr_2026-08-17.csv")) |> 
  janitor::clean_names() |> 
  mutate(org_ident_cely = unname(orgs_amcr_id[organizace]))


# oao s platnou dohodou
oao_platne <- read_sf(here::here("app/data/oao_meta.geojson")) %>% 
  select(amcr_id, nazev_zkraceny) %>% 
  st_drop_geometry()

# # updated_gd_url <- "https://docs.google.com/spreadsheets/d/1RXXRGpgkrgtBhF9taEtCVuHxVIJcbeATF9RBxBtORJY/edit?usp=sharing"
# # gd_updated <- drive_get(updated_gd_url)

# Katastry
ku <- RCzechia::katastry() |> 
  st_centroid(of_largest_polygon = TRUE)


# HOTFIX!!! ---------------------------------------------------------------
# check names exist
# unique(akce$organizace)[!unique(akce$organizace) %in% names(ident_cely)]
# akce <- akce %>% 
#   mutate(organizace = case_when(
#     organizace == "Archeologický ústav Praha" ~ "Archeologický ústav AV ČR, Praha",
#     str_detect(organizace, "NPÚ") ~ "NPÚ Generální ředitelství",
#     str_detect(organizace, "Archeologický ústav Brno") ~ "Archeologický ústav AV ČR, Brno",
#     organizace == "ARCHEO Sever" ~ "Archeo Sever",
#     .default = organizace
#   ))

unique(proj$organizace[!proj$organizace %in% oao_platne$nazev_zkraceny])
# unique(proj$organizace)[!unique(proj$org_ident_cely) %in% oao_platne$amcr_id]
proj <- proj %>% 
  filter(organizace != "[neuvedeno]") |> 
  mutate(organizace = case_when(
    organizace == "MU Brno - Přírodovědecká fakulta" ~ "MU Brno - Filozofická fakulta",
    str_detect(organizace, "NPÚ") ~ "NPÚ Generální ředitelství",
    str_detect(organizace, "Archeologický ústav AV ČR, Brno") ~ "Archeologický ústav AV ČR, Brno",
    # organizace == "ARCHEO Sever" ~ "Archeo Sever",
    .default = organizace
  ))
unique(proj$organizace[!proj$organizace %in% oao_platne$nazev_zkraceny])

# Filter out OAO bez platné licence/dohody
proj <- proj |> 
  filter(organizace %in% oao_platne$nazev_zkraceny) |> 
  mutate(amcr_id = unname(ident_cely[organizace])) |> 
  left_join(select(orgs, amcr_id, ico), join_by("amcr_id"))

# data prep ---------------------------------------------------------------

# map names to ico
# akce_clean <- akce %>% 
#   select(ident = ident_cely,
#          nazev_zkraceny = organizace,
#          # typ = hlavni_typ,
#          # pristup = pristupnost,
#          # negativni = negativni_jednotka,
#          # datum = datum_ukonceni_v,
#          # dj,
#          # pian,
#          x = centroid_e,
#          y = centroid_n) %>% 
#   mutate(
#     amcr_id = unname(ident_cely[nazev_zkraceny])
#     # nazev_zkraceny = if_else(str_detect(nazev_zkraceny, "MU Brno"), 
#     #                          "MU Brno", nazev_zkraceny),
#     # nazev_zkraceny = if_else(str_detect(nazev_zkraceny, "NPÚ"), 
#     #                          "NPÚ generální ředitelství", nazev_zkraceny),
#     # nazev_zkraceny = if_else(str_detect(nazev_zkraceny, "Archeologický ústav Brno"), 
#     #                          "Archeologický ústav Brno", nazev_zkraceny)
#   ) %>% 
#   filter(!is.na(amcr_id)) %>% 
#   select(-nazev_zkraceny)

# proj_clean <- proj %>% 
  # filter(!is.na(organizace_prihlaseni)) %>% 
  # select(ident = ident_cely,
  #        nazev_zkraceny = organizace_prihlaseni,
  #        # typ = hlavni_typ,
  #        # pristup = pristupnost,
  #        # negativni = negativni_jednotka,
  #        # datum = datum_ukonceni_v,
  #        # dj,
  #        # pian,
  #        x = geometry_e,
  #        y = geometry_n) %>% 
  # mutate(amcr_id = unname(ident_cely[nazev_zkraceny])) %>% 
  # filter(!is.na(amcr_id)) %>% 
  # select(-nazev_zkraceny)

proj <- proj |> 
  select(amcr_id, hlavni_katastr, dalsi_katastry, organizace, ico) |> 
  mutate(main = str_extract(hlavni_katastr, "\\d{6}"),
    # other = str_extract_all(dalsi_katastry, "\\d{6}")
  )

# pian_clean <- akce_clean %>% bind_rows(proj_clean)

# check if all oao present
proj$ico %in% orgs$ico %>% all()

# pian_clean$ico[!pian_clean$ico %in% ico]

# oao without NO information in amcr
oao_platne[!oao_platne$amcr_id %in% proj$amcr_id, ] %>% as_tibble()

# sf ----------------------------------------------------------------------

# pian_sf <- pian_clean %>% 
#   select(-ident) %>% 
#   st_as_sf(coords = c("x", "y")) %>% 
#   st_set_crs(4326)

proj_sf <- proj |> 
  left_join(select(ku, KOD, geometry), join_by("main" == "KOD")) |> 
  st_as_sf()


# remove pian outside polygon --------------------

id_seq <- ident_cely %>% factor() %>% levels()

res <- vector("list", length(id_seq)) %>% setNames(id_seq)

for (i in seq_along(id_seq)) {
  poly_i <- poly %>% 
    filter(amcr_id == id_seq[i])
  pian_i <- proj_sf %>% 
    filter(amcr_id == id_seq[i])
  
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
  group_by(amcr_id) %>% 
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
  mutate(grid = purrr::map(data, contained_in_grid, grid)) %>% 
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
