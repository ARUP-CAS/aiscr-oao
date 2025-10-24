library(sf)
library(tidyverse)
# library(units)

cap <- function(x = NA_character_) {
  if (is.na(x)) {
    paste0("Data: AIS CR (", today(), ")")
  } else {
    paste0("Data: AIS CR, ", x, " (", today(), ")")
  }
}


# data --------------------------------------------------------------------

## hexgrid ----------------------------------------------------------------

republika <- RCzechia::republika() %>% 
  st_transform(5514)

hex <- st_make_grid(republika, square = FALSE, cellsize = 5e3) %>% 
  st_as_sf() %>% 
  st_filter(republika)

# ggplot() +
#   geom_sf(data = republika) +
#   geom_sf(data = hex, fill = NA) +
#   ggspatial::annotation_scale() +
#   ggspatial::annotation_north_arrow(style = ggspatial::north_arrow_minimal())


# oao ---------------------------------------------------------------------

oao_poly <- st_read(here::here("data/final/oao_territory_poly_simple.geojson")) %>% 
  st_transform(5514)

hex_oao <- hex %>% 
  mutate(oao = lengths(st_intersects(st_centroid(hex), oao_poly))) %>% 
  filter(oao > 0)

hex_oao_union <- hex_oao %>% 
  group_by(oao) %>% 
  nest() %>% 
  mutate(data = map(data, \(x) st_union(x))) %>% 
  unnest(data) %>% 
  st_as_sf()

hex_oao_union %>% 
  mutate(oao = factor(oao)) %>% 
  ggplot() +
  aes(fill = oao) +
  geom_sf(color = "black") +
  scale_fill_viridis_d(direction = -1) +
  theme_minimal() +
  labs(fill = "Počet OAO", caption = cap()) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

ggsave(here::here("plots/oao_map.png"), width = 14, height = 8)


