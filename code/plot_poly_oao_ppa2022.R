# create OAO plots for poster

library(tidyverse)
library(sf)
library(here)


# data --------------------------------------------

rep <- RCzechia::republika() %>% 
  st_transform(5514)

# metadata
meta <- read_sf(here("data/final/oao_meta.geojson")) %>% 
  st_drop_geometry() %>% 
  select(ico, nazev_zkraceny, nazev)

# polygons
poly <- read_sf(here("data/final/oao_territory_poly_simple.geojson")) %>% 
  st_transform(5514)

# grid
grid <- read_sf(here("data/final/oao_grid.geojson")) %>% 
  st_transform(5514)


# prep --------------------------------------------
nrow <- 5
ncol <- 6
len_nazev <- 24

poly_flt <- poly %>% 
  left_join(meta, by = "ico") %>% 
  arrange(desc(area)) %>% 
  slice(1:(nrow * ncol)) %>% 
  mutate(
    nazev = str_wrap(nazev, len_nazev)
  )

grid_flt <- grid %>% 
  left_join(meta, by = "ico") %>% 
  filter(ico %in% poly_flt$ico) %>% 
  mutate(
    nazev = str_wrap(nazev, len_nazev)
  )


# plot --------------------------------------------

poly_flt %>% 
  ggplot() +
  geom_sf(data = rep, 
          color = NA, fill = "#3E3F3A33") + # 
  geom_sf(color = NA, fill = "#3E3F3A80") + # #26A269 
  geom_sf(data = grid_flt, aes(fill = scaled), 
          color = NA, show.legend = FALSE) + 
  scale_fill_viridis_c(option = "C") +
  facet_wrap(vars(nazev), ncol = ncol) +
  theme_void() +
  theme(strip.text = element_text(family = "Museo", size = 16)) +
  labs(caption = "Data: AIS ČR, pozadí: R package RCzechia.")

ggsave(here("plot_oao_poly_ppa2022.svg"), 
       width = 574, height = 404, units = "mm")
