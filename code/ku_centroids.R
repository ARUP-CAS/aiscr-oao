# download data from cuzk and create centroids

library(sf)


# functions ---------------------------------------------------------------

st_overwrite <- function(x, path) {
  if (file.exists(path)) {
    file.remove(path)
  }
  
  sf::st_write(x, path)
}


# data --------------------------------------------------------------------

link <- "https://services.cuzk.cz/shp/stat/epsg-5514/1.zip"

path_zip <- here::here("data/ruian.zip")
path_unz <- here::here("data/ruian/")
path_ku <- here::here("app/data/ku.geojson")

# dl
download.file(link, path_zip)

# unz
unzip(path_zip, exdir = path_unz)

# okr
okr <- RCzechia::okresy() %>% 
  st_drop_geometry() %>% 
  dplyr::select(KOD_LAU1, NAZ_LAU1)


# prep ku layer -----------------------------------------------------------

ku <- st_read(paste0(path_unz, "/1/KATUZE_P.shp")) %>% 
  st_set_crs(5514)


# centroids ---------------------------------------------------------------

centroids <- ku %>% 
  dplyr::select(NAZEV, LAU1_KOD) %>% 
  dplyr::left_join(okr, by = c("LAU1_KOD" = "KOD_LAU1")) %>% 
  st_centroid() %>%
  st_transform(4326) %>% 
  st_simplify() %>% 
  dplyr::select(ku = NAZEV, okr = NAZ_LAU1)

# output ------------------------------------------------------------------

centroids %>% 
  st_overwrite(path_ku)

