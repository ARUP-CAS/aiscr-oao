# download data from cuzk and create centroids for KU lookup

library(sf)


# functions ---------------------------------------------------------------

check_dirs <- function(x) {
  if (!dir.exists(x)) {
    dir.create(x)
  }
}

st_overwrite <- function(x, path) {
  if (file.exists(path)) {
    file.remove(path)
  }
  
  sf::st_write(x, path)
}


# data --------------------------------------------------------------------

url <- "https://services.cuzk.cz/shp/stat/epsg-5514/1.zip"

# paths
p_data <- here::here("data")
p_in <- paste0(p_data, "/input")
p_proc <- paste0(p_data, "/processed")
p_fin <- paste0(p_data, "/final")
p_zip <- paste0(p_in, "/ruian.zip")
p_unz <- paste0(p_in, "/ruian/")
p_ku_out <- here::here("app/data/ku.geojson")

# dirs
check_dirs(p_data)
check_dirs(p_in)
check_dirs(p_proc)
check_dirs(p_fin)

# dl
# options(timeout = 100)
download.file(url, p_zip)

# unz
unzip(p_zip, exdir = p_unz, overwrite = TRUE)

# find file
unz_files <- list.files(p_unz, recursive = TRUE)
p_ku_in <- paste0(p_unz, unz_files[stringr::str_detect(unz_files, "KAT.+shp")])

# okr
okr <- RCzechia::okresy() %>% 
  st_drop_geometry() %>% 
  dplyr::select(KOD_LAU1, NAZ_LAU1)


# prep ku layer -----------------------------------------------------------

ku <- st_read(p_ku_in)


# centroids ---------------------------------------------------------------

ku_centroids <- ku %>% 
  dplyr::select(NAZEV, LAU1_KOD) %>% 
  dplyr::left_join(okr, by = c("LAU1_KOD" = "KOD_LAU1")) %>% 
  st_centroid() %>%
  st_transform(4326) %>% 
  st_simplify() %>% 
  dplyr::select(ku = NAZEV, okr = NAZ_LAU1)

# output ------------------------------------------------------------------

ku_centroids %>% 
  st_overwrite(p_ku_out)

