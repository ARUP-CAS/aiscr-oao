#' Add the CartoDB Positron ("Desaturovaná mapa") base layer
#'
#' CARTO basemap tiles now require an API key, passed as a `?key=` query
#' parameter. The key is read from the `CARTO_API_KEY` environment variable
#' (set it in `.Renviron`, locally and on the Shiny Server). `providerTileOptions()`
#' cannot inject it because the bundled leaflet-providers CartoDB URL has no
#' `{key}` placeholder, so the tile URL is built explicitly here. When the key is
#' missing, fall back to OSM tiles so the app still renders in local development.
#'
#' @param map A leaflet map object.
#' @param group Layer group name.
#'
#' @return The leaflet map with the base layer added.
add_carto_positron <- function(map, group = "Desaturovaná mapa") {
  carto_key <- Sys.getenv("CARTO_API_KEY", unset = "")

  if (nzchar(carto_key)) {
    leaflet::addTiles(
      map,
      urlTemplate = sprintf(
        "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png?key=%s",
        carto_key),
      attribution = paste0(
        "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> ",
        "&copy; <a href='https://carto.com/attributions'>CARTO</a>"),
      options = leaflet::tileOptions(maxZoom = 20),
      group = group)
  } else {
    warning("CARTO_API_KEY not set; using OpenStreetMap tiles for '", group, "'.")
    leaflet::addTiles(map, group = group)
  }
}


#' Leaflet map, Czech Republic extent
#'
#' Creates leaflet object with extent and zoom set to the Czech Republic.
#' Tiles include CartoDB Pozitron map, basic OSM and CUZK ZM.
#'
#' @param data Data passed to leaflet.
#'
#' @return Leaflet object.
#' @export
#'
#' @examples
leaflet_czechrep <- function(data) {
  data %>% leaflet::leaflet(
    options = leaflet::leafletOptions(minZoom = 7, maxZoom = 16)) %>%
    leaflet::setView(zoom = 7, lng = 15.4730, lat = 49.8175) %>%
    leaflet::setMaxBounds(11, 48, 20, 52) %>%
    leaflet::addTiles(group = "Open Street Map") %>%
    add_carto_positron(group = "Desaturovaná mapa") %>%
    leaflet::addTiles(
      urlTemplate = paste0("https://ags.cuzk.cz/arcgis1/rest/services/ZTM_WM/", 
                           #"http://ags.cuzk.cz/arcgis/rest/services/zmwm/",
                           "MapServer/tile/{z}/{y}/{x}?blankTile=false"), 
      attribution = paste0("Základní Mapy ČR ©",
                           "<a href='https://www.cuzk.cz/' target = '_blank'>ČÚZK</a>"), 
      group = "Základní mapy ČR") %>% 
    leaflet::addLayersControl(
      baseGroups = c(
        "Desaturovaná mapa", 
        "Open Street Map", 
        "Základní mapy ČR"),
      options = leaflet::layersControlOptions(position = "bottomleft"))
}


leaflet_czechrep_add_marker <- function(click, url) {
  leaflet::leafletProxy("clickmap") %>%
    leaflet::clearMarkers() %>%
    leaflet::addCircleMarkers(
      layerId = "poi", click$lng, click$lat, 
      color = "#3E3F3A", radius = 16, 
      stroke = TRUE, fillOpacity = 0.6,
      # popup = paste0(
      #   tags$b("Zvolený bod"), tags$br(),
      #   tags$a(
      #     href = paste0(client_url(), "?lat=", click$lat, "&lng=", click$lng), 
      #     click$lat, "N ", click$lng, "E"))
      )
}

click2sf <- function(click) {
  sf::st_as_sf(data.frame(click), 
               coords = c("lng", "lat"), 
               crs = 4326)
}

leaflet_zoom <- function(ku, centroids) {
  coords <- centroids[centroids$ku == ku, ] %>%
    st_coordinates()
  
  leaflet::leafletProxy("clickmap") %>%
    leaflet::setView(zoom = 14, lng = coords[1], lat = coords[2])
}
