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
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron, 
      group = "Desaturovaná mapa") %>% 
    leaflet::addTiles(
      urlTemplate = paste0("http://ags.cuzk.cz/arcgis/rest/services/zmwm/",
                           "MapServer/tile/{z}/{y}/{x}?blankTile=false"), 
      attribution = paste0("Základní Mapy ČR ©",
                           "<a href='https://www.cuzk.cz/' target = '_blank'>ČÚZK</a>"), 
      group = "Základní mapy ČR") %>% 
    leaflet::addLayersControl(
      baseGroups = c(
        "Desaturovaná mapa", 
        "Open Street Map", 
        "Základní mapy ČR"),
      options = leaflet::layersControlOptions(collapsed = TRUE))
}


leaflet_czechrep_add_marker <- function(click) {
  leaflet::leafletProxy("clickmap") %>%
    leaflet::clearMarkers() %>%
    leaflet::addCircleMarkers(
      layerId = "poi", click$lng, click$lat, 
      color = "#3E3F3A", radius = 16, 
      stroke = TRUE, fillOpacity = 0.6,
      popup = "Zvolená poloha")
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
