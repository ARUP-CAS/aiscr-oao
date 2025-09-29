oao_filter_poly <- function(data, click, rep, oao_names, url) {
  # oprávnění mají
  poly_pred <- sf::st_intersects(data, click)
  
  sf::st_drop_geometry(data)[poly_pred %>% lengths() > 0, ] %>% 
    dplyr::arrange(area) %>% 
    # removes whole republic oao
    dplyr::filter(!amcr_id %in% rep$amcr_id) %>%
    dplyr::mutate(name = oao_names[amcr_id],
                  link = paste0("<a href='", url, 
                                "detail?oao=", amcr_id, "/'>", 
                                icon_map_link, "</a>")) %>% 
    dplyr::select(Detail = link, Organizace = name)
}

oao_filter_grid <- function(data, click, buffer, oao_names, url) {
  # výzkumy provádí
  grid_pred <- sf::st_intersects(
    data,
    sf::st_buffer(click, buffer * 1e3)
  )
  
  sf::st_drop_geometry(data)[grid_pred %>% lengths() > 0, ] %>%
    dplyr::group_by(amcr_id) %>%
    dplyr::summarize(value = sum(value)) %>%
    dplyr::arrange(dplyr::desc(value)) %>%
    dplyr::mutate(name = oao_names[amcr_id],
                  link = paste0("<a href='", url, 
                                "detail?oao=", amcr_id, "/'>", 
                                icon_map_link, "</a>")) %>% 
    dplyr::select(Detail = link, Organizace = name)
}

click_cell <- function(click) {
  sf::st_bbox(sf::st_buffer(click, 1e3))
}

click_buffer <- function(click, buffer) {
  sf::st_bbox(sf::st_buffer(click, buffer * 1e3))
}
