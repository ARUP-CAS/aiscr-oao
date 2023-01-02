
#' Create DT
#'
#' @param data Prepared tibble with metadata. 
#'
#' @return
#' @export
#'
#' @examples
dt_create <- function(data) {
  data %>% 
    DT::datatable(
      escape = FALSE,
      extensions = 'Scroller', 
      rownames = FALSE,
      colnames = c(
        "Organizace" = "nazev",
        "Mapa<br>působnosti" = "link_map",
        "IČO" = "ico",
        "Webové stránky" = "web",
        "Adresa" = "adresa",
        "Platnost oprávnění<br>MK ČR" = "mk_to",
        "Platnost dohody<br>s AV ČR" = "av_to"),
      options = list(
        dom = "t",
        deferRender = TRUE,
        scrollY = "calc(100vh - 300px)", 
        scroller = TRUE,
        columnDefs = list(
          list(
            # className = 'dt-left', targets = "_all"
            className = 'dt-center', targets = c(1, 2, 5, 6)
          )
        )
      )
    )
}


#' Prepare data for DT
#'
#' @param data A tibble with metadata.
#'
#' @return
#' @export
#'
#' @examples
dt_data_prep <- function(data, url) {
  data %>% 
    dplyr::mutate(
      dplyr::across(
        c("nazev", "adresa"), 
        \(x) stringr::str_replace_all(
          stringr::str_wrap(x, width = 36), "\\n", "<br>")),
      dplyr::across(
        dplyr::ends_with(c("from", "to")), 
        \(x) as.Date(x, format = "%d. %m. %Y")),
      mk_to = dplyr::if_else(is.na(mk_to) & mk_neomezena, "neomezena", as.character(mk_to)),
      av_to = dplyr::if_else(is.na(av_to) & av_neomezena, "neomezena", as.character(av_to)),
      link_map = paste0("<a href='", url, "detail?oao=", 
                        ico, "/'>", icon_map_link, "</a>")
    ) %>% 
    dplyr::select(nazev, link_map, ico, web, adresa, mk_to, av_to)
}

