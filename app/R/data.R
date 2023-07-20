#' Read metadata on OAO
#' 
#' Reads and manipulates metadata on OAO.
#'
#' @param dsn Data.
#'
#' @return A \code{sf} object.
#' @export
#'
#' @examples
oao_meta <- function(dsn, client_url) {
  sf::st_read(dsn = dsn) %>% 
    # sf::st_drop_geometry() %>% 
    dplyr::mutate(
      dplyr::across(dplyr::ends_with(c("from", "to")), \(x) format(x, "%d. %m. %Y")),
      # dplyr::across(dplyr::ends_with(c("from", "to")), \(x) stringr::str_replace_all(x, "\\s", "&nbsp")),
      datum_mk = dplyr::if_else(
        is.na(mk_to),
        dplyr::if_else(
          mk_neomezena, 
          stringr::str_c("od ", mk_from, " na dobu neurčitou"),
          stringr::str_c("od ", mk_from)),
        stringr::str_c("od ", mk_from, " do ", mk_to)),
      datum_av = dplyr::if_else(
        is.na(av_to),
        dplyr::if_else(
          av_neomezena,
          stringr::str_c("od ", av_from, " na dobu neurčitou"),
          stringr::str_c("od ", av_from)),
        stringr::str_c("od ", av_from, " do ", av_to)),
      dplyr::across(c("datum_av", "datum_mk"), 
                    \(x) stringr::str_remove_all(x, "(?<=\\s)0")),
      opravneni = dplyr::if_else(
        !nazev_zkraceny %in% c("Archeologický ústav Brno", "Archeologický ústav Praha"),
        dplyr::if_else(
          !stringr::str_detect(nazev_zkraceny, "ÚAPP"),
          paste0(dplyr::if_else(
            !is.na(datum_mk),
            paste0("Platnost oprávnění MK ČR ",
                   dplyr::if_else(!is.na(mk_id),
                                  paste0("(", mk_id, ") "),
                                  ""),
                   datum_mk, ". "),
            ""),
            "Dohoda s AV ČR ", datum_av, "."),
          paste0(mk_id, ". Dohoda s AV ČR ", datum_av, ".")),
        "Oprávnění v plném rozsahu dle zákona o státní památkové péči."),
      web = dplyr::if_else(
        !is.na(web), 
        paste0("<a target='_blank' href='", web, "'>",
               icon("fas fa-external-link-alt"), " ", web, "</a>"),
        "")) %>% 
    dplyr::arrange(nazev)
}

#' Wrapper around \code{sf::st_read}
#'
#' @param dsn Data.
#'
#' @return A \code{sf} object.
#' @export
#'
#' @examples
oao_sf <- function(dsn) {
  sf::st_read(dsn = dsn)
}

oao_filter <- function(data, oao) {
  data %>% 
    dplyr::filter(ico %in% oao)
}