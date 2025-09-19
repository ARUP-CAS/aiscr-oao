
#' Create DT
#'
#' @param data Prepared tibble with metadata. 
#'
#' @return
#' @export
#'
#' @examples
dt_create <- function(data) {
  data |> 
    DT::datatable(
      escape = FALSE,
      extensions = 'Scroller', 
      rownames = FALSE,
      colnames = c(
        " " = "link_map",
        "Organizace" = "nazev",
        "AMČR ID" = "amcr_id",
        "IČO" = "ico",
        "Webové stránky" = "web",
        "Email" = "email",
        # "Adresa" = "adresa",
        "Platnost oprávnění<br>MK ČR" = "mk_to",
        "Platnost dohody<br>s AV ČR" = "av_to"),
      options = list(
        dom = "t",
        deferRender = TRUE,
        scrollY = "calc(100vh - 340px)",
        scroller = TRUE
        # columnDefs = list(
        #   list(className = 'dt-right', targets = c(0)),
        #   list(className = 'dt-center', targets = c(3, 4, 7, 8)))
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
      # adresa = str_replace(adresa, ", ", "<br>"),
      dplyr::across(
        c("nazev"),
        \(x) stringr::str_replace_all(stringr::str_wrap(x, width = 36), "\\n", "<br>")),
      dplyr::across(
        dplyr::ends_with(c("from", "to")), 
        \(x) as.Date(x, format = "%d. %m. %Y")),
      mk_to = dplyr::if_else(is.na(mk_to) & mk_neomezena, "na dobu<br>neurčitou", as.character(mk_to)),
      av_to = dplyr::if_else(is.na(av_to) & av_neomezena, "na dobu<br>neurčitou", as.character(av_to)),
      link_map = paste0("<a href='", url, "detail?oao=", 
                        amcr_id, "/'>", icon_map_link, "</a>")) %>% 
    dplyr::select(link_map, nazev, amcr_id, ico, web, email, mk_to, av_to)
}

