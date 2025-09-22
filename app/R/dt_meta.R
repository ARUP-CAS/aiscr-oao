
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
        "Organizace" = "label",
        "AMČR ID" = "amcr_id",
        "Typ organizace" = "typ",
        # "IČO" = "ico",
        "Webové stránky" = "web",
        "Email" = "email",
        "Telefon" = "telefon",
        # "Adresa" = "adresa",
        "Platnost oprávnění<br>MK ČR" = "mk_to",
        "Platnost dohody<br>s AV ČR" = "av_to",
        "Dohoda o<br>užívání AMČR" = "amcr"),
      options = list(
        dom = "t",
        deferRender = TRUE,
        scrollY = "calc(100vh - 340px)",
        scroller = TRUE,
        columnDefs = list(
          # list(className = 'dt-right', targets = c(0)),
          list(className = 'dt-center', targets = c(0, 7:9)))
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
        c("label"),
        \(x) stringr::str_replace_all(stringr::str_wrap(x, width = 32), "\\n", "<br>")),
      dplyr::across(
        dplyr::ends_with(c("from", "to")), 
        \(x) as.Date(x, format = "%d. %m. %Y")),
      telefon = stringr::str_replace_all(telefon, ",\\s", "<br>"),
      mk_to = dplyr::if_else(is.na(mk_to) & mk_neomezena, "na dobu<br>neurčitou", as.character(mk_to)),
      av_to = dplyr::if_else(is.na(av_to) & av_neomezena, "na dobu<br>neurčitou", as.character(av_to)),
      link_map = paste0("<a href='", url, "detail?oao=", 
                        amcr_id, "/'>", icon_map_link, "</a>"),
      amcr = if_else(amcr, "&#x2714;", "&#10008;")) %>% 
    dplyr::select(link_map, label, amcr_id, typ, web, email, telefon, mk_to, av_to, amcr)
}

