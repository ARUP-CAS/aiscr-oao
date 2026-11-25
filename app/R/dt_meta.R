
#' Create DT
#'
#' @param data Prepared tibble with metadata. 
#'
#' @return
#' @export
#'
#' @examples
dt_create <- function(data) {
  dt <- data |>
    DT::datatable(
      escape = FALSE,
      extensions = 'Scroller',
      rownames = FALSE,
      filter = list(position = "top", clear = TRUE, plain = FALSE),
      colnames = c(
        " " = "link_map",
        "Organizace" = "label",
        "AMČR ID" = "amcr_id",
        "Typ organizace" = "typ",
        "Zřizovatel" = "zrizovatel",
        "Typ zřizovatele" = "typ_zrizovatele",
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
          # organisation name wraps in the browser; search runs on the raw name
          list(targets = 1, render = DT::JS(
            "function(data, type) {",
            "  return type === 'display' ?",
            "    '<div style=\"max-width:16em;white-space:normal\">' + data + '</div>' : data;",
            "}")),
          # list(className = 'dt-right', targets = c(0)),
          list(className = 'dt-center', targets = c(0, 9:11)),
          # column filters only on: Organizace, AMČR ID, Typ organizace,
          # Zřizovatel, Typ zřizovatele, Platnost MK ČR, Platnost AV ČR
          list(searchable = FALSE, targets = c(0, 6:8, 11)))
      )
    )

  # Czech placeholder in the column filter row
  dt$x$filterHTML <- gsub('placeholder="All"', 'placeholder="Vše"',
                          dt$x$filterHTML, fixed = TRUE)
  dt
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
      # multi-select column filters (DT offers a dropdown for factors)
      dplyr::across(
        c("typ", "zrizovatel", "typ_zrizovatele"),
        \(x) factor(x, levels = stringr::str_sort(unique(stats::na.omit(x)), locale = "cs"))),
      dplyr::across(
        dplyr::ends_with(c("from", "to")), 
        \(x) as.Date(x, format = "%d. %m. %Y")),
      telefon = stringr::str_replace_all(telefon, ",\\s", "<br>"),
      mk_to = dplyr::if_else(is.na(mk_to) & mk_neomezena, "na dobu<br>neurčitou", as.character(mk_to)),
      av_to = dplyr::if_else(is.na(av_to) & av_neomezena, "na dobu<br>neurčitou", as.character(av_to)),
      link_map = paste0("<a href='", url, "detail?oao=", 
                        amcr_id, "/'>", icon_map_link, "</a>"),
      amcr = if_else(amcr, "&#x2714;", "&#10008;")) %>% 
    dplyr::select(link_map, label, amcr_id, typ, zrizovatel, typ_zrizovatele, web, email, telefon, mk_to, av_to, amcr)
}

