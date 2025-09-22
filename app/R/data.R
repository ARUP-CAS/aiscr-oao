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
  res <- sf::st_read(dsn = dsn) %>% 
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
      dplyr::across(c("datum_av", "datum_mk"), 
                    \(x) if_else(!is.na(x), paste0("<b>", x, "</b>"), x)),
      opravneni = dplyr::if_else(
        !nazev_zkraceny %in% c("Archeologický ústav AV ČR, Brno", "Archeologický ústav AV ČR, Praha"),
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
      amcr_note = if_else(amcr, "Organizace <b>má uzavřenou</b> dohodu o užívání AMČR.",
                          "Organizace <b>neuzavřela</b> dohodu o užívání AMČR."),
      amcr_zverejneni = paste0("Po <b>", zverejneni/12, " letech</b> od archivace jsou dokumenty organizace v AMČR zveřejněny na úrovni přístupnosti <b>", pristupnost, "</b>."),
      web0 = web,
      web = dplyr::if_else(
        !is.na(web), 
        paste0("<a target='_blank' href='", web, "'>",
               icon_ext_link, " ", web, "</a>"), ""),
      web_app = paste0("https://oao.aiscr.cz/#!/detail?oao=", amcr_id),
      mail0 = email,
      email = dplyr::if_else(
        !is.na(email), 
        paste0("<a target='_blank' href='mailto:", email, "'>",
               icon_mail, " ", email, "</a>"), ""),
      telefon = if_else(is.na(telefon), "–", telefon),
      telefon = stringr::str_replace_all(telefon, ";\\s", ", "),
      ror = if_else(!is.na(ror), str_remove(ror, "https://ror.org/"), ror),
      ror = if_else(is.na(ror), "–", paste0("<a target=_blank href='", url_ror, ror,
                                            "'>", icon_ext_link, " ", ror, "</a>")),
      amcr_id = if_else(is.na(amcr_id), "–", amcr_id),
      api = if_else(is.na(amcr_id), "–", paste0("<a target=_blank href='", url_api,
                                                amcr_id, "'> ", icon_ext_link, " OAI-PMH API</a>")),
      da = if_else(is.na(amcr_id), "–", paste0("<a target=_blank href='", url_da,
                                               amcr_id, ":or'> ", icon_ext_link, " Projekty</a>, ",
                                               "<a target=_blank href='", url_da_akce,
                                               amcr_id, ":or'> ", icon_ext_link, " Akce</a>, ",
                                               "<a target=_blank href='", url_da_dokumenty,
                                               amcr_id, ":or'> ", icon_ext_link, " Dokumenty</a>, ",
                                               "<a target=_blank href='", url_da_nalezy,
                                               amcr_id, ":or'> ", icon_ext_link, " Samostatné nálezy</a>"))
    ) %>% 
    dplyr::arrange(nazev, .locale = "cs")
  
  return(res)
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
    dplyr::filter(amcr_id %in% oao)
}


detail_table <- function(data) {
  data %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(typ, web, email, telefon, adresa, ico, ror, amcr_id, api, da) %>% 
    tidyr::pivot_longer(cols = dplyr::everything()) %>% 
    dplyr::mutate(name = c("Typ organizace", "Webové stránky", "Email", "Telefon", "Adresa", "IČO", "ROR", "AMČR ID", "AMČR API", "Digitální Archiv AMČR")) %>% 
    knitr::kable(col.names = c(), format = "html", escape = FALSE)
}