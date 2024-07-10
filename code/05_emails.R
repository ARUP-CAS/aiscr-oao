# This script reads updated data from GD and creates emails

library(tidyverse)
# library(googledrive)
library(googlesheets4)


# funs --------------------------------------------------------------------

#' Prepare email text to remind OAO
#'
#' @param x Table OAO from GD.
#'
#' @return
#' @export
#'
#' @examples
write_email_html <- function(x) {
  
  format_date <- function(x) {
    format(x, "%d. %m. %Y")
  }
  
  x %>% 
    mutate(mk_email = if_else(!is.na(mk_minus6),
                              paste0(
                                "<html lang='cs'><body>Vážená paní/vážený pane,<br><br>",
                                "dovolujeme si Vás upozornit, že organizaci ",
                                "<a href='https://oao.aiscr.cz/#!/detail?oao=", ico, "/' target=_blank>", nazev, "</a>",
                                " brzy vyprší <b>Povolení provádět archeologické výzkumy</b> udělené Ministerstvem kultury České Republiky (č. j. ",
                                mk_id, ") platné od ", format_date(mk_from), " do ", format_date(mk_to), ".<br><br>",
                                "S pozdravem a přáním hezkého dne,<br>",
                                "Archeologický informační systém České Republiky (<a href='https://aiscr.cz/' target=_blank>AIS CR</a>)<br><br><hr>",
                                "Zpráva je automaticky generovaná, prosím neodpovídejte na ni.<br>",
                                "V případě potřeby kontaktujte administrátora ARÚ Praha ",
                                "(<a href='mailto:amcr@arup.cas.cz'>amcr@arup.cas.cz</a>) nebo ARÚ Brno ",
                                "(<a href='mailto:amcr@arub.cz'>amcr@arub.cz</a>).<br>",
                                "Odesláno z aplikace Mapa archeologických organizací ",
                                "(<a href='https://oao.aiscr.cz/' target=_blank>https://oao.aiscr.cz/</a>).</body></html>"
                              ), NA_character_),
           av_email = if_else(!is.na(av_minus6),
                              paste0(
                                "<html lang='cs'><body>Vážená paní/vážený pane,<br><br>",
                                "dovolujeme si Vás upozornit, že organizaci ",
                                "<a href='https://oao.aiscr.cz/#!/detail?oao=", ico, "/' target=_blank>", nazev, "</a>",
                                " brzy vyprší <b>Dohoda o rozsahu a podmínkách provádění archeologických výzkumů</b> uzavřená s Akademií věd České Republiky ",
                                "platná od ", format_date(av_from), " do ", format_date(av_to), ".<br><br>",
                                "S pozdravem a přáním hezkého dne,<br>",
                                "Archeologický informační systém České Republiky (<a href='https://aiscr.cz/' target=_blank>AIS CR</a>)<br><br><hr>",
                                "Zpráva je automaticky generovaná, prosím neodpovídejte na ni.<br>",
                                "V případě potřeby kontaktujte administrátora ARÚ Praha ",
                                "(<a href='mailto:amcr@arup.cas.cz'>amcr@arup.cas.cz</a>) nebo ARÚ Brno ",
                                "(<a href='mailto:amcr@arub.cz'>amcr@arub.cz</a>).<br>",
                                "Odesláno z aplikace Mapa archeologických organizací ",
                                "(<a href='https://oao.aiscr.cz/' target=_blank>https://oao.aiscr.cz/</a>).</body></html>"
                              ), NA_character_))
}


#' Write files with email text to disk
#'
#' @param x OAO table.
#'
#' @return
#' @export
#'
#' @examples
write_email_disk <- function(x) {
  dir_emails <- here::here("emailer/emails/")
  if (!dir.exists(dir_emails)) {
    dir.create(dir_emails)
  }
  
  for (i in seq_along(x$ico)) {
    write_lines(
      c(
        x[i, ]$ico,
        x[i, ]$nazev_zkraceny,
        x[i, ]$mail,
        as.character(x[i, ]$minus6),
        x[i, ]$subj,
        x[i, ]$email
      ), file = here::here(paste0(dir_emails, "/", x[i, ]$type, x[i, ]$ico, ".txt")), append = FALSE)
  }
}

# paths -------------------------------------------------------------------

dir_data <- here::here("data")
# dir_raw <- paste0(dir_data, "/raw")
# dir_der <- paste0(dir_data, "/derived")
dir_fin <- paste0(dir_data, "/final")


# data from GD ------------------------------------------------------------

revised_gd_url <- "https://docs.google.com/spreadsheets/d/1knxDiUuCVqwgzgQkodhGg0vMe6w6LsKiGVqrsvi5dpw/edit#gid=0"

oao_gd <- read_sheet(revised_gd_url, sheet = "oao_webapp") %>% 
  # remove space from PSČ to geocode addresses properly
  mutate(adresa = str_remove(adresa, "(?<=\\d{3})\\s(?=\\d{2})")) %>% 
  filter(app) %>% 
  select(nazev_zkraceny, nazev, ico, mail, starts_with(c("mk", "av"))) %>% 
  filter(!is.na(av_to) | !is.na(av_to)) %>% 
  mutate(
    mk_minus6 = lubridate::date(mk_to - lubridate::dmonths(6)),
    av_minus6 = lubridate::date(av_to - lubridate::dmonths(6))
  )


# data prep ---------------------------------------------------------------

mk <- oao_gd %>% 
  write_email_html() %>% 
  select(ico, nazev_zkraceny, mail, minus6 = mk_minus6, email = mk_email) %>% 
  filter(!is.na(minus6))

av <- oao_gd %>% 
  write_email_html() %>% 
  select(ico, nazev_zkraceny, mail, minus6 = av_minus6, email = av_email) %>% 
  filter(!is.na(minus6))

oao <- bind_rows(mk = mk, av = av, .id = "type") %>% 
  mutate(
    subj = if_else(type == "mk", "Informace k Povolení provádět archeologické výzkumy", NA_character_),
    subj = if_else(type == "av", "Informace k Dohodě o rozsahu a podmínkách provádění archeologických výzkumů", subj)
  )


# output ------------------------------------------------------------------

write_email_disk(oao)

# oao[2, ]$email %>% htmltools::HTML() %>% htmltools::html_print()
# 
# write_lines(
#   c(
#     oao[22, ]$ico,
#     oao[22, ]$nazev_zkraceny,
#     oao[22, ]$mail,
#     as.character(oao[4, ]$minus6),
#     oao[22, ]$subj,
#     oao[22, ]$email
#   ), file = here::here("fuubar.txt"), append = FALSE
# )

# htmltools::html_print(
#   htmltools::HTML(
#     paste0(
#       "Vážená paní/vážený pane,<br><br>",
#       "dovolujeme si Vás upozornit, že organizaci ", 
#       "<a href='https://oao.aiscr.cz/#!/detail?oao=", y$ico, "/' target=_blank>", y$nazev, "</a>",
#       " brzy vyprší <b>Povolení provádět archeologické výzkumy</b> udělené Ministerstvem kultury České Republiky (č. j. ",
#       y$mk_id, ") platné od ", format_date(y$mk_from), " do ", format_date(y$mk_to), ".<br><br>",
#       "S pozdravem a přáním hezkého dne,<br>",
#       "Archeologický informační systém České Republiky (<a href='https://aiscr.cz/' target=_blank>AIS CR</a>)<br><br><hr>",
#       "Zpráva je automaticky generovaná, prosím neodpovídejte na ni.<br>",
#       "V případě potřeby kontaktujte administrátora ARÚ Praha ",
#       "(<a href='mailto:amcr@arup.cas.cz'>amcr@arup.cas.cz</a>) nebo ARÚ Brno ",
#       "(<a href='mailto:amcr@arub.cz'>amcr@arub.cz</a>).<br>",
#       "Odesláno z aplikace Mapa archeologických organizací ",
#       "(<a href='https://oao.aiscr.cz/' target=_blank>https://oao.aiscr.cz/</a>)."
#     )))

# htmltools::html_print(
#   htmltools::HTML(
#     paste0(
#       "Vážená paní/vážený pane,<br><br>",
#       "dovolujeme si Vás upozornit, že organizaci ", 
#       "<a href='https://oao.aiscr.cz/#!/detail?oao=", y$ico, "/' target=_blank>", y$nazev, "</a>",
#       " brzy vyprší <b>Dohoda o rozsahu a podmínkách provádění archeologických výzkumů</b> uzavřená s Akademií věd České Republiky ",
#       "platná od ", format_date(y$av_from), " do ", format_date(y$av_to), ".<br><br>",
#       "S pozdravem a přáním hezkého dne,<br>",
#       "Archeologický informační systém České Republiky (<a href='https://aiscr.cz/' target=_blank>AIS CR</a>)<br><br><hr>",
#       "Zpráva je automaticky generovaná, prosím neodpovídejte na ni.<br>",
#       "V případě potřeby kontaktujte administrátora ARÚ Praha ",
#       "(<a href='mailto:amcr@arup.cas.cz'>amcr@arup.cas.cz</a>) nebo ARÚ Brno ",
#       "(<a href='mailto:amcr@arub.cz'>amcr@arub.cz</a>).<br>",
#       "Odesláno z aplikace Mapa archeologických organizací ",
#       "(<a href='https://oao.aiscr.cz/' target=_blank>https://oao.aiscr.cz/</a>)."
#     )))

