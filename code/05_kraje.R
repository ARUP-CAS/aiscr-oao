# Download data o nařízení krajů

# URLs
url <- "https://sbirkapp.gov.cz/vyhledavani/vysledek?format_exportu=csv&hlavni_typ=pp&oblast=archeologicke-nalezy"
api <- "https://api.aiscr.cz/2.2/oai?verb=ListRecords&set=ruian_kraj&metadataPrefix=oai_amcr"

# Paths
p <- here::here("data/input/kraje/")

# Funs ----
# Download CSV file from 'sbirka'
download_csv <- function(url, path) {
  p <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_html() |>
    rvest::html_element(xpath = "/html/body/div/div/div/div[1]/a") |>
    rvest::html_attr("href")

  download.file(
    paste0("https://sbirkapp.gov.cz", p),
    destfile = paste0(
      path,
      "/",
      "narizeni-kraje_",
      lubridate::today(),
      ".csv"
    ),
    mode = "wb"
  )
}

# Read latest downloaded CSV
read_latest <- function(path) {
  csvs <- list.files(path)

  latest <- csvs |> stringr::str_extract("\\d{4}(-\\d{2}){2}") |> max()

  read.csv(file = paste0(path, "/", csvs[stringr::str_detect(csvs, latest)]))
}

# Get email filed from AMCR API
get_email_from_api <- function(url) {
  resp <- httr2::request(api) |>
    httr2::req_perform() |>
    httr2::resp_body_xml() |>
    xml2::xml_find_all(xpath = "//amcr:ruian_kraj")

  data.frame(
    kraj = sapply(resp, \(x) {
      x |> xml2::xml_find_first(xpath = ".//amcr:nazev") |> xml2::xml_text()
    }),
    email = sapply(x, \(x) {
      x |> xml2::xml_find_first(xpath = ".//amcr:email") |> xml2::xml_text()
    })
  )
}


# Data input ----

# Download CSV with data from 'Sbirka'
download_csv(url, path = p)

# Get emails from AMCR OAI-PMH API
emails <- get_email_from_api(api)

narizeni <- read_latest(p) |>
  dplyr::select(
    kraj = "Kraj.publikujícího",
    id = "Číslo.právního.předpisu",
    druh = "Druh.právního.předpisu",
    nazev = "Název.právního.předpisu",
    date_published = "Datum.vydání",
    date = "Datum.nabytí.účinnosti",
    url = "URL.záznamu",
    narizeni = "Platný.právní.předpis"
  ) |>
  dplyr::left_join(emails, by = dplyr::join_by("kraj"))


# Export ----
narizeni |>
  readr::write_csv(here::here("app/data/narizeni-kraje.csv"))
