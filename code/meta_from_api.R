# get data on OAO from OAI-PMH AMCR API

library(tidyverse)
# library(httr2)
# library(xml2)


# paths -------------------------------------------------------------------

dir_data <- here::here("data/")


# funs --------------------------------------------------------------------
amcr_orgs <- function() {
  # const
  api_version = "2.2"
  url = paste0("https://api.aiscr.cz/", api_version, "/oai")
  verb = "ListRecords"
  prefix = "oai_amcr"
  set = "organizace"
  
  # tags
  tags = c(
    amcr_id = "amcr:ident_cely",
    nazev = "amcr:nazev",
    # name_en = "amcr:nazev_en",
    nazev_zkraceny = "amcr:nazev_zkraceny",
    # name_abbrv_en = "amcr:nazev_zkraceny_en",
    typ = "amcr:typ_organizace",
    oao = "amcr:oao",
    zverejneni = "amcr:mesicu_do_zverejneni",
    pristupnost = "amcr:zverejneni_pristupnost",
    adresa = "amcr:adresa",
    ico = "amcr:ico",
    ror = "amcr:ror",
    # web
    email = "amcr:email",
    telefon = "amcr:telefon"
  )
  
  # request
  query <- function(url, verb, set, prefix, token = NA) {
    req <- httr2::request(url) |>
      httr2::req_url_query(verb = verb)
    
    if (!missing(token)) {
      req <- req |> 
        httr2::req_url_query(resumptionToken = token)
    } else {
      req <- req |> 
        httr2::req_url_query(set = set, metadataPrefix = prefix)
    }
    
    resp <- req |> 
      httr2::req_perform() |>
      httr2::resp_body_xml() # |>
    # xml2::xml_ns_strip()
    
    return(resp)
  }
  
  # response
  resp <- query(url, verb, set, prefix)
  
  # resumptionToken
  get_token <- function(response) {
    token <- response |> xml2::xml_ns_strip() |> xml2::xml_find_all("//resumptionToken") |>
      xml2::xml_text()
    return(token)
  }
  token <- get_token(resp)
  
  # parsing
  xpath <- function(response, xpath) {
    res <- xml2::xml_find_first(response, xpath) |> 
      xml2::xml_text()
    if (length(res) == 0) {
      res <- NA_character_
    }
    return(res)
  }
  
  # parser to handle missing elements
  parser <- function(response, tags) {
    # split to children nodes
    children <- xml2::xml_find_all(response, xpath = "//amcr:organizace")
    
    parsed <- vector("list", length(children))
    
    # parse xpaths
    for (i in seq_along(children)) {
      parsed[[i]] <- lapply(tags, \(tag) xpath(children[i], xpath = paste0(tag))) |>
        bind_cols()
    }
    
    return(bind_rows(parsed))
  }
  
  parsed <- parser(resp, tags)
  
  # while loop to fetch all pages
  while (length(token) == 1) {
    resp2 <- query(url, verb, set, prefix, token = token)
    token <- get_token(resp2)
    parsed <- bind_rows(parsed, parser(resp2, tags))
  }
  
  return(parsed)
  
}


# query data --------------------------------------------------------------

orgs <- amcr_orgs()


# export ------------------------------------------------------------------

orgs %>% 
  write_csv(paste0(dir_data, "/input/api_orgs_", today(), ".csv"))



