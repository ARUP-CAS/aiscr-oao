# get OAO AMCR ident_cely

library(tidyverse)

orgs <- read_csv(here::here("data/input/api_orgs.csv"))

# data from GD ------------------------------------------------------------

revised_gd_url <- "https://docs.google.com/spreadsheets/d/1knxDiUuCVqwgzgQkodhGg0vMe6w6LsKiGVqrsvi5dpw/edit#gid=0"


# revised ----------------------------------------

oao_gd <- googlesheets4::read_sheet(revised_gd_url, sheet = "oao_webapp")

oao_gd %>% 
  select(nazev_zkraceny) %>% 
  left_join(orgs, by = join_by("nazev_zkraceny" == "name_abbrv_cs")) %>% 
  write_csv(here::here("data/processed/nazev_zkraceny2ident_cely.csv"))
