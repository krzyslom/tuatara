# Libraries

library(dplyr)
library(pbapply)
library(readr)
library(rvest)
library(stringdist)
library(stringi)
library(tidyr)

# Functions

source("adres_z_opisu.R")

scrapuj <-
  function(adresy) {
    adresy %>%
      pblapply(
        function(href) {
          html <-
            tryCatch(
              {
                href %>%
                  read_html()
              },
              error = function(e) {
                message(paste("Oferta nieaktualna", href))
                message("Oryginalna wiadomość:")
                message(e)
                actualPages[1] %>% read_html()
              }
            )
          opis <-
            html %>%
            html_nodes(".vip-details .description") %>%
            html_text()
          adres <-
            html %>%
            html_nodes(".address") %>%
            html_text()
          if (adres %>% length() == 0) {adres <- "BRAK_ADRESU"}
          cena <-
            html %>%
            html_nodes(".clearfix .amount") %>%
            html_text()
          if (cena %>% length() == 0) {
            cena <-
              html %>%
              html_nodes(".clearfix .value") %>%
              html_text()
          }
          tytuł <-
            html %>%
            html_nodes(".myAdTitle") %>%
            html_text()
          c(".name", ".attribute .value") %>%
            lapply(
              function(css) {
                css %>%
                  html_nodes(html, .) %>%
                  html_text()
              }
            ) %>%
            setNames(c("keys", "values"))
        } %>%
          as_data_frame() %>%
          spread(keys, values) %>%
          setNames(names(.) %>% tolower() %>% gsub("[[:space:]]", "_", .)) %>%
          cbind(href, opis, adres, cena, tytuł)
      )
  }

# Variables

slownik <-
  data.table::fread(
    'warszawskie_ulice.txt', encoding = "UTF-8", data.table = FALSE
  ) %>%
  unlist() %>%
  unname()
mainPage <- "https://www.gumtree.pl"
searchPage <- mainPage %>% paste0("/s-mieszkania-i-domy-do-wynajecia/warszawa/v1c9008l3200008p")
nOfPages <-
  searchPage %>%
  paste0(1) %>%
  read_html() %>%
  html_nodes(".last") %>%
  html_attr("href") %>%
  gsub(".*v1c9008l3200008p", "", .) %>%
  as.integer()

links <-
  1:nOfPages %>%
  pblapply(
    function(nop) {
      searchPage %>%
        paste0(nop) %>%
        read_html() %>%
        html_nodes(".href-link") %>%
        html_attr("href")
    }
  ) %>%
  unlist() %>%
  gsub("/a-mieszkania-i-domy-do-wynajecia/", "", .) %>%
  unique()

actualPages <-
  mainPage %>%
  paste0("/a-mieszkania-i-domy-do-wynajecia/", links)

# Uwaga, należy jeszcze podzielić adresy po około 1000
data <- scrapuj(actualPages)