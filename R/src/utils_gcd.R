# LIBRARIES and SOURCES ---------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)
library(hrbrthemes)

source(here::here("R", "global.R"), encoding = "utf8")

# CONSTANTS ---------------------------------------------------------------


# FUNCTIONS ---------------------------------------------------------------

make_url <- function(str_url, ...) {
  
  sprintf(str_url, ...)
}

mi_GET <- function(url) {
  
  res <- GET(url = url,
             content_type_json(),
             add_headers(`Authorization` = sprintf("api_key %s", API_KEY)))
  
  return(res)
}

get_contenido <- function(res) {
  
  the_content <- content(res, as = "text", encoding = "ISO-8859-15") %>%
    fromJSON(flatten = TRUE) %>%
    as_tibble() %>%
    readr::type_convert()
  
  return(the_content)
}

get_datos_y_metadatos <- function(the_content) {
  
  url_datos <- the_content %>% pull(datos)
  url_metadatos <- the_content %>% pull(metadatos)
  
  datos <- fromJSON(readLines(url_datos), flatten = TRUE) %>% as_tibble()
  metadatos <- fromJSON(readLines(url_metadatos), flatten = TRUE)
  
  return(list(datos = datos,
              metadatos = metadatos))
}