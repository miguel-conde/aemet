# http://www.aemet.es/es/datos_abiertos/AEMET_OpenData

# LIBRARIES and SOURCES ---------------------------------------------------

source(here::here("R", "src", "utils_gcd.R"), encoding = "utf8")

library(sf)

# FUNCTIONES --------------------------------------------------------------

make_dec_lat <- function(x) {
  
  d <- substr(x, 1, 2)
  m <- substr(x, 3, 4)
  s <- substr(x, 5, 6)
  
  # the_sign <- ifelse(substr(x, 7, 7) == "W", "-", "+")
  the_sign <- substr(x, 7, 7)
  
  paste0(d, "d", m, "'", s, "\"", the_sign) %>% sp::char2dms() %>% as.numeric()
}

make_dec_long <- function(x) {
  
  d <- substr(x, 1, 2)
  m <- substr(x, 3, 4)
  s <- substr(x, 5, 6)
  
  # the_sign <- ifelse(substr(x, 7, 7) == "W", "-", "+")
  the_sign <- substr(x, 7, 7)
  
  paste0(d, "d", m, "'", s, "\"", the_sign) %>% sp::char2dms() %>% as.numeric()
}


# GET DATA ----------------------------------------------------------------

# Estaciones --------------------------------------------------------------

# Curl
# curl -X GET --header 'Accept: application/json' --header 'api_key: eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtaWd1ZWxjbzIwMDBAZ21haWwuY29tIiwianRpIjoiNmU0YjMzYjgtZDU2OS00YmQ2LTllODktMTY4OWJjMjlmZWQxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2NTkzNzYwMTksInVzZXJJZCI6IjZlNGIzM2I4LWQ1NjktNGJkNi05ZTg5LTE2ODliYzI5ZmVkMSIsInJvbGUiOiIifQ.8DwfGTyKxzhsT8ySBAfSSnDzfBfOAUR_sTYopCKO9Ms' 'https://opendata.aemet.es/opendata/api/valores/climatologicos/inventarioestaciones/todasestaciones'

# Request URL
# https://opendata.aemet.es/opendata/api/valores/climatologicos/inventarioestaciones/todasestaciones

url <- URL_ESTACIONES

estaciones <- mi_GET(url) %>% get_contenido() %>% get_datos_y_metadatos()

saveRDS(estaciones, FILE_AEMET_ESTACIONES)


# Maestro - Municipios ----------------------------------------------------

## GET /api/maestro/municipios

# Curl
# curl -X GET --header 'Accept: text/plain' --header 'api_key: eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtaWd1ZWxjbzIwMDBAZ21haWwuY29tIiwianRpIjoiNmU0YjMzYjgtZDU2OS00YmQ2LTllODktMTY4OWJjMjlmZWQxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2NTkzNzYwMTksInVzZXJJZCI6IjZlNGIzM2I4LWQ1NjktNGJkNi05ZTg5LTE2ODliYzI5ZmVkMSIsInJvbGUiOiIifQ.8DwfGTyKxzhsT8ySBAfSSnDzfBfOAUR_sTYopCKO9Ms' 'https://opendata.aemet.es/opendata/api/maestro/municipios'

# Request URL
# https://opendata.aemet.es/opendata/api/maestro/municipios

url <- URL_MAESTRO_MUNICIPIOS

maestro_municipios <- mi_GET(url) %>% get_contenido()

saveRDS(maestro_municipios, FILE_AEMET_MAESTRO_MUNICIPIOS)

# Maestro - Municipio -----------------------------------------------------

# GET /api/maestro/municipio/{municipio}

# Curl
# curl -X GET --header 'Accept: application/json' --header 'api_key: eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtaWd1ZWxjbzIwMDBAZ21haWwuY29tIiwianRpIjoiNmU0YjMzYjgtZDU2OS00YmQ2LTllODktMTY4OWJjMjlmZWQxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2NTkzNzYwMTksInVzZXJJZCI6IjZlNGIzM2I4LWQ1NjktNGJkNi05ZTg5LTE2ODliYzI5ZmVkMSIsInJvbGUiOiIifQ.8DwfGTyKxzhsT8ySBAfSSnDzfBfOAUR_sTYopCKO9Ms' 'https://opendata.aemet.es/opendata/api/maestro/municipio/madrid'

# Request URL
# https://opendata.aemet.es/opendata/api/maestro/municipio/madrid
municipio <-  "id44003"

url <- make_url(URL_MAESTRO_MUNICIPIO, municipio)

maestro_municipio_id44003 <- mi_GET(url) %>% get_contenido()


# Climatologías normales (1981-2010). -------------------------------------

# GET /api/valores/climatologicos/normales/estacion/{idema}
# Climatologías normales (1981-2010).

# Curl
# curl -X GET --header 'Accept: application/json' --header 'api_key: eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtaWd1ZWxjbzIwMDBAZ21haWwuY29tIiwianRpIjoiNmU0YjMzYjgtZDU2OS00YmQ2LTllODktMTY4OWJjMjlmZWQxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2NTkzNzYwMTksInVzZXJJZCI6IjZlNGIzM2I4LWQ1NjktNGJkNi05ZTg5LTE2ODliYzI5ZmVkMSIsInJvbGUiOiIifQ.8DwfGTyKxzhsT8ySBAfSSnDzfBfOAUR_sTYopCKO9Ms' 'https://opendata.aemet.es/opendata/api/valores/climatologicos/normales/estacion/3100B'

# Request URL
# https://opendata.aemet.es/opendata/api/valores/climatologicos/normales/estacion/3100B

url <- make_url(URL_CLIMATOLOGIAS_NORMALES, "3100B")

res_normales_3100B <- mi_GET(url) %>% get_contenido() %>% get_datos_y_metadatos()


# Climatologías diarias. --------------------------------------------------

# GET /api/valores/climatologicos/diarios/datos/fechaini/{fechaIniStr}/fechafin/{fechaFinStr}/estacion/{idema}
# Climatologías diarias.

# Curl
# curl -X GET --header 'Accept: application/json' --header 'api_key: eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtaWd1ZWxjbzIwMDBAZ21haWwuY29tIiwianRpIjoiNmU0YjMzYjgtZDU2OS00YmQ2LTllODktMTY4OWJjMjlmZWQxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2NTkzNzYwMTksInVzZXJJZCI6IjZlNGIzM2I4LWQ1NjktNGJkNi05ZTg5LTE2ODliYzI5ZmVkMSIsInJvbGUiOiIifQ.8DwfGTyKxzhsT8ySBAfSSnDzfBfOAUR_sTYopCKO9Ms' 'https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/2022-01-01T00%3A00%3A01UTC/fechafin/2022-01-31T00%3A00%3A01UTC/estacion/3100B'

# Request URL
# https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/2022-01-01T00%3A00%3A01UTC/fechafin/2022-01-31T00%3A00%3A01UTC/estacion/3100B

fecha_ini <- "2021-01-01T00:00:01UTC"
fecha_fin <- "2021-12-31T23:59:59UTC"
idema <- "3100B"

url <- make_url(URL_CLIMATOLOGIAS_DIARIAS,
                fecha_ini,
                fecha_fin,
                idema)

res_diario__2021_3100B <- mi_GET(url) %>% get_contenido() %>% get_datos_y_metadatos()

#### 

ids_estaciones <- estaciones$datos$indicativo %>% unique()

url <- make_url(URL_CLIMATOLOGIAS_DIARIAS,
                fecha_ini,
                fecha_fin,
                ids_estaciones %>% paste(collapse = ","))

res_diario_2021 <- mi_GET(url) %>% get_contenido() %>% get_datos_y_metadatos()

###

ids_estaciones <- estaciones$datos$indicativo %>% unique()

seq_yrs <- 1920:2021

lst_res <- vector(mode = "list", length = length(seq_yrs))
names(lst_res) <- seq_yrs

for (idx_yr in seq_along(seq_yrs)) {
  
  print(seq_yrs[idx_yr])
  
  fecha_ini <- sprintf("%s-01-01T00:00:01UTC", seq_yrs[idx_yr])
  fecha_fin <- sprintf("%s-12-31T23:59:59UTC", seq_yrs[idx_yr])
  
  url <- make_url(URL_CLIMATOLOGIAS_DIARIAS,
                  fecha_ini,
                  fecha_fin,
                  ids_estaciones %>% paste(collapse = ","))
  
  lst_res[[idx_yr]] <- mi_GET(url) %>% get_contenido() %>% get_datos_y_metadatos()
  
}

saveRDS(lst_res, FILE_AEMET_DATOS_ESTACIONES_1922_2021)


# CLEAN DATA --------------------------------------------------------------


# Estaciones --------------------------------------------------------------

probe <- estaciones$datos %>% 
  mutate(latitud = make_dec_lat(latitud)) %>%  
  mutate(longitud = make_dec_lat(longitud)) %>% 
  mutate(altitud = as.numeric(altitud))
st_estaciones <- probe %>% 
  # st_as_sf(coords = c("longitud", "latitud"), crs = 27700)  %>% 
  st_as_sf(coords = c("longitud", "latitud", "altitud"), crs = 4326, remove = FALSE) 
st_estaciones %>% st_geometry() %>% plot()
st_estaciones


# Evolutivo ---------------------------------------------------------------

evol <- lst_res %>% 
  sapply(function(x) x$datos) %>% 
  bind_rows() %>% 
  mutate_at(vars(altitud, tmed, prec, tmin, tmax, velmedia, sol, presMax, 
                 presMin, dir, racha),
            ~ str_replace(., ",", ".") %>% as.numeric) %>% 
  mutate(fecha = lubridate::as_date(fecha))

st_evol <- evol %>% 
  select(-provincia, -altitud, -nombre) %>% 
  left_join(probe, by = "indicativo") %>% 
  # st_as_sf(coords = c("longitud", "latitud"), crs = 27700)  %>% 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326) 

st_evol


# Save --------------------------------------------------------------------


all_data <- list(st_estaciones = st_estaciones,
                 evol = evol,
                 st_evol = st_evol)

saveRDS(all_data, FILE_ALL_DATA)


### 

