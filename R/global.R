library(tidyverse)

# CONSTANTS ---------------------------------------------------------------

API_KEY <- "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtaWd1ZWxjbzIwMDBAZ21haWwuY29tIiwianRpIjoiNmU0YjMzYjgtZDU2OS00YmQ2LTllODktMTY4OWJjMjlmZWQxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2NTkzNzYwMTksInVzZXJJZCI6IjZlNGIzM2I4LWQ1NjktNGJkNi05ZTg5LTE2ODliYzI5ZmVkMSIsInJvbGUiOiIifQ.8DwfGTyKxzhsT8ySBAfSSnDzfBfOAUR_sTYopCKO9Ms"

URL_CLIMATOLOGIAS_NORMALES <- "https://opendata.aemet.es/opendata/api/valores/climatologicos/normales/estacion/%s"
URL_CLIMATOLOGIAS_DIARIAS <- "https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/%s/fechafin/%s/estacion/%s"
URL_MAESTRO_MUNICIPIOS <- "https://opendata.aemet.es/opendata/api/maestro/municipios"
URL_MAESTRO_MUNICIPIO <- "https://opendata.aemet.es/opendata/api/maestro/municipio/%s"
URL_ESTACIONES <- "https://opendata.aemet.es/opendata/api/valores/climatologicos/inventarioestaciones/todasestaciones"


# FOLDERS and FILES -------------------------------------------------------

DIR_DATA <- here::here('data')

DIR_RAW_DATA <- file.path(DIR_DATA, 'raw')
DIR_CLEAN_DATA <- file.path(DIR_DATA, 'clean')

DIR_OUTPUTS <- here::here('outputs')

DIR_OUTPUTS_REPORTS <- file.path(DIR_OUTPUTS, 'reports')
DIR_OUTPUTS_FILES <- file.path(DIR_OUTPUTS, 'files')


FILE_AEMET_ESTACIONES <- file.path(DIR_RAW_DATA, "estaciones.Rds")
FILE_AEMET_MAESTRO_MUNICIPIOS <- file.path(DIR_RAW_DATA, "maestro_municipios.Rds")
FILE_AEMET_DATOS_ESTACIONES_1922_2021 <- file.path(DIR_RAW_DATA, "datos_estaciones_1922_2021")

FILE_ALL_DATA <- file.path(DIR_CLEAN_DATA, "all_data.Rds")
