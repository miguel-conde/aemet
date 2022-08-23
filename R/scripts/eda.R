
# LIBRARIES and SOURCES ---------------------------------------------------

source(here::here("R", "src", "utils_gcd.R"), encoding = "utf8")

library(sf)
library(leaflet)
library(mapSpain)
library(lubridate)
library(clock)

library(tibbletime)

library(lme4)

# DATA --------------------------------------------------------------------

all_data <- readRDS(FILE_ALL_DATA)


# FUNCTIONS ---------------------------------------------------------------

mean_roll_10 <- rollify(mean, window = 10)
mean_roll_120 <- rollify(mean, window = 120)
sd_roll_10 <- rollify(sd, window = 10)
sd_roll_120 <- rollify(sd, window = 120)

# EDA ---------------------------------------------------------------------

# Estaciones --------------------------------------------------------------

NUTS0 <- esp_get_nuts(nuts_level = 0, moveCAN = FALSE) %>% 
  st_transform(crs = 4326)
# NUTS1 <- esp_get_nuts(nuts_level = 1, moveCAN = FALSE)
# NUTS2 <- esp_get_nuts(nuts_level = 2, moveCAN = FALSE)
# CCAA_sf <- esp_get_ccaa(moveCAN = FALSE)
# 
# country <- esp_get_country()
# lines <- esp_get_can_box()

ggplot(NUTS0) +
  geom_sf(fill = "cornsilk", color = "#887e6a") +
  labs(title = "Map of Spain") +
  theme(
    panel.background = element_rect(fill = "#fffff3"),
    panel.border = element_rect(
      colour = "#887e6a",
      fill = NA,
    ),
    text = element_text(
      family = "serif",
      face = "bold"
    )
  ) +
  geom_sf(data = st_geometry(st_estaciones))

bbox <- as.double(round(
  st_bbox(NUTS0) + c(-1, -1, 1, 1), 2
))

pal = colorNumeric("RdYlBu", domain = all_data$st_estaciones$altitud)

# Start leaflet
m <- leaflet(NUTS0) %>%
  addProviderEspTiles("MDT.Relieve",
                      group = "MDT.Relieve") %>%
  addProviderEspTiles("Militar",
                      group = "Militar") %>%
  # add different provider tiles
  addProviderTiles(
    "OpenStreetMap",
    # give the layer a name
    group = "OpenStreetMap"
  ) %>%
  addProviderTiles(
    "Stamen.Toner",
    group = "Stamen.Toner"
  ) %>%
  addProviderTiles(
    "Stamen.Terrain",
    group = "Stamen.Terrain"
  ) %>%
  addProviderTiles(
    "Esri.WorldStreetMap",
    group = "Esri.WorldStreetMap"
  ) %>%
  addProviderTiles(
    "Wikimedia",
    group = "Wikimedia"
  ) %>%
  addProviderTiles(
    "CartoDB.Positron",
    group = "CartoDB.Positron"
  ) %>%
  addProviderTiles(
    "Esri.WorldImagery",
    group = "Esri.WorldImagery"
  ) %>%
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "MDT.Relieve", "Militar",
      "OpenStreetMap", "Stamen.Toner",
      "Stamen.Terrain", "Esri.WorldStreetMap",
      "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
    ),
    # position it on the topleft
    position = "topleft"
  )

# Add layers
m <- m  %>%
  addPolygons(
    color = NA,
    fillColor = "green",
    group = "Polygon"
  ) %>%
  # setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  addMarkers(data = all_data$st_estaciones, 
             # popup = ~nombre,
             label = paste(
               "Estación:",   all_data$st_estaciones$nombre, "<br>",
               "Provincia:",  all_data$st_estaciones$provincia, "<br>",
               "Altitud:",    all_data$st_estaciones$altitud, "m", "<br>",
               "Indicativo:", all_data$st_estaciones$indicativo
             )%>%
               lapply(htmltools::HTML)
  ) %>%
  addCircles(data = all_data$st_estaciones, col = ~pal(altitud), opacity = 0.9) %>%
  addLegend(data =  all_data$st_estaciones, pal = pal, values = ~altitud)

# Add additional options

m 


# Evol --------------------------------------------------------------------

all_data$evol 
kk <- all_data$evol %>% 
  group_by(indicativo) %>% 
  group_modify(~ .x %>% mutate_at(vars(tmin, tmed, tmax), 
                                  ~ ifelse(sum(!is.na(.)) >= 2,
                                           imputeTS::na_interpolation(., "spline"),
                                           .))) %>% 
  ungroup()

probe <- all_data$evol %>% 
  group_by(yr = year(fecha)) %>% 
  summarise_at(vars(tmin, tmed, tmax), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))


probe %>% 
  select(-contains("sd")) %>% 
  gather(key = t, value = val, -yr) %>% 
  ggplot(aes(x = yr, y = val, col = t)) +
  geom_line() +
  geom_smooth() +
  labs(title = "Temperaturas anuales medias",
       subtitle = "España - 1922/2021",
       x = "Año",
       y = "ºC")


probe %>% 
  select(-contains("avg")) %>% 
  gather(key = t, value = val, -yr) %>% 
  ggplot(aes(x = yr, y = val, col = t)) +
  geom_line() +
  geom_smooth() +
  geom_smooth() +
  labs(title = "Desviación Estándar Temperaturas anuales",
       subtitle = "España - 1922/2021",
       x = "Año",
       y = "ºC")

all_data$evol %>% mutate(fecha = date_group(fecha, "month")) %>% group_by(fecha)  %>% 
  summarise_at(vars(tmed), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))%>% 
  select(-contains("sd")) %>% 
  gather(key = t, value = val, -fecha) %>% 
  ggplot(aes(x = fecha, y = val, col = t)) +
  geom_line() +
  geom_smooth() + 
  geom_point()

all_data$evol %>% mutate(fecha = date_group(fecha, "year")) %>% group_by(fecha)  %>% 
  summarise_at(vars(tmed), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))%>% 
  select(-contains("sd")) %>% 
  gather(key = t, value = val, -fecha) %>% 
  ggplot(aes(x = fecha, y = val, col = t)) +
  geom_line() +
  geom_smooth() + 
  geom_point()

# Y si solo miro las estaciones que han estado en todo el periodo?

estaciones_1922 <- all_data$evol %>% 
  filter(year(fecha) == 1922) %>% 
  drop_na(tmin, tmax, tmed) %>% 
  pull(indicativo) %>% 
  unique()

all_data$evol %>% 
  filter(indicativo %in% estaciones_1922) %>% 
  drop_na(tmin, tmax, tmed) %>% 
  select(nombre) %>% 
  distinct()

ggplot(NUTS0) +
  geom_sf(fill = "cornsilk", color = "#887e6a") +
  labs(title = "Map of Spain") +
  theme(
    panel.background = element_rect(fill = "#fffff3"),
    panel.border = element_rect(
      colour = "#887e6a",
      fill = NA,
    ),
    text = element_text(
      family = "serif",
      face = "bold"
    )
  ) +
  geom_sf(data = st_geometry(st_estaciones %>% 
                               filter(indicativo %in% estaciones_1922)))

all_data$evol %>% 
  filter(indicativo %in% estaciones_1922) %>% 
  mutate(fecha = date_group(fecha, "month")) %>% 
  group_by(fecha)  %>% 
  summarise_at(vars(tmed), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))%>% 
  select(-contains("sd")) %>% 
  gather(key = t, value = val, -fecha) %>% 
  ggplot(aes(x = fecha, y = val, col = t)) +
  geom_line() +
  geom_smooth() + 
  geom_point() +
  labs(title = "Temperaturas mensuales medias",
       subtitle = "España - 1922/2021 - Solo estaciones 1922",
       x = "Año",
       y = "ºC")

all_data$evol %>% 
  filter(indicativo %in% estaciones_1922) %>% 
  mutate(fecha = date_group(fecha, "year")) %>% 
  group_by(fecha)  %>% 
  summarise_at(vars(tmin, tmed, tmax), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))%>% 
  select(-contains("sd")) %>% 
  gather(key = t, value = val, -fecha) %>% 
  ggplot(aes(x = fecha, y = val, col = t)) +
  geom_line() +
  geom_smooth() + 
  geom_point() +
  labs(title = "Temperaturas anuales medias",
       subtitle = "España - 1922/2021 - Solo estaciones 1922",
       x = "Año",
       y = "ºC")

all_data$evol %>% 
  filter(nombre == "MADRID, RETIRO") %>% 
  mutate(fecha = date_group(fecha, "month")) %>% 
  group_by(fecha)  %>% 
  summarise_at(vars(tmed), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))%>% 
  select(-contains("sd")) %>% 
  gather(key = t, value = val, -fecha) %>% 
  ggplot(aes(x = fecha, y = val, col = t)) +
  geom_line() +
  geom_smooth() + 
  geom_point() +
  labs(title = "Temperaturas mensuales medias",
       subtitle = "España - 1922/2021 - MADRID, RETIRO",
       x = "Año",
       y = "ºC")

all_data$evol %>% 
  filter(nombre == "MADRID, RETIRO") %>% 
  mutate(fecha = date_group(fecha, "year")) %>% 
  group_by(fecha)  %>% 
  summarise_at(vars(tmin, tmed, tmax), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))%>% 
  select(-contains("sd")) %>% 
  gather(key = t, value = val, -fecha) %>% 
  ggplot(aes(x = fecha, y = val, col = t)) +
  geom_line() +
  geom_smooth() + 
  geom_point() +
  labs(title = "Temperaturas anuales medias",
       subtitle = "España - 1922/2021 - MADRID, RETIRO",
       x = "Año",
       y = "ºC")

all_data$evol %>% 
  filter(nombre == "ESTACIÓN DE TORTOSA (ROQUETES)") %>% 
  mutate(fecha = date_group(fecha, "month")) %>% 
  group_by(fecha)  %>% 
  summarise_at(vars(tmed), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))%>% 
  select(-contains("sd")) %>% 
  gather(key = t, value = val, -fecha) %>% 
  ggplot(aes(x = fecha, y = val, col = t)) +
  geom_line() +
  geom_smooth() + 
  geom_point() +
  labs(title = "Temperaturas mensuales medias",
       subtitle = "España - 1922/2021 - ESTACIÓN DE TORTOSA (ROQUETES)",
       x = "Año",
       y = "ºC")

all_data$evol %>% 
  filter(nombre == "ESTACIÓN DE TORTOSA (ROQUETES)") %>% 
  mutate(fecha = date_group(fecha, "year")) %>% 
  group_by(fecha)  %>% 
  summarise_at(vars(tmin, tmed, tmax), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))%>% 
  select(-contains("sd")) %>% 
  gather(key = t, value = val, -fecha) %>% 
  ggplot(aes(x = fecha, y = val, col = t)) +
  geom_line() +
  geom_smooth() + 
  geom_point() +
  labs(title = "Temperaturas anuales medias",
       subtitle = "España - 1922/2021 - ESTACIÓN DE TORTOSA (ROQUETES)",
       x = "Año",
       y = "ºC")


## Medias móviles de temperaturas - medias y sds mensuales - solo estaciones 1922

probe_mth <- all_data$evol %>% 
  filter(indicativo %in% estaciones_1922) %>% 
  mutate(fecha = date_group(fecha, "month")) %>% 
  group_by(fecha)  %>% 
  summarise_at(vars(tmin, tmed, tmax), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))) %>% 
  mutate(tmin_120 = mean_roll_120(tmin_avg),
         tmed_120 = mean_roll_120(tmed_avg),
         tmax_120 = mean_roll_120(tmax_avg),
         tmin_sd_120 = sd_roll_120(tmin_avg),
         tmed_sd_120 = sd_roll_120(tmed_avg),
         tmax_sd_120 = sd_roll_120(tmax_avg))

gg_probe_mth <- probe_mth %>% 
  select(fecha, tmed_avg, tmed_120, tmed_sd_120) %>% 
  mutate(up = tmed_120 + 1.96 * tmed_sd_120,
         lo = tmed_120 - 1.96 * tmed_sd_120) %>% 
  select(-tmed_sd_120) 

gg_probe_mth_gather <- gg_probe_mth %>% 
  gather(t, deg, -fecha) 

ggplot(data = gg_probe_mth, aes(x = fecha, y = tmed_avg)) +
  geom_point(aes(color = "Temp. Media Mensual")) +
  geom_line(aes(color = "Temp. Media Mensual")) +
  geom_line(data = gg_probe_mth, aes(y = tmed_120, color = "Temp. Media 10 años")) +
  geom_ribbon(data = gg_probe_mth, aes(ymin=lo,ymax=up),alpha=0.3)


## Medias móviles de temperaturas - medias y sds mensuales - solo estaciones 1922

probe_yr <- all_data$evol %>%
filter(indicativo %in% estaciones_1922) %>% 
  mutate(fecha = date_group(fecha, "year")) %>% 
  group_by(fecha)  %>% 
  summarise_at(vars(tmin, tmed, tmax), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))) %>% 
  mutate(tmin_10 = mean_roll_10(tmin_avg),
         tmed_10 = mean_roll_10(tmed_avg),
         tmax_10 = mean_roll_10(tmax_avg),
         tmin_sd_10 = sd_roll_10(tmin_avg),
         tmed_sd_10 = sd_roll_10(tmed_avg),
         tmax_sd_10 = sd_roll_10(tmax_avg))

gg_probe_yr <- probe_yr %>% 
  select(fecha, tmed_avg, tmed_10, tmed_sd_10) %>% 
  mutate(up = tmed_10 + 1.96 * tmed_sd_10,
         lo = tmed_10 - 1.96 * tmed_sd_10) %>% 
  select(-tmed_sd_10) 

gg_probe_yr_gather <- gg_probe_yr %>% 
  gather(t, deg, -fecha) 

ggplot(data = gg_probe_yr, aes(x = fecha, y = tmed_avg)) +
  geom_point(aes(color = "Temp. Media Anual")) +
  geom_line(aes(color = "Temp. Media Anual")) +
  geom_line(data = gg_probe_yr, aes(y = tmed_10, color = "Temp. Media 10 años")) +
  geom_ribbon(data = gg_probe_yr, aes(ymin=lo,ymax=up),alpha=0.3)

## MODELO ANUAL NIVEL ESPAÑA - solo estaciones 1922

probe_model_anual_esp <- probe_yr %>% 
  select(fecha, tmed_avg) %>% 
  mutate(n_yr = year(fecha) - min(year(fecha)))

lm_anual_esp <- lm(tmed_avg ~ n_yr + 1, probe_model_anual_esp)
summary(lm_anual_esp)

## Descomposicion MENSUAL  NIVEL ESPAÑA - solo estaciones 1922
probe_model_mensual_esp <- probe_mth %>% 
  select(fecha, tmed_avg) %>% 
  mutate(n_yr = year(fecha) - min(year(fecha)),
         n_mth = month(fecha, label = TRUE) %>% factor(ordered = FALSE))

ts_probe_model_mensual_esp <- probe_model_mensual_esp$tmed_avg %>% 
  ts(start = c(1922, 1), frequency = 12)

stl_probe_model_mensual_esp <- stl(ts_probe_model_mensual_esp, s.window = "periodic")
plot(stl_probe_model_mensual_esp)


lm_mensual_esp <- lm(tmed_avg ~ n_yr*n_mth + 1, probe_model_mensual_esp)
summary(lm_mensual_esp)


# MODELOS -----------------------------------------------------------------

probe <- all_data$evol %>% 
  filter(indicativo %in% estaciones_1922) %>% 
  mutate(fecha = date_group(fecha, "month")) %>% 
  group_by(indicativo, nombre, fecha) %>% 
  summarise_at(vars(tmin, tmed, tmax), 
               list(avg = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(n_yr = year(fecha) - min(year(fecha)),
         mth = month(fecha, label = TRUE) %>% factor(ordered = FALSE),
         indicativo = factor(indicativo))


# No pooling --------------------------------------------------------------

f <- tmed_avg ~ n_yr + indicativo + mth + n_yr:indicativo + n_yr:mth + 1
f <- tmed_avg ~ n_yr + indicativo + mth + 1
no_pool_m_mes_est <- lm(f, probe)
summary(no_pool_m_mes_est)

# Multinivel meses y estaciones -------------------------------------------



f <- tmed_avg ~ n_yr + 1 + (n_yr + 1 | indicativo) + (n_yr + 1 | mth)

mltlvel_m_mes_est <- lmer(formula = f, data = probe)

summary(mltlvel_m_mes_est)
fixef(mltlvel_m_mes_est)
ranef(mltlvel_m_mes_est)

sapply(estaciones_1922,
       function(x) fixef(mltlvel_m_mes_est)["n_yr"] + 
         ranef(mltlvel_m_mes_est)$indicativo[x, "n_yr"] + 
         ranef(mltlvel_m_mes_est)$mth[, "n_yr"])

sapply(estaciones_1922,
       function(x) fixef(mltlvel_m_mes_est)["(Intercept)"] + 
         ranef(mltlvel_m_mes_est)$indicativo[x, "(Intercept)"] + 
         ranef(mltlvel_m_mes_est)$mth[, "(Intercept)"])
