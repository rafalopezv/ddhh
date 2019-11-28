library(tidyverse)
library(magrittr)
library(highcharter)

df <- rio::import("outputs/data/base_limpia.xlsx")
ine <- rio::import("inputs/codigos.ine.xlsx")
ine %<>% 
  select(municipalidad = MUNICIPIO,
         CODIGO,
         departamento = DEPARTAMENTO)

df %<>% merge(., ine, all.x = T)

# número de muertos por municipio
temp <- df %>% 
  group_by(departamento, CODIGO, municipalidad) %>% 
  count() %>% 
  filter(!is.na(CODIGO)) %>% 
  rename(value = n)


# mapa
munis <- jsonlite::fromJSON("maps/municipios.339.geojson", simplifyVector = F)

highchart(type = "map") %>%
  hc_add_series(mapData = munis, showInLegend = F, data = temp,
                value = "value", joinBy = "CODIGO",
                borderColor = "transparent") %>%
  hc_colorAxis(stops = color_stops(colors = viridisLite::viridis(10, direction = -1))) %>%
  hc_title(text = "Número de muertes por municipio",
           style = list(useHTML = TRUE, fontSize = "18")) %>%
  hc_subtitle(text = "Desde 1982 a la fecha",
              style = list(useHTML = TRUE, fontSize = "16")) %>% 
  hc_chart(backgroundColor="#FFFFFF", style=list(fontFamily = "Roboto",
                                                 color = "#383733")) %>% 
  hc_tooltip(style = list(useHTML = TRUE, fontSize = "16")) %>% 
  htmlwidgets::saveWidget(here::here("demo", "muertes_municipo.html"))

# en que años muere mas gente
temp <- df %>% 
  group_by(ano) %>% 
  count() %>% 
  filter(!is.na(ano))

highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "A highcharter chart") %>% 
  hc_xAxis(categories = temp %>% pull(ano)) %>% 
  hc_add_series(data = temp %>% pull(n),
                name = "¿En que año murieron más personas?") %>% 
  hc_title(text = "Número de muertes por año",
           style = list(useHTML = TRUE, fontSize = "18")) %>%
  hc_subtitle(text = "Desde 1982 a la fecha",
              style = list(useHTML = TRUE, fontSize = "16")) %>% 
  hc_chart(backgroundColor="#FFFFFF", style=list(fontFamily = "Roboto",
                                                 color = "#383733")) %>% 
  hc_tooltip(style = list(useHTML = TRUE, fontSize = "16")) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  htmlwidgets::saveWidget(here::here("demo", "muertes_año.html"))

# bajo que prediidencia muere mas gente
df$administracion_presidencial %<>% gsub("\\(2nd\\)|\\(1st\\)", "", .)
df$administracion_presidencial %<>% gsub('"', '', .)
temp <- df %>% 
  group_by(administracion_presidencial) %>% 
  count() %>% 
  filter(!is.na(administracion_presidencial)) %>% 
  arrange(desc(n))


highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_xAxis(categories = temp %>% pull(administracion_presidencial)) %>% 
  hc_add_series(data = temp %>% pull(n)) %>% 
  hc_title(text = "Número de muertes por Presidencia",
           style = list(useHTML = TRUE, fontSize = "18")) %>%
  hc_subtitle(text = "Desde 1982 a la fecha",
              style = list(useHTML = TRUE, fontSize = "16")) %>% 
  hc_chart(backgroundColor="#FFFFFF", style=list(fontFamily = "Roboto",
                                                 color = "#383733")) %>% 
  hc_tooltip(style = list(useHTML = TRUE, fontSize = "16")) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  htmlwidgets::saveWidget(here::here("demo", "muertes_presidencia.html"))


# genero del difunto
temp <- df %>% 
  group_by(genero_del_difunto) %>% 
  count() %>% 
  ungroup() %>% 
  filter(!is.na(genero_del_difunto)) %>% 
  mutate(genero_del_difunto = case_when(
    genero_del_difunto == "F" ~ "mujeres",
    T ~ "hombres"
  ))
  

highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_xAxis(categories = temp %>% pull(genero_del_difunto)) %>% 
  hc_add_series(data = temp %>% pull(n)) %>% 
  hc_title(text = "Sexo de los difuntos",
           style = list(useHTML = TRUE, fontSize = "18")) %>%
  hc_subtitle(text = "Desde 1982 a la fecha",
              style = list(useHTML = TRUE, fontSize = "16")) %>% 
  hc_chart(backgroundColor="#FFFFFF", style=list(fontFamily = "Roboto",
                                                 color = "#383733")) %>% 
  hc_tooltip(style = list(useHTML = TRUE, fontSize = "16")) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  htmlwidgets::saveWidget(here::here("demo", "muertes_sexo.html"))







  







