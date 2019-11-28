# about: exploring data set

# activate libraries
library(tidyverse) # this activates dply, ggplot, and many other packages
library(googlesheets)
library(janitor)
library(magrittr) # although the pipe "%>%" is loaded with the tidyverse the double pipe "%<>%" requires magrittr to be loaded 
library(hrbrthemes) 

# selecting data set
gs_ls()

# get the Bolivia database google sheet
bd <- gs_title("Bolivia Database")

# list worksheets
gs_ws_ls(bd)

# selecting the main worksheet
df <- gs_read(ss = bd, ws = "Entries")

# shifting to a local data frame to avoid downloading the google sheet repeatedly
data <- df

# --------------------
# cleaning  
# --------------------
data %<>%
  # removing unnecesary rows
  filter(str_detect(Event, "#", negate = T)) %>% 
  # removing unnecesary columns
  select(-(55:59), -X53, -X2) %>% 
  # removing columns and rows with no values
  remove_empty()
  
# strings with english and spanish columns' names
colnames_en <- data %>% slice(1) %>% as.character()
colnames_sp <- data %>% slice(2) %>% as.character()

# translating column 24 into spanish
colnames_sp[is.na(colnames_sp)] <- "Grupo involucrado en muerte" 

# operating with spanish column names
colnames(data) <- colnames_sp

# simplifying column names ans slicing unnesecary rows
data %<>% 
  clean_names() %>% 
  slice(-(1:3))

# visualizing missing data  (percent)
map(data, is.na) %>% map(., sum) %>% map(., ~ . /522 * 100) %>% unlist %>% 
  bind_rows() %>% t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  arrange(V1) %>% 
  mutate(num = 1:nrow(.)) %>% 
  ggplot(aes(fct_reorder(rowname, num), V1)) + 
  geom_col() + 
  geom_text(aes(label = V1)) +
  coord_flip() +
  labs(
    title = "valores perdidos por columna",
    subtitle = "en porcentaje",
    x = "nombre de columna",
    y = "%"
  ) + 
  theme_ipsum_rc() + 
  ggsave("outputs/valores_perdidos_porcentaje.jpg", height = 10)

# visualizing missing data  (raw number)
map(data, is.na) %>% map(., sum) %>% unlist %>% 
  bind_rows() %>% t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  arrange(V1) %>% 
  mutate(num = 1:nrow(.)) %>% 
  ggplot(aes(fct_reorder(rowname, num), V1)) + 
  geom_col() + 
  geom_text(aes(label = V1)) +
  coord_flip() +
  labs(
    title = "valores perdidos por columna",
    subtitle = "sobre un total de 522 valores",
    x = "nombre de columna",
    y = "%"
  ) + 
  theme_ipsum_rc() + 
  ggsave("outputs/valores_perdidos_numero.jpg", height = 10)

# cleaning municipality and province names to make them compatible with official codes
ine <- rio::import("inputs/codigos.ine.xlsx")

# which municipalities have the name wrong
(data$municipalidad %>% unique)[!(data$municipalidad %>% unique) %in% (ine$MUNICIPIO %>% unique)]

# modifying municipality names
data$municipalidad %<>% gsub("Santa Cruz", "Santa Cruz de la Sierra", .)
data$municipalidad %<>% gsub("Sipe Sipe", "Sipesipe", .)
data$municipalidad %<>% gsub("Ayopaya", "Independencia", .)
data$municipalidad %<>% gsub("Chimore", "Chimoré", .)
data$municipalidad %<>% gsub("Yapacani", "Yapacaní", .)
data$municipalidad %<>% gsub("Entre Rios", "Entre Ríos", .)
data$municipalidad %<>% gsub("Ichilo", "Yapacaní", .) # Ichilo is the province
data$municipalidad %<>% gsub("Copacapana", "Copacabana", .) 
data$municipalidad %<>% gsub("San Ignacio de Moxos", "San Ignacio", .) 
data$municipalidad %<>% gsub("Santa Cruz de la Sierra de la Sierra", "Santa Cruz de la Sierra", .) 

# probando la limpieza
(data$municipalidad %>% unique)[!(data$municipalidad %>% unique) %in% (ine$MUNICIPIO %>% unique)]

# which provinces have the name wrong
(data$provincia %>% unique)[!(data$provincia %>% unique) %in% (ine$PROVINCIA %>% unique)]

# modifying province names
data$provincia %<>% gsub("José Miguel de Velasco", "Velasco", .)
data$provincia %<>% gsub("Tomas Frias", "Tomás Frías", .)
data$provincia %<>% gsub("Carcado", "Cercado", .)
data$provincia %<>% gsub("Pantaléon Dalence", "Pantaleón Dalence", .)
data$provincia %<>% gsub("Edurado Avaroa", "Abaroa", .)
data$provincia %<>% gsub("Yapacaní", "Ichilo", .) # Yapacaní is the municipality name
data$provincia %<>% gsub("Yungas", "Sud Yungas", .) # the entry belongs to Sud Yungas
data$provincia %<>% gsub("Sud Sud Yungas", "Sud Yungas", .) 
data$provincia %<>% gsub("Nor Sud Yungas", "Nor Yungas", .) 
data$provincia %<>% gsub("José Ballivián", "José Ballivian", .) 
data$provincia %<>% gsub("Gran Choco", "Gran Chaco", .) 
data$provincia %<>% gsub("Bolivar", "Bolívar", .) 
data$provincia %<>% gsub("Andrés Ibañez", "Andrés Ibáñez", .) 
data$provincia %<>% gsub("Inquisvi", "Inquisivi", .)
data$provincia %<>% gsub("Alonso de Ibañez", "Alonso de Ibáñez", .)
data$provincia %<>% gsub("Eliodoro Camacho", "Camacho", .)

(data$provincia %>% unique)[!(data$provincia %>% unique) %in% (ine$PROVINCIA %>% unique)]

# que departamentos tienen el nombre correcto
(data$departamento %>% unique)[!(data$departamento %>% unique) %in% (ine$DEPARTAMENTO %>% unique)]

# adjusting columns class
sapply(data, class)

# these need to be coerced to numeric
temp <- c("year", "month", "day", "year_later", "month_later", "day_later", "deceases_age",
          "alt_age")


data$alt_age %>% unique

# arreglar day
# arreglar year_later
# ask clarification for month_later
# arreglar day_later
# arreglar deceased_age
# arreglar alt_age

which(colnames(data) == "alt_age")
















