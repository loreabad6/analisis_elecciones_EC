library(sf)
library(tidyverse)
library(stringi)

# Leer resultados recopilados por Gabo Gaona de https://actascne.andresarauz.ec/
res_url = "https://gitlab.com/gavg712/elecciones-ecuador-2021/-/raw/master/data/csv/cne_ec_2021_presidencia.zip"
res_file = "data/cne_ec_2021_presidencia.zip"
download.file(res_url, destfile = res_file)
unzip(res_file)

resultados =  read.csv('data/csv/cne_ec_2021_presidencia.csv')

# Obtener una fila por acta con resultados por
# candidato, nulos, blancos y total de sufragantes
resultados_wider = resultados %>%
  pivot_wider(
    names_from = cod_org_pol,
    values_from = votos
  ) %>%
  select(-c(
    cod_candidato,
    porcentaje_votos_validos,
    updated,
    contains("acumulado"))
  ) %>%
  group_by(acta_numero) %>%
  summarise_all(function(x) last(na.omit(x))) %>%
  mutate_at(vars(1:9), as.factor)

# Revisar si la suma de los nulos, blancos y candidatos es
# igual al total de sufragantes
resultados_invalidos = resultados_wider %>%
  mutate(sum_votes = Reduce("+",.[11:28])) %>%
  rowwise() %>%
  mutate(
    valid = ifelse(total_sufragantes == sum_votes, TRUE, FALSE),
    diff = total_sufragantes - sum_votes
  ) %>%
  filter(!valid)

# Actas invalidas por provincia
resultados_invalidos %>%
  group_by(cod_prov) %>%
  summarise(actas_invalidas = n()) %>% View()

# --------Graficar en el mapa------------------------------------

# Descarga de poligonos de:
# https://data.humdata.org/dataset/ecuador-admin-level-2-boundaries
# https://data.humdata.org/dataset/ab3c7592-3b0c-41cd-999a-2919a6b243f2/resource/5b65ea45-5946-4b73-b38e-702ad8ad8a59/download/ecu_adm_inec_20190724_shp.zip

# Poligonos de cantones
cantones = st_read(
  'data/ecu_adminboundaries_candidate.gdb',
  layer = 'ecu_admbnda_adm2_inec_20190724'
)

country = st_read(
  'data/ecu_adminboundaries_candidate.gdb',
  layer = 'ecu_admbnda_adm1_inec_20190724'
) %>%
  st_transform(32717)

# Leer CSV preparado manualmente con
# informacion de: https://actascne.andresarauz.ec/
codigos_cant = read.csv(
  "data/csv/codigos_cantones.csv",
  fileEncoding = "UTF-8"
)

# Corregir Encoding
cantones_shape = cantones %>%
  transmute(
    cant_n = toupper(stri_trans_general(admin2Name_es, 'Any-Latin')),
    prov_n = toupper(stri_trans_general(admin1Name_es, 'Any-Latin'))
  ) %>%
  mutate(
    cant_n = case_when(
      cant_n == "RUMIÐAHUI" ~ "RUMIÑAHUI",
      cant_n == "LOGROÐO" ~ "LOGROÑO",
      TRUE ~ cant_n
    ),
    prov_n = case_when(
      cant_n == "LA CONCORDIA" ~ "SANTO DOMINGO DE LOS TSACHILAS",
      TRUE ~ prov_n
    )
  )

# Crear archivo georeferenciado con centroides de los cantones
# presentes en los registros de https://actascne.andresarauz.ec/
cantones_cent = left_join(codigos_cant, cantones_shape) %>%
  st_as_sf() %>%
  st_transform(32717) %>%
  mutate(Shape = st_centroid(Shape)) %>%
  mutate_at(1:4, as.factor)

# Codigos candidatos
cod_presidentes = read.csv(
  "https://gitlab.com/gavg712/elecciones-ecuador-2021/-/raw/master/data/csv/cne_ec_2021_candidatos_presidencia.csv",
  fileEncoding = "UTF-8"
) %>%
  select(cod_org_pol, nombre) %>%
  mutate_all(as.factor)

# Unir con registros de https://actascne.andresarauz.ec/
resultados_geom = resultados_wider %>%
  pivot_longer(
    cols = -c(1:10),
    names_to = 'candidate',
    values_to = 'votes'
  ) %>%
  group_by(cod_cant, candidate) %>%
  summarise(vote_count = sum(votes)) %>%
  left_join(cod_presidentes, by = c('candidate' = 'cod_org_pol')) %>%
  left_join(cantones_cent, by = c("cod_cant" = "cant_cod")) %>%
  st_as_sf()

# Resumir por numero de votos y ganador por canton
cantones_ganador =
  resultados_geom %>%
  group_by(cod_cant, cant_n) %>%
  summarise(
    votos_totales = sum(vote_count),
    ganador_votos = max(vote_count),
    ganador_nombre = nombre[which.max(vote_count)],
    prov_cod = first(prov_cod),
    prov_n = first(prov_n)
  ) %>%
  mutate(
    prov_frame = case_when(
      prov_cod %in% c(26:28) ~ "3. Exterior",
      prov_cod == 20 ~ "1. Insular",
      TRUE ~ "2. Continental"
    ),
    ganador_nombre = fct_drop(ganador_nombre)
  )

# Mapear
library(tmap)
main = tm_shape(filter(country, admin1Name_es != "Galapagos")) +
  tm_polygons(col = "grey80", border.alpha = 0) +
  tm_shape(filter(cantones_ganador,prov_frame == "2. Continental")) +
  # tm_shape(cantones_ganador) +
  tm_dots(
    col = "ganador_nombre",
    size = "votos_totales",
    palette = c("orangered", "purple", "green", "deepskyblue3"), #"#ffa500",
    labels = c("Xavier Hervas", "Yaku Pérez", "Andrés Arauz",  "Guillermo Lasso"),
    shapes.legend = 21, shapes.legend.fill = "grey80", shape = 21,
    perceptual = T, scale = 3, alpha = 1,
    title.size = "No. total de votos",
    title = "Ganador por cantón"
  ) +
  tm_layout(
    legend.title.fontface = 'bold',
    legend.text.size = 0.9,
    legend.outside = T,
    legend.outside.size = 0.3,
    frame = F
  ) +
  tm_credits(
    "Fuente: CNE, actascne.andresarauz.ec.\nRecopilación: @gavg712.\nVisualización: @loreabad6",
    just = "right"
  )

inset = tm_shape(filter(country, admin1Name_es == "Galapagos")) +
  tm_polygons(col = "grey80", border.alpha = 0) +
  tm_shape(filter(cantones_ganador,prov_frame == "1. Insular")) +
  # tm_shape(cantones_ganador) +
  tm_dots(
    col = "ganador_nombre",
    size = "votos_totales",
    palette = c("orangered", "purple", "green", "deepskyblue3"), #"#ffa500",
    labels = c("Xavier Hervas", "Yaku Pérez", "Andrés Arauz",  "Guillermo Lasso"),
    shapes.legend = 21, shapes.legend.fill = "grey80", shape = 21,
    perceptual = T, scale = 0.75, alpha = 1,
    title.size = "No. total de votos",
    title = "Ganador por cantón",
    legend.show = F
  ) +
  tm_layout(legend.title.fontface = 'bold', legend.width = 0.8)

vp = grid::viewport(x = 0.8, y = 0.3, width = 0.6, height = 0.5)#, just=c("right", "top"))

tmap_save(main,filename="resultados_canton.png",
          dpi=300, insets_tm=inset, insets_vp=vp,
          height=15, width=22, units="cm")

