library(rtweet)
library(tidyverse)
library(lubridate)
library(gganimate)

# Extraer tweets de Carlos Oporto
resultados = search_tweets(
  q = "Resultados AND Oficiales AND CNE AND carlosoporto",
  include_rts = F
)

resultados_tidy = resultados %>%
  # Seleccionar columnas de interes
  select(created_at, text) %>%
  # Cambiar a zona horaria de Ecuador
  mutate(hora_ec = with_tz(created_at, tzone = "America/Bogota")) %>%
  # Filtrar tweets con resultados de Yaku, Hervas, Lasso y Arauz
  filter(str_detect(text, "Arauz | Yaku | Lasso | Hervas")) %>%
  # Dividir texto del tweet en cada salto de linea
  mutate(tmp_chunks = str_split(text, fixed("\n"))) %>%
  # Crear nuevas columnas en funcion de cada linea
  mutate(
    arauz = map_chr(tmp_chunks, function(s) s[which(str_detect(s, "Arauz"))]),
    yaku = map_chr(tmp_chunks, function(s) s[which(str_detect(s, "Yaku"))]),
    lasso = map_chr(tmp_chunks, function(s) s[which(str_detect(s, "Lasso"))]),
    hervas = map_chr(tmp_chunks, function(s) s[which(str_detect(s, "Hervas"))])
  ) %>%
  # Seleccionar las columnas de interes
  select(-c(text, tmp_chunks, created_at)) %>%
  # Extraer solo valores numericos
  mutate_if(is.character, str_extract, "\\d+\\.*\\d*") %>%
  # Convertirlos en valores numericos
  mutate_if(is.character, as.numeric)

s = resultados_tidy %>%
  pivot_longer(
    -c(hora_ec),
    names_to = "candidato",
    values_to = "porcentaje"
  ) %>%
  group_by(hora_ec) %>%
  mutate(rank = rank(-porcentaje, ties.method = "first") * 1) %>%
  ungroup() %>%
  mutate(candidato = as.factor(candidato)) %>%
  mutate(color = case_when(
    candidato == "arauz" ~ "#ffa500",
    candidato == "lasso" ~ "deepskyblue3",
    candidato == "yaku" ~ "purple",
    candidato == "hervas" ~ "orangered"
  )) %>%
  mutate(nombre = case_when(
    candidato == "arauz" ~ "Andrés\nArauz",
    candidato == "lasso" ~ "Guillermo\nLasso",
    candidato == "yaku" ~ "Yaku\nPérez",
    candidato == "hervas" ~ "Xavier\nHervas"
  )) %>%
  ggplot(
    aes(
      x = porcentaje,
      y = as.factor(rank),
      group = candidato,
      fill = color,
      color = color
    )
  ) +
  geom_col(
    show.legend = F, alpha = 1,
    position = 'identity'
  ) +
  geom_text(
    aes(label = paste(nombre, " ")),
    show.legend = F, nudge_x = 2, size = 5
  ) +
  scale_color_identity(aesthetics = c("color", "fill")) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1, accuracy = 0.1)
  ) +
  coord_cartesian(
    xlim = c(14, 35)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    text = element_text(size = 16)
  ) +
  transition_states(
    hora_ec,
    transition_length = 15,
    # state_length = 20,
    wrap = FALSE
  ) +
  ease_aes('linear')+
  enter_fade() +
  exit_fade() +
  labs(
    subtitle = 'Hora y fecha: {closest_state}',
    caption = "Fuente: CNE, @carlosoporto. Visualización: @loreabad6"
  )

anim_save("conteo_top4.gif", end_pause = 15,
          duration = 25,
          animation = s, rewind = F,
          width = 500, height = 400)
