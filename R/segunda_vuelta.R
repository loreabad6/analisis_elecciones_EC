library(rtweet)
library(tidyverse)
library(lubridate)
library(gganimate)
library(ggtext)

# Extraer tweets de Andrea Gomez
# La funcion solo busca en los ultimos nueve dias de registros,
# por eso guardo los resultados en un .rda
# conteo = search_tweets(
#   q = "angiegomeza AND ANDRES AND ARAUZ AND GALARZA",
#   include_rts = F
# )
# save(conteo, file = "data/segunda_vuelta.Rda")

load("data/segunda_vuelta.Rda")
conteo_tidy = conteo %>%
  # Seleccionar columnas de interes
  select(created_at, text) %>%
  # Cambiar a zona horaria de Ecuador
  mutate(hora_ec = with_tz(created_at, tzone = "America/Bogota")) %>%
  # Dividir texto del tweet en cada salto de linea
  mutate(tmp_chunks = str_split(tolower(text), fixed("\n"),  n = 5)) %>%
  # Crear nuevas columnas en funcion de cada linea
  mutate(
    arauz = map_chr(tmp_chunks, function(s) s[which(str_detect(s, "arauz"))][1]),
    lasso = map_chr(tmp_chunks, function(s) s[which(str_detect(s, "lasso"))][1])
  ) %>%
  # Extraer solo porcentajes
  mutate_if(is.character, str_extract, "(?<=\\().+?(?=\\))") %>%
  mutate_if(is.character, str_extract, "\\d+\\.*\\d*") %>%
  # Convertirlos en valores numericos
  mutate_if(is.character, as.numeric)

View(conteo_tidy)

# Obtener posiciones en el tiempo para graficar
max_x_position = max(conteo_tidy$hora_ec)
min_x_position = min(conteo_tidy$hora_ec)

# Truco para mantener el texto en el ultimo cuadro
# Repetir la primera fila con 4 horas mas
last_lasso = conteo_tidy$lasso[1]
last_arauz = conteo_tidy$arauz[1]

conteo_tidy2 = conteo_tidy %>%
  add_row(
    tibble_row(
      hora_ec = max_x_position + 600,
      arauz = last_arauz,
      lasso = last_lasso,
    ),
    .before = 1
  )

# Grafico del cambio en el porcentaje de votos por candidato a la segunda vuelta
g = conteo_tidy2 %>%
  select(hora_ec, arauz, lasso) %>%
  pivot_longer(
    -c(hora_ec),
    names_to = "candidato",
    values_to = "porcentaje"
  ) %>%
  ggplot(aes(x = hora_ec)) +
  geom_line(
    aes(color = candidato, y = porcentaje),
    show.legend = T, size = 1.5
  ) +
  geom_ribbon(
    data = conteo_tidy2,
    aes(ymin = lasso, ymax = arauz),
    fill = "gray", alpha = 0.5
  ) +
  geom_label(
    data = conteo_tidy2,
    aes(label = paste0(arauz, "%"), y = arauz),
    size = 4.5, nudge_x = -500
  ) +
  geom_label(
    data = conteo_tidy2,
    aes(label = paste0(lasso, "%"), y = lasso),
    size = 4.5, nudge_x = -500
  ) +
  scale_x_datetime(
    date_breaks = "1 hour", expand = expansion(mult = c(0, 0.1), add = 0),
    labels = scales::date_format("%d-%m\n%H:%M", tz = "America/Bogota")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, accuracy = 0.1)
  ) +
  scale_color_manual(
    "",
    values = c("orange", "deepskyblue3"),
    labels = c("ANDREZ ARAUZ", "GUILLERMO LASSO")
  ) +
  labs(
    caption = "Fuente: CNE, @angiegomeza. Visualización: @loreabad6"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    axis.title = element_blank(),
    text = element_text(size = 16)
  ) +
  transition_reveal(
    hora_ec,
    range(min_x_position, max_x_position),
    keep_last = F
  )

anim_save("figs/segunda_vuleta.gif", end_pause = 25,
          duration = 10,
          animation = g, rewind = F,
          width = 500, height = 400)

