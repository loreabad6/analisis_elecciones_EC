library(rtweet)
library(tidyverse)
library(lubridate)
library(gganimate)
library(ggtext)

# Extraer tweets de Andrea Gomez
reconteo = search_tweets(
  q = "angiegomeza AND Actualización AND Diferencia",
  include_rts = F
)
reconteo_tidy = reconteo %>%
  # Seleccionar columnas de interes
  select(created_at, text) %>%
  # Cambiar a zona horaria de Ecuador
  mutate(hora_ec = with_tz(created_at, tzone = "America/Bogota")) %>%
  # Dividir texto del tweet en cada salto de linea
  mutate(tmp_chunks = str_split(text, fixed("\n"),  n = 5)) %>%
  # Crear nuevas columnas en funcion de cada linea
  mutate(
    yaku = map_chr(tmp_chunks, function(s) s[which(str_detect(s, "Yaku"))][1]),
    lasso = map_chr(tmp_chunks, function(s) s[which(str_detect(s, "Lasso"))][1]),
    diff = map_chr(tmp_chunks, function(s) s[which(str_detect(s, "Diferencia"))][1]),
    votos_por_comp = map_chr(
      tmp_chunks,
      function(s) {
        t = s[which(str_detect(s, "Votos"))]
        if (length(t) == 0) NA_character_ else t
      }
    )
  ) %>%
  mutate(actas_por_comp = str_extract(text, "(?<=\\().+?(?=\\))")) %>%
  # Seleccionar las columnas de interes
  select(hora_ec, yaku, lasso, diff, actas_por_comp, votos_por_comp) %>%
  # Cambiar el separador "." en diferencia (es en miles)
  mutate_at(vars(diff, actas_por_comp, votos_por_comp), str_replace, "\\.", "") %>%
  # Extraer solo valores numericos
  mutate_if(is.character, str_extract, "\\d+\\.*\\d*") %>%
  # Convertirlos en valores numericos
  mutate_if(is.character, as.numeric) %>%
  # Remover los dos primeros tweets (datos por candidato en millones y no porcentaje)
  filter_at(vars(yaku, lasso), any_vars(. > 10))

View(reconteo_tidy)

# Obtener posiciones en el tiempo para graficar
max_x_position = max(reconteo_tidy$hora_ec)
min_x_position = min(reconteo_tidy$hora_ec)
susp_x_start = min_x_position + (3600*9+550)
susp_x_end = min_x_position + (3600*15+550)
susp_x_mean = min_x_position + (3600*12+550)

# Truco para mantener el texto en el ultimo cuadro
# Repetir la primera fila con 4 horas mas
last_lasso = reconteo_tidy$lasso[1]
last_yaku = reconteo_tidy$yaku[1]
last_diff = reconteo_tidy$diff[1]
last_por_comp = reconteo_tidy$votos_por_comp[1]

reconteo_tidy2 = reconteo_tidy %>%
  add_row(
    tibble_row(
      hora_ec = max_x_position + 36000,
      yaku = last_yaku,
      lasso = last_lasso,
      diff = last_diff,
      votos_por_comp = last_por_comp
    ),
    .before = 1
  )

# Grafico del cambio en el porcentaje de votos por candidato a la segunda vuelta
g = reconteo_tidy2 %>%
  mutate(label_diff = paste0(
      "Diferencia: <span style='color:",
      ifelse(diff > votos_por_comp, "#FF0000", "#0000FF"),
      "'>", diff, "</span>",
      "<br>Votos aproximados<br>por computar:<br>",
      "<span style='color:",
      ifelse(diff < votos_por_comp, "#FF0000", "#0000FF"),
      "'>", votos_por_comp, "</span>"
    )
  ) %>%
  pivot_longer(
    -c(hora_ec, diff, votos_por_comp, actas_por_comp, label_diff),
    names_to = "candidato",
    values_to = "porcentaje"
  ) %>%
  ggplot(aes(x = hora_ec)) +
  # geom_vline(xintercept = susp_x_start, color = 'grey', size = 2) +
  # geom_vline(xintercept = susp_x_end, color = 'grey', size = 2) +
  geom_line(
    aes(color = candidato, y = porcentaje),
    show.legend = T, size = 1.5
  ) +
  geom_ribbon(
    data = reconteo_tidy2,
    aes(ymin = lasso, ymax = yaku),
    fill = "gray", alpha = 0.5
  ) +
  geom_richtext(
    aes(label = label_diff),
    y = 19.95, x = max_x_position - 1800,
    size = 5, nudge_x = -5000,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  geom_label(
    data = reconteo_tidy2,
    aes(label = paste0(yaku, "%"), y = yaku),
    size = 4.5, nudge_x = -500
  ) +
  geom_label(
    data = reconteo_tidy2,
    aes(label = paste0(lasso, "%"), y = lasso),
    size = 4.5, nudge_x = -500
  ) +
  scale_x_datetime(
    date_breaks = "12 hours", expand = expansion(mult = c(0, 0.1), add = 0),
    labels = scales::date_format("%d-%m\n%H:%M", tz = "America/Bogota")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, accuracy = 0.1)
  ) +
  scale_color_manual(
    "",
    values = c("deepskyblue3", "purple"),
    labels = c("Guillermo Lasso", "Yaku Pérez")
  ) +
  # annotate('text', y = 19.9, angle = 90,
  #         x = susp_x_mean, label = "Suspención\ndel conteo") +
  labs(
    # title = "La carrera por la segunda vuelta...",
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

anim_save("conteo_votos.gif", end_pause = 25,
          duration = 15,
          animation = g, rewind = F,
          width = 700, height = 400)

# Grafico de actas procesadas a traves del tiempo
g2 = reconteo_tidy %>%
  # Calcular actas computadas desde el ultimo tweet hasta ahora
  mutate(actas_computadadas = lead(actas_por_comp) - actas_por_comp) %>%
  group_by(hora = floor_date(hora_ec, "1 hour")) %>%
  summarize(actas_por_hora = sum(actas_computadadas)) %>%
  filter(
    hora >= as.POSIXct("2021-02-11 11:00:00", tz = "America/Bogota") &
    hora <= as.POSIXct("2021-02-12 01:00:00", tz = "America/Bogota")
  ) %>%
  ggplot(aes(x = hora, y = actas_por_hora)) +
  # geom_rect(
  #   xmin = susp_x_start - 1800, ymin = -Inf,
  #   xmax = susp_x_end - 1800, ymax = Inf,
  #   fill = "grey90", alpha = 0.05
  # ) +
  # annotate('text', y = 300,
  #          x = susp_x_mean - 1800, label = "Suspención\ndel conteo") +
  geom_col(
    aes(fill = actas_por_hora),
    show.legend = F
  ) +
  geom_text(aes(label = actas_por_hora), nudge_y = 5) +
  scale_x_datetime(
    date_breaks = "1 hours", date_minor_breaks = "1 hour",
    expand = c(0.01, 0.1),
    labels = scales::date_format("%d-%m\n%H:%M", tz = "America/Bogota")
  ) +
  scale_fill_viridis_c(option = "D", direction = -1) +
  labs(
    title = "Actas computadas por hora",
    subtitle = "Desde 11-02 12:00 hasta 12-02 01:00",
    caption = "Fuente: CNE, @angiegomeza. Visualización: @loreabad6"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    legend.position = "top",
    axis.title = element_blank(),
    text = element_text(size = 13)
  )

g3 = reconteo_tidy %>%
  ggplot(aes(x = hora_ec, y = actas_por_comp)) +
  geom_line(size = 0.5, color = "grey50", show.legend = F) +
  geom_rect(
    xmin = susp_x_start, ymin = -Inf,
    xmax = susp_x_end, ymax = Inf,
    fill = "grey90", alpha = 0.05, color = NA
  ) +
  annotate('text', y = 300,
           x = susp_x_mean, label = "Suspención\ndel conteo") +
  geom_point(
    aes(color = actas_por_comp),
    size = 3, show.legend = F, group = 1,
  ) +
  annotate(
    "text", x = max(reconteo_tidy$hora_ec),
    y = min(reconteo_tidy$actas_por_comp, na.rm = T) + 100,
    label = min(reconteo_tidy$actas_por_comp, na.rm = T)
  ) +
  scale_x_datetime(
    date_breaks = "4 hours", date_minor_breaks = "1 hour",
    expand = c(0.02, 0.1),
    labels = scales::date_format("%d-%m\n%H:%M", tz = "America/Bogota")
  ) +
  scale_color_viridis_c(option = "D", direction = -1) +
  scale_y_continuous(n.breaks = 6, limits = c(0, NA)) +
  labs(
    title = "Actas por procesar",
    caption = "Fuente: CNE, @angiegomeza. Visualización: @loreabad6"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    legend.position = "top",
    axis.title = element_blank(),
    text = element_text(size = 12)
  )

ggsave(filename = "actas_por_hora_11feb.png", plot = g2, width = 22, height = 14, units = 'cm')
ggsave(filename = "actas_por_procesar.png", plot = g3, width = 22, height = 14, units = 'cm')
