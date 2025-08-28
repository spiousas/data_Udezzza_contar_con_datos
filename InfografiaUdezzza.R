# Datos y graficos

pacman::p_load(Hmisc, plotly, showtext, tidyverse, forcats, lubridate, haven,
               labelled, hms, here,ggpubr,gridExtra, ggbeeswarm, PerformanceAnalytics,
               patchwork,dagitty, ggdag, echarts4r, scales, colorspace)

# Este script genera la base de datos que vamos a usar llamando al resto de los scripts anidados
source(here("R/metascript.R"))

# Nuestro data frame para trabajar entonces, se llama "newdata", 
# con la que creamos "data_label" según grupo de sueño

# Clasificamos en bien y mal dormidos segun indice de calidad-----
newdata$cal_invertido <- 21 - newdata$cal_indice

data_label <- newdata %>%
  mutate(grupo_sueno = case_when(
    cal_invertido >= 16 ~ "Bien Dormido",
    cal_invertido <= 13 ~ "Mal Dormido",
    cal_invertido > 13 & cal_invertido < 16 ~ "Intermedio"
  ))

# Fuente a utilizar------
library(sysfonts)
library(showtext)
library(showtextdb)
font_add("ibm", 'ibm.ttf')

# Paleta personalizada para los grupos
colores_grupos <- c(
  "Bien Dormido" = "#024059",
  "Intermedio" = "#D6CBB8",
  "Mal Dormido" = "#ED8B16"
)

# Grafico 1: Distribucion de duracion de sueño-----

# Crear sleep_duration en horas
data <- data %>%
  drop_na(demo_edad) %>%
  filter(demo_edad >= 16, record_id > 17) %>% 
  mutate(sleep_duration = as.numeric(calidad_horas_suenio) / 3600) %>%
  mutate(sleep_duration = if_else(sleep_duration >= 3 & sleep_duration <= 16, sleep_duration, NA))

# Filtrar solo estudiantes
data_estudiantes <- data %>%
  filter(demo_rol.factor == "Estudiante" & !is.na(sleep_duration))

# Calcular el promedio general
promedio_general <- mean(data_estudiantes$sleep_duration, na.rm = TRUE)

# Crear variable categórica para la duración del sueño
data_estudiantes <- data_estudiantes %>%
  mutate(sleep_cat = factor(
    ifelse(sleep_duration < 7, "< 7 horas", "≥ 7 horas"),
    levels = c("< 7 horas", "≥ 7 horas")
  ))

conteo_sleep <- data_estudiantes |>
  count(sleep_cat)

# Añadir columna x con coordenada diferente según grupo
conteo_sleep <- conteo_sleep |>
  mutate(
    x = ifelse(sleep_cat == "< 7 horas", 3.2, 10.8),  # izquierda/derecha
    y = 1  # altura vertical fija
  )

# Crear gráfico
set.seed(123)
jitter_duration <- ggplot(data_estudiantes, aes(x = sleep_duration, y = 1, color = sleep_cat, fill = sleep_cat)) +
  # Puntos
  geom_jitter(width = 0.05, height = 0.2, size = 2.8, alpha = 0.5, pch = 21) +
  
  # Línea punteada gris en 7 horas
  geom_vline(xintercept = 7, color = "gray40", linetype = "dashed", linewidth = 1) +
  # Anotaciones con n por grupo
  geom_text(
    data = conteo_sleep,
    aes(x = x, y = y, label = paste0("n = ", n), color = sleep_cat),
    inherit.aes = FALSE,
    hjust = ifelse(conteo_sleep$sleep_cat == "< 7 horas", 0, 1),  # alinear izq o der
    size = 14 / .pt,
    family = "ibm", 
    fontface = "bold",
    show.legend = FALSE
  ) +
  # Texto gris a la derecha de la línea
  annotate(
    "text",
    x = 7.15, y = 1.35,
    label = "Mínimo recomendado",
    hjust = 0, size = 12 / .pt, color = "gray40", family = "ibm"
  ) +
  # Colores
  scale_color_manual(
    values = c("≥ 7 horas" = darken("#7DA3B0",amount = 0.3), "< 7 horas" = darken("#D6CBB8", amount = 0.3)),
    name = "Duración del sueño"
  ) +
  scale_fill_manual(
    values = c("≥ 7 horas" = "#7DA3B0", "< 7 horas" = "#D6CBB8"),
    name = "Duración del sueño"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 5)  # Cambia el número según lo grande que quieras los puntos
    )
  ) +
  # Ejes y estética
  scale_x_continuous(breaks = seq(3, 11, 1), limits = c(3, 11)) +
  labs(
    title = "Duración del sueño en estudiantes",
    x = "Duración del sueño (en horas)", y = NULL
  ) +
  theme_minimal(base_size = 30, base_family = "ibm") +
  theme(
    aspect.ratio = 0.15,
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
    legend.position = "bottom",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black")
  )
jitter_duration

ggsave(
  "jitter_duracion.png",
  plot = jitter_duration,
  width = 9,
  height = 4,  # respeta la proporción visual
  dpi = 300,
  bg = "transparent"
)

# Grafico 2: Cronotipos

cronotipo_total <- data_label |>
  dplyr::filter(!is.na(cronotipo)) |>
  dplyr::count(cronotipo) |>
  dplyr::mutate(
    pct = round(n / sum(n) * 100, 1)
  )

cronotipo_total$cronotipo <- factor(
  cronotipo_total$cronotipo,
  levels = c("Nocturno","Vespertino","Intermedio","Matutino")
) |> fct_rev()


plot_cronotipo3 <- ggplot(cronotipo_total, aes(x = cronotipo, y = pct, fill = cronotipo)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, size = 12) +
  labs(
    x = "Cronotipo",
    y = NULL,
    title = NULL,
    fill = NULL
  ) +
  scale_fill_manual(values = c(
    "Matutino" = "#D6CBB8",
    "Intermedio" = "#7DA3B0",
    "Vespertino" = "#024059",
    "Nocturno" = "#81B29A"
  )) +
  ylim(0, 75) +
  theme_minimal(base_size = 30, base_family = "ibm") +
  theme(
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 40),
    axis.text.y = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_cronotipo3

ggsave(
  filename = "plot_cronotipo.png", 
  plot = plot_cronotipo3, 
  width = 5,
  height = 4, 
  dpi = 300, 
  bg = "transparent" 
)


# Grafico 3: Alarma

#Posponer alarma----
data_label %>%
  group_by(grupo_sueno) %>%
  summarize(
    promedio_posponer = mean(habsuenio_posponer, na.rm = TRUE),) %>%
  arrange(desc(promedio_posponer))

data_label %>%
  filter(!is.na(grupo_sueno)) %>%  # Excluye los NA en grupo_sueno
  group_by(grupo_sueno, habsuenio_posponer) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(grupo_sueno) %>% 
  mutate(porcentaje = (n / sum(n)) * 100) %>%  
  arrange(grupo_sueno, habsuenio_posponer)

#De los bien dormidos, el 40,3% nunca pospone y el 50,6% pospone de 1 a 3 veces
#De los mal dormidos, el 26,8% nunca pospone, el 50,1% pospone de 1 a 3 veces y el 17,7% pospone de 3 a 6 veces

#preungta: de los que posponen mas de tres veces, que % pertenece a que grupo?
barra_posponer <- data_label %>%
  filter(habsuenio_posponer > 2, !is.na(grupo_sueno)) %>%
  mutate(grupo_sueno = fct_relevel(grupo_sueno, "Mal Dormido", "Intermedio", "Bien Dormido")) %>%
  count(grupo_sueno) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1))

posponer_plt <- ggplot(barra_posponer, aes(x = 1, y = porcentaje, fill = grupo_sueno)) +
  geom_col(width = 0.2, position = position_stack(reverse = TRUE), color = "white") +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    position = position_stack(reverse = TRUE, vjust = 0.5),
    color = "white", size = 13, fontface='bold'
  ) +
  scale_fill_manual(
    values = c("Mal Dormido" = "#ED8B16",
               "Intermedio" = "#D6CBB8",
               "Bien Dormido" = "#024059"),
    breaks = c("Mal Dormido", "Intermedio", "Bien Dormido")
  ) +
  coord_flip() +
  labs(
    title = NULL,
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal(base_size= 30, base_family = "ibm") +
  theme(
    aspect.ratio = 0.15,
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.text =  element_text(size=40),
    legend.position = "bottom",
    plot.margin = margin(10, 20, 10, 20)
  )

posponer_plt

ggsave("grafico_posponer.png", 
       plot = posponer_plt, 
       width = 5, height = 2, dpi = 300, 
       bg = "transparent")

# Grafico 4: Como se despiertan------

# Preparamos tabla para gráfico
# Etiquetas explícitas para eje x
data_label <- data_label %>%
  mutate(habsuenio_despertarse = factor(
    habsuenio_despertarse,
    levels = c(1, 2, 3),
    labels = c("Cansado", "Normal", "Energético")
  ))

despertarse_plot_data <- data_label %>%
  filter(grupo_sueno %in% c("Bien Dormido", "Mal Dormido"),
         !is.na(habsuenio_despertarse)) %>%
  mutate(grupo_sueno = factor(grupo_sueno, levels = c("Mal Dormido", "Bien Dormido"))) %>%
  group_by(grupo_sueno, habsuenio_despertarse) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(grupo_sueno) %>%
  mutate(porcentaje = (n / sum(n)) * 100,
         porcentaje_plot = ifelse(grupo_sueno == "Bien Dormido", porcentaje, -porcentaje))

plot_mal <- despertarse_plot_data %>%
  filter(grupo_sueno == "Mal Dormido") %>%
  ggplot(aes(x = porcentaje_plot, y = habsuenio_despertarse)) +
  geom_col(fill = "#ED8B16", width = 0.7) +
  geom_text(
    aes(label = paste0(round(abs(porcentaje), 1), "%")),
    hjust = 1.1,
    size = 7,
    color = "black",
    family='ibm'
  ) +
  scale_x_continuous(limits = c(-80, 0), labels = abs, expand = c(0, 0)) +
  scale_y_discrete(limits = rev(levels(despertarse_plot_data$habsuenio_despertarse)))+
  theme_minimal(base_size = 30, base_family = "ibm") +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(r = 0)
  )

plot_bien <- despertarse_plot_data %>%
  filter(grupo_sueno == "Bien Dormido") %>%
  ggplot(aes(x = porcentaje_plot, y = habsuenio_despertarse)) +
  geom_col(fill = "#024059", width = 0.7) +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    hjust = -0.1,
    size = 7,
    color = "black",
    family='ibm'
  ) +
  scale_x_continuous(limits = c(0, 80), expand = c(0, 0)) +
  scale_y_discrete(limits = rev(levels(despertarse_plot_data$habsuenio_despertarse)))+
  theme_minimal(base_size = 30, base_family = "archivo") +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(l = 0)
  )

etiquetas_centro <- data.frame(
  habsuenio_despertarse = factor(
    c("Cansado", "Normal", "Energético"),
    levels = c("Cansado", "Normal", "Energético")
  ),
  label = c("Cansado", "Normal", "Energético")
)

plot_etiquetas <- ggplot(etiquetas_centro, aes(x = 3, y = habsuenio_despertarse, label = label)) +
  geom_text(size = 7, family='ibm',vjust=0.5) +
  scale_y_discrete(limits = rev(levels(etiquetas_centro$habsuenio_despertarse))) +
  theme_void() +
  theme(plot.margin = margin(l = 4, r = 0))


grafico_final <- plot_mal + plot_etiquetas + plot_bien +
  plot_layout(widths = c(1.17, 0.3, 1)) 


ggsave("grafico_despertarse.png", 
       plot = grafico_final, 
       width = 40, height = 10, unit='cm',dpi = 300,  
       bg = "transparent")

# Grafico 5: Cafe

#Consumo de café

porcentajes_cafe <- data_label %>%
  filter(grupo_sueno %in% c("Bien Dormido", "Mal Dormido"),
         !is.na(hab_cafe)) %>%
  group_by(grupo_sueno, hab_cafe) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(grupo_sueno) %>%
  mutate(porcentaje = n / sum(n) * 100)

library(echarts4r)

# Path de la taza (extraemos solo el atributo `d`)
coffee_path <- 'path://M12.874 6.999c4.737-4.27-.979-4.044.116-6.999-3.781 3.817 1.41 3.902-.116 6.999zm-2.78.001c3.154-2.825-.664-3.102.087-5.099-2.642 2.787.95 2.859-.087 5.099zm8.906 2.618c-.869 0-1.961-.696-1.961-1.618h-10.039c0 .921-1.13 1.618-2 1.618v1.382h14v-1.382zm-13 2.382l2.021 12h7.959l2.02-12h-12z'

# Tus datos
cafe <- data.frame(
  grupo = c("Bien Dormido", "Mal Dormido"),
  porcentaje = c(30.2, 35.2),
  path = coffee_path,
  color = c("#024059", "#ED8B16")  # azul y naranja
)

# Gráfico
plot_cafe <- cafe %>%
  e_charts(grupo) %>%
  e_x_axis(show = FALSE) %>%
  e_y_axis(max = 100, show = FALSE) %>%
  # Color del café relleno
  e_color(color = c("#024059" )) %>%
  
  # Parte rellena
  e_pictorial(porcentaje,
              symbol = path,
              z = 10,
              symbolBoundingData = 100,
              symbolClip = TRUE,
              name = 'relleno') %>%
  
  # Parte fondo gris
  e_pictorial(porcentaje,
              symbol = path,
              symbolBoundingData = 100,
              name = 'fondo',
              itemStyle = list(color = "#a6a6a6")) %>%
  
  # Etiquetas inferiores con los grupos
  e_labels(position = "bottom", offset = c(0, 10),
           textStyle = list(fontSize = 16,
                            fontFamily = 'Arial',
                            fontWeight = 'bold',
                            color = "#024059"),
           formatter = "{@[0]}") %>%
  
  # Línea invisible + etiqueta del valor a la izquierda y derecha
  e_mark_line(serie = 0, # para la primera barra
              data = list(list(
                yAxis = cafe$porcentaje[1],
                xAxis = -0.2,
                label = list(formatter = paste0(round(cafe$porcentaje[1], 1), "%"),
                             position = "start",
                             color = "#3A6A75",
                             fontSize = 14,
                             fontWeight = "bold")
              ))) %>%
  
  e_mark_line(serie = 0, # para la segunda barra
              data = list(list(
                yAxis = cafe$porcentaje[2],
                xAxis = 1.2,
                label = list(formatter = paste0(round(cafe$porcentaje[2], 1), "%"),
                             position = "end",
                             color = "black",
                             fontSize = 14,
                             fontWeight = "bold")
              ))) %>%
  
  e_legend(show = FALSE) %>%
  e_title("Consumo de café según calidad del sueño", left = "center")
plot_cafe

# Grafico 6: Medialuna/Desayuno

# Grafico 7: Iconos persona siesta 



# Grafico 8: Duracion de siestas por grupo

data_label$daily_horas_siesta <- data_label$demo_horas_siesta / 7

siesta_rangos <- data_label %>%
  filter(habsuenio_siesta == 1,  # Solo quienes dijeron que sí
         !is.na(daily_horas_siesta),
         grupo_sueno %in% c("Bien Dormido", "Mal Dormido")) %>%
  mutate(rango_siesta = case_when(
    daily_horas_siesta <= 0.5 ~ "Menos de media hora",
    daily_horas_siesta > 0.5 & daily_horas_siesta <= 1 ~ "Media hora - 1 hora",
    daily_horas_siesta > 1 & daily_horas_siesta <= 2.99 ~ "1-2 horas",
    daily_horas_siesta >= 3 ~ "Más de 3 horas"
  )) %>%
  mutate(rango_siesta = factor(rango_siesta, levels = c("Menos de media hora", "Media hora - 1 hora", "1-2 horas", "Más de 3 horas")))

# Reordenamos los factores
siesta_rangos_tabla <- siesta_rangos %>%
  group_by(grupo_sueno, rango_siesta) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(grupo_sueno) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  mutate(grupo_sueno = factor(grupo_sueno, levels = c("Mal Dormido", "Bien Dormido")))  # Ordenar el grupo

# 1. Preparar datos
siesta_rangos_tabla_plot <- siesta_rangos_tabla %>%
  mutate(pct_plot = ifelse(grupo_sueno == "Mal Dormido", -pct, pct))

orden_rangos = c(
  "Menos de media hora",
  "Media hora - 1 hora",
  "1-2 horas",
  "Más de 3 horas"
)

siesta_rangos_tabla_plot <- siesta_rangos_tabla_plot %>%
  mutate(
    rango_siesta = fct_rev(factor(rango_siesta, levels = orden_rangos))
  )

# 2. Gráfico para "Mal Dormido" (lado izquierdo, naranja)
plot_mal_dormido <- siesta_rangos_tabla_plot %>%
  filter(grupo_sueno == "Mal Dormido") %>%
  ggplot(aes(x = pct_plot, y = rango_siesta)) +
  geom_col(fill = "#ED8B16", width = 0.7) +
  geom_text(
    aes(label = paste0(pct, "%")),
    hjust = 1.2,  # Texto fuera de la barra (derecha)
    size = 7,
    color = "black",
    family='ibm'
  ) +
  scale_x_continuous(labels = abs, limits = c(-75, 0), expand = c(0, 0)) +
  scale_y_discrete(limits = rev) + 
  theme_minimal(base_size = 14, base_family = 'ibm') +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x= element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(r = 0)  # Eliminar margen derecho
  )

# 3. Gráfico para "Bien Dormido" (lado derecho, azul)
plot_bien_dormido <- siesta_rangos_tabla_plot %>%
  filter(grupo_sueno == "Bien Dormido") %>%
  ggplot(aes(x = pct_plot, y = rango_siesta)) +
  geom_col(fill = "#024059", width = 0.7) +
  geom_text(
    aes(label = paste0(pct, "%")),
    hjust = -0.2,  # Texto fuera de la barra (izquierda)
    size = 7,
    color = "black",
    family='ibm'
  ) +
  scale_x_continuous(limits = c(0, 75), expand = c(0, 0)) +
  scale_y_discrete(limits = rev) + 
  theme_minimal(base_size = 14, base_family = 'ibm') +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(l = 0)  # Eliminar margen izquierdo
  )

# 4. Etiquetas centrales personalizadas
# Asumiendo que 'rango_siesta' es un factor con niveles ordenados
etiquetas_centro <- data.frame(
  rango_siesta = levels(siesta_rangos_tabla_plot$rango_siesta),
  #ordeno los niveles con el nuevo label
  label = c("<30'", "30'-1h", "1h-2h", ">3h")
)

etiquetas_centro <- data.frame(
  rango_siesta = factor(
    c("Menos de media hora", "Media hora - 1 hora", "1-2 horas", "Más de 3 horas"),
    levels = c("Menos de media hora", "Media hora - 1 hora", "1-2 horas", "Más de 3 horas")
  ),
  label = c("<30'", "30'-1h", "1h-2h" , ">3h")
)

plot_etiquetas <- ggplot(etiquetas_centro, aes(x = 1, y = rango_siesta, label = label)) +
  geom_text(size = 7, family='ibm') +
  theme_void() +
  theme(plot.margin = margin(l = 5, r = 5))  # Ajustar márgenes

# 5. Combinar con patchwork
grafico_final <- plot_mal_dormido + plot_etiquetas + plot_bien_dormido +
  plot_layout(widths = c(4, 0.9, 4)) +  # Ajustar proporciones
  plot_annotation(
    title = NULL
  )

grafico_final

# Guardar
ggsave(
  "plot_siesta_centrado.png",
  grafico_final,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "transparent"
)

# Grafico 9: Consumo de mate

#Mate----
mate_path <- "path://M70,15L70,10L53.333,10L53.333,15L50,20L73.333,20Z M85,35v5h-5V26.666c0-1.832-1.501-3.332-3.333-3.332H46.666c-1.832,0-3.332,1.5-3.332,3.332v16.668h3.604l2.386,7.154C52.995,53.994,55,58.503,55,63.333c0,6.494-3.398,12.621-9.078,16.696c0.43,1.536,0.998,2.907,1.691,4.05l0.721,1.194V90h28.333C78.499,90,80,88.499,80,86.667V73.333h5v5l5-1.666V36.666L85,35z M61.667,43.334l-8.334-5l8.334-5l8.333,5L61.667,43.334z M80,68.333V45h5v23.333H80z M50,63.333c0-3.912-1.908-7.447-5-10l-1.666-4.999H28.135l-1.533-5.73c0.756-0.992,1.117-2.271,0.768-3.57c-0.367-1.363-1.396-2.32-2.633-2.77C22.25,28.428,18.623,21.256,14.029,15L10,17.963c4.418,6.008,7.715,12.672,9.975,19.791c-0.682,0.971-0.986,2.197-0.654,3.438c0.348,1.299,1.299,2.227,2.451,2.705l1.188,4.438h-6.293L15,53.333c-3.09,2.553-5,6.088-5,10c0,6.055,3.883,11.335,9.678,14.255c-0.465,3.323-1.445,6.491-3.012,9.079V90h26.668v-3.333c-1.563-2.588-2.547-5.756-3.008-9.079C46.117,74.668,50,69.385,50,63.333z"

porcentajes_mate <- data_label %>%
  filter(!is.na(hab_mate), !is.na(grupo_sueno)) %>%
  group_by(grupo_sueno, hab_mate) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(grupo_sueno) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()

# Tus datos
mate <- data.frame(
  grupo = c("Bien Dormido", "Mal Dormido"),
  porcentaje = c(21.6, 25.6),
  path = mate_path,
  color = c("#ED8B16", "#ED8B16")  # azul y naranja
)

# Gráfico
plot_mate <- mate %>%
  e_charts(grupo) %>%
  e_x_axis(show = FALSE) %>%
  e_y_axis(max = 100, show = FALSE) %>%
  # Color del café relleno
  e_color(color = c("#024059")) %>%
  
  # Parte rellena
  e_pictorial(porcentaje,
              symbol = path,
              z = 10,
              symbolBoundingData = 100,
              symbolClip = TRUE,
              name = 'relleno') %>%
  
  # Parte fondo gris
  e_pictorial(porcentaje,
              symbol = path,
              symbolBoundingData = 100,
              name = 'fondo',
              itemStyle = list(color = "#a6a6a6")) %>%
  
  # Etiquetas inferiores con los grupos
  e_labels(position = "bottom", offset = c(0, 10),
           textStyle = list(fontSize = 16,
                            fontFamily = 'Arial',
                            fontWeight = 'bold',
                            color = "#024059"),
           formatter = "{@[0]}") %>%
  
  # Línea invisible + etiqueta del valor a la izquierda y derecha
  e_mark_line(serie = 0, # para la primera barra
              data = list(list(
                yAxis = mate$porcentaje[1],
                xAxis = -0.2,
                label = list(formatter = paste0(round(mate$porcentaje[1], 1), "%"),
                             position = "start",
                             color = "#3A6A75",
                             fontSize = 14,
                             fontWeight = "bold")
              ))) %>%
  
  e_mark_line(serie = 0, # para la segunda barra
              data = list(list(
                yAxis = mate$porcentaje[2],
                xAxis = 1.2,
                label = list(formatter = paste0(round(mate$porcentaje[2], 1), "%"),
                             position = "end",
                             color = "black",
                             fontSize = 14,
                             fontWeight = "bold")
              ))) %>%
  
  e_legend(show = FALSE) %>%
  e_title("Consumo de café según calidad del sueño", left = "center")
plot_mate

# Grafico 10: Ejercicio

ejercicio <- data_label %>%
  filter(grupo_sueno %in% c("Bien Dormido", "Mal Dormido"),
         !is.na(hab_ejercicio)) %>%
  count(grupo_sueno, hab_ejercicio) %>%
  group_by(grupo_sueno) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  mutate(hab_ejercicio = case_when(
    hab_ejercicio == 1 ~ "No realiza",
    hab_ejercicio == 2 ~ "1 a 3 días",
    hab_ejercicio == 3 ~ "4 a 6 días",
    hab_ejercicio == 4 ~ "Todos los días",
    TRUE ~ as.character(hab_ejercicio)
  ))
ejercicio <- ejercicio %>%
  mutate(hab_ejercicio = factor(hab_ejercicio, levels = c(
    "No realiza",
    "1 a 3 días",
    "4 a 6 días",
    "Todos los días"
  )))

# Preparar el objeto con posición del texto ajustada
ejercicio_plot <- ejercicio %>%
  mutate(
    grupo_sueno = factor(grupo_sueno, levels = c("Mal Dormido", "Bien Dormido")),
    pct_plot = ifelse(grupo_sueno == "Bien Dormido", pct, -pct),
    pos_texto = ifelse(grupo_sueno == "Bien Dormido", pct_plot + 5, pct_plot - 5)
  )

ejercicio_mal <- ejercicio_plot %>%
  filter(grupo_sueno == "Mal Dormido") %>%
  ggplot(aes(x = pct_plot, y = hab_ejercicio)) +
  geom_col(fill = "#ED8B16", width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = 1.2, size = 8, color = "black", family='ibm') +
  scale_x_continuous(labels = abs, limits = c(-75, 0), expand = c(0, 0)) +
  theme_minimal(base_family = "ibm") +
  theme(
    axis.title = element_blank(), 
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(r = 0, l = 10)
  )

ejercicio_bien <- ejercicio_plot %>%
  filter(grupo_sueno == "Bien Dormido") %>%
  ggplot(aes(x = pct_plot, y = hab_ejercicio)) +
  geom_col(fill = "#024059", width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.2, size = 8, color = "black",family='ibm') +
  scale_x_continuous(limits = c(0, 75), expand = c(0, 0)) +
  theme_minimal(base_family = "ibm") +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.text.x= element_blank(),
    plot.margin = margin(l = 0, r = 10)
  )

etiquetas_ejercicio <- data.frame(
  hab_ejercicio = unique(ejercicio_plot$hab_ejercicio),
  label = unique(ejercicio_plot$hab_ejercicio)
)

plot_etiquetas_ej <- ggplot(etiquetas_ejercicio, aes(x = 1, y = hab_ejercicio, label = label)) +
  geom_text(size = 8, family='ibm') +
  theme_void() +
  theme(plot.margin = margin(l = 8, r = 8))

ejercicio_final <- ejercicio_mal + plot_etiquetas_ej + ejercicio_bien +
  plot_layout(widths = c(4, 1.9, 4)) +
  plot_annotation(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

ejercicio_final

ggsave(
  filename = "plot_ejercicio.png", # nombre del archivo
  plot = ejercicio_final, # tu objeto ggplot
  width = 25,# ancho en pulgadas (ajustá según necesidad)
  height = 11, 
  units='cm', 
  dpi = 500,# resolución alta 
  bg = 'transparent'
)

# Grafico 11: Somnolencia diruna

# 1. Clasificar niveles de somnolencia en ambos grupos
data_label <- data_label %>%
  mutate(
    nivel_somno = cut(
      somno_indice,
      breaks = c(-Inf, 10, 15, Inf),
      labels = c("Bajo", "Moderado", "Excesivo")
    )
  )

# 2. Calcular porcentajes por grupo y nivel
tabla_somno_plot <- data_label %>%
  group_by(grupo_sueno, nivel_somno) %>%
  summarise(pct = n(), .groups = "drop") %>%
  group_by(grupo_sueno) %>%
  mutate(pct = round(pct / sum(pct) * 100, 1)) %>%
  ungroup()

# 3. Eje simétrico
niveles_somno <- c("Bajo", "Moderado", "Excesivo")

# Asegurar orden correcto y que no falte ningún nivel
tabla_somno_plot <- tabla_somno_plot %>%
  mutate(
    nivel_somno = factor(nivel_somno, levels = niveles_somno),
    pct_plot = ifelse(grupo_sueno == "Mal Dormido", -pct, pct)
  )

# Verificá que estén todos los niveles para cada grupo:
table(tabla_somno_plot$grupo_sueno, tabla_somno_plot$nivel_somno)

tabla_somno_plot <- tabla_somno_plot %>% filter(!is.na(nivel_somno))


# 4. Gráfico lado izquierdo: Mal Dormido 
plot_mal <- tabla_somno_plot %>%
  filter(grupo_sueno == "Mal Dormido") %>%
  ggplot(aes(x = pct_plot, y = nivel_somno)) +
  geom_col(fill = "#ED8B16", width = 0.8) +
  #geom_text(aes(label = paste0(pct, "%")),
  # hjust = 1.2, size = 8, color = "black") +
  scale_x_continuous(labels = abs, limits = c(-75, 0), expand = c(0, 0)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(r = 0)
  )

# 5. Gráfico lado derecho: Bien Dormido
plot_bien <- tabla_somno_plot %>%
  filter(grupo_sueno == "Bien Dormido") %>%
  ggplot(aes(x = pct_plot, y = nivel_somno)) +
  geom_col(fill = "#024059", width = 0.8) +
  #geom_text(aes(label = paste0(pct, "%")),
  # hjust = -0.2, size = 8, color = "black") +
  scale_x_continuous(limits = c(0, 92), expand = c(0, 0)) +
  theme_minimal(base_size = 14, base_family = "roboto") +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(l = 0)
  )

# 6. Etiquetas centrales
etiquetas_centro <- data.frame(
  nivel_somno = factor(c("Bajo", "Moderado", "Excesivo"), levels = c("Bajo", "Moderado", "Excesivo")),
  label = c("Bajo", "Moderado", "Excesivo")
)

plot_labels <- ggplot(etiquetas_centro, aes(x = 1, y = nivel_somno, label = label)) +
  geom_text(size = 7, family='ibm') +
  theme_void() +
  theme(plot.margin = margin(l = 6, r = 2))
plot_labels
# 7. Combinar
plot_bien <- plot_bien + coord_cartesian(clip = "off")
plot_bien <- plot_bien +
  theme(
    plot.margin = margin(t = 5, r = 30, b = 5, l = 5)
  )

grafico_somno_final <- plot_mal + plot_labels + plot_bien +
  plot_layout(widths = c(6, 1.2, 5.5)) +
  plot_annotation(
    title = NULL
  )

grafico_somno_final
# 8. Guardar
ggsave("grafico_somno_espejo.png",
       plot = grafico_somno_final,
       width = 12, height = 4, dpi = 300,units='cm', bg = "transparent")

# Grafico 12: Uso de pantallas

# 1. Preparar los datos y generar rangos
data_heatmap <- data_label %>%
  filter(grupo_sueno %in% c("Bien Dormido", "Mal Dormido"), !is.na(pantalla_indice)) %>%
  mutate(
    pantalla_range = cut(
      pantalla_indice,
      breaks = seq(5, 30, by = 5),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("5-10", "10-15", "15-20", "20-25", "25-30")
    )
  )

# 2. Agrupar y calcular n y media
pantalla_summary <- data_heatmap %>%
  group_by(grupo_sueno, pantalla_range) %>%
  summarise(
    n = n(),
    media_pantalla = round(mean(pantalla_indice, na.rm = TRUE), 2),
    .groups = "drop"
  )

# 3. Completar combinaciones faltantes (para que no falte la celda vacía)
pantalla_summary_completo <- pantalla_summary %>%
  complete(
    grupo_sueno = c("Bien Dormido", "Mal Dormido"),
    pantalla_range = factor(pantalla_range, levels = c("5-10", "10-15", "15-20", "20-25", "25-30"))
  ) %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    media_pantalla = ifelse(is.na(media_pantalla), NA, media_pantalla),
    label = paste0("n = ", n))


pantalla_summary_completo <- pantalla_summary_completo %>%
  mutate(
    text_color = ifelse(n >= 129, "white", "#363636")
  )

# 4. Mal dormidos (naranja)
mal_dormidos_plot <- pantalla_summary_completo %>%
  filter(grupo_sueno == "Mal Dormido") %>%
  ggplot(aes(x = pantalla_range, y = grupo_sueno, fill = n)) +
  geom_tile(color = "white")  + 
  geom_text(aes(label = label, color = text_color), size = 7) +
  scale_color_identity() +
  scale_fill_gradient(low = "#FFE0C7", high = "#ED8B16") +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 16, base_family = "ibm") +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

# 5. Bien dormidos (azul)
bien_dormidos_plot <- pantalla_summary_completo %>%
  filter(grupo_sueno == "Bien Dormido") %>%
  ggplot(aes(x = pantalla_range, y = grupo_sueno, fill = n)) +
  geom_tile(color = "white") + 
  geom_text(aes(label = label, color = text_color), size = 7) +
  scale_color_identity() +
  scale_fill_gradient(low = "#D7EAF2", high = "#024059") +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 15, base_family = "roboto") +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x= element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

# 6. Combinar con patchwork
plot_heatmap_final <- mal_dormidos_plot / bien_dormidos_plot +
  plot_annotation(
    title = "Distribución por uso de pantalla y grupo de sueño",
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5, family = "roboto"),
      plot.margin = margin(20, 20, 20, 20)
    )
  )

# Mostrar
plot_heatmap_final <- plot_heatmap_final + 
  theme(plot.background = element_rect(fill = "transparent", color = NA))

# Guardar
ggsave(
  filename = "plot_heatmap.png",
  plot = plot_heatmap_final,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "transparent"
)

# Grafico 13: Procrastinacion y calidad

# Crear gráfico de dispersión: procrastinación vs. índice de calidad del sueño
plot_procra_calidad <- ggplot(
  subset(data_label, !is.na(cal_invertido) & !is.na(procra_suenio) & !is.na(grupo_sueno)),
  aes(x = procra_suenio, y = cal_invertido, color = grupo_sueno)
) +
  geom_jitter(width = 0.2, height = 0.2, size = 2.7, alpha = 0.5) +
  scale_color_manual(values = colores_grupos) +
  labs(
    title = "Relación entre procrastinación y calidad del sueño",
    x = "Índice de procrastinación del sueño",
    y = "Índice de calidad del sueño",
    color = "Grupo de sueño"
  ) +
  theme_minimal(base_size = 19, base_family = "ibm") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  scale_x_continuous(
    breaks = seq(10, 40, by = 10)  # O ajustá el rango según tus datos
  ) +
  scale_y_continuous(
    breaks = seq(5, 20, by = 5)  # O poné seq(5, 20, by = 5) si querés algo similar
  ) +
  theme(
    panel.grid.major = element_line(color = "gray85", linewidth = 0.6),
    panel.grid.minor = element_blank()
  ) +
  theme(
    panel.grid.major = element_line(color = "gray85", linewidth = 0.6),
    panel.grid.minor = element_blank()
  )

# Mostrar gráfico
plot_procra_calidad

# Guardar gráfico
ggsave(
  filename = "plot_procra_calidad.png",
  plot = plot_procra_calidad,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "transparent"
)

data_label %>%
  filter(grupo_sueno %in% c("Mal Dormido", "Bien Dormido")) %>%
  group_by(grupo_sueno) %>%
  summarise(promedio_procra = mean(procra_suenio, na.rm = TRUE))

# Grafico 14: Distribucion de jetlag


# Quitar outliers (criterio: 1.5 × IQR)
jetlag_filtrado <- data_label |>
  filter(grupo_sueno %in% c("Bien Dormido", "Mal Dormido")) |>
  filter(!is.na(jetlag_social)) |>
  group_by(grupo_sueno) |>
  mutate(
    q1 = quantile(jetlag_social, 0.25),
    q3 = quantile(jetlag_social, 0.75),
    iqr = q3 - q1,
    limite_inf = q1 - 1.5 * iqr,
    limite_sup = q3 + 1.5 * iqr
  ) |>
  filter(jetlag_social >= limite_inf, jetlag_social <= limite_sup)

# quienes tienen mas de dos horas de social jetlag
jetlag_filtrado %>%
  mutate(jetlag_alto = if_else(jetlag_social > 2, "Sí", "No")) %>%
  filter(!is.na(jetlag_alto), !is.na(grupo_sueno)) %>%
  count(grupo_sueno, jetlag_alto) %>%
  group_by(grupo_sueno) %>%
  mutate(pct = n / sum(n) * 100)

# Calcular medias sin outliers
medias_jetlag <- jetlag_filtrado |>
  group_by(grupo_sueno) |>
  summarise(media = mean(jetlag_social), .groups = "drop")

library(scales)
# Gráfico con medias
plot_jetlag_densidad_media <- ggplot(
  jetlag_filtrado,
  aes(x = jetlag_social, fill = grupo_sueno)
) +
  geom_density(alpha = 0.6, adjust = 1.2) +
  geom_vline(
    data = medias_jetlag,
    aes(xintercept = media, color = grupo_sueno),
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_fill_manual(values = c("Bien Dormido" = "#024059", "Mal Dormido" = "#ED8B16")) +
  scale_color_manual(values = c("Bien Dormido" = "#024059", "Mal Dormido" = 	"#A55200")) +
  labs(
    title = "Distribución de jet lag social por grupo",
    x = "Jet lag social (horas)",
    y = "Densidad",
    fill = "Grupo de sueño",
    color = "Media"
  ) +
  theme_minimal(base_size = 7, base_family = 'ibm') +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
    axis.text.y = element_text(size=26),
    axis.text.x = element_text(size=26),
    axis.title = element_text(size=28),
    axis.title.y = element_text(size=28),
    legend.position = "bottom"
  )

plot_jetlag_densidad_media

# Guardar
ggsave(
  filename = "plot_jetlag_densidad_con_media.png",
  plot = plot_jetlag_densidad_media,
  width = 14,
  height = 7,
  dpi = 300,
  bg = "transparent"
)

# Grafico 15: Procrastinacion y estudio

# Filtrar casos completos para ambas variables
scatter_procra_examen <- data_label |>
  filter(
    !is.na(procra_suenio),
    !is.na(habsuenio_examen),
    grupo_sueno %in% c("Bien Dormido", "Mal Dormido")
  ) |>
  ggplot(aes(x = habsuenio_examen, y = procra_suenio, color = grupo_sueno)) +
  geom_jitter(width = 0.2, height = 0.2, size = 2.5, alpha = 0.6) +
  scale_color_manual(values = colores_grupos) +
  scale_x_continuous(
    breaks = 1:5,
    color= 'black',
    labels = c(
      "Extremadamente\n en desacuerdo",
      "En\n desacuerdo",
      "Neutral",
      "De\n acuerdo",
      "Extremadamente\n de acuerdo"
    )
  ) +
  labs(
    title = NULL,
    x = "Nivel de acuerdo",
    y = "Índice de procrastinación del sueño",
    color = NULL,
  ) +
  theme_minimal(base_size = 28, base_family = 'ibm') +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust= 1.2, size=15),
    axis.text.y = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.title.x = element_text(size=16) 
  )

scatter_procra_examen

# Guardar
ggsave(
  filename = "plot_procra_vs_examen.png",
  plot = scatter_procra_examen
)

# Grafico 16: Estres y Ansiedad

stacked_df <- data_label %>% 
  filter(grupo_sueno %in% c("Bien Dormido", "Mal Dormido")) %>% 
  mutate(grupo_sueno = factor(grupo_sueno,
                              levels = c("Mal Dormido", "Bien Dormido"))) %>% 
  pivot_longer(
    cols = c(habsuenio_ansiedad, habsuenio_estres),
    names_to = "pregunta",
    values_to = "resp"
  ) %>% 
  filter(resp %in% c(1, 2)) %>% 
  mutate(
    resp = factor(resp, levels = c(1, 2), labels = c("Sí", "No")),
    pregunta = recode(pregunta,
                      "habsuenio_ansiedad" = "Ansiedad",
                      "habsuenio_estres" = "Estrés")
  ) %>% 
  count(pregunta, grupo_sueno, resp) %>% 
  group_by(pregunta, grupo_sueno) %>% 
  mutate(pct = n / sum(n) * 100) %>% 
  ungroup()

stacked_df <- stacked_df %>%
  mutate(
    resp = factor(resp, levels = c("No", "Sí")),
    fill_color = interaction(resp, grupo_sueno),
    respuesta_label = resp  # esto se usará para la leyenda
  )

colores_personalizados <- c(
  "Sí.Mal Dormido" = "#ED8B16",   # naranja fuerte
  "No.Mal Dormido" = "#F3C188",   # naranja claro
  "Sí.Bien Dormido" =  "#024059",  # azul fuerte
  "No.Bien Dormido" = "#7DA3B0"   # azul claro
)

graf_estres <- stacked_df %>%
  filter(pregunta == "Estrés") %>%
  ggplot(aes(x = grupo_sueno, y = pct,
             fill = fill_color)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 10, family = "archivo", color = 'white', fontface = 'bold') +
  scale_fill_manual(values = colores_personalizados, guide = "none") +
  labs(title = NULL) +
  theme_minimal(base_size = 14) +
  scale_x_discrete(expand = expansion(mult = c(0.9, 0.9))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(size=22, color= 'black'),
    axis.text.y = element_blank(),       # 🔸 Quitar eje Y
    axis.ticks.y = element_blank(),      # 🔸 Quitar ticks del eje Y
    plot.title = element_text(hjust = 0.5, face = "bold", size= 24),
    axis.title = element_blank()
  )

graf_ansiedad <- stacked_df %>%
  filter(pregunta == "Ansiedad") %>%
  ggplot(aes(x = grupo_sueno, y = pct,
             fill = fill_color)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 10, family = "archivo", color = 'white', fontface = 'bold') +
  scale_fill_manual(
    values = colores_personalizados,
    breaks = c("Sí.Mal Dormido", "No.Mal Dormido", "Sí.Bien Dormido", "No.Bien Dormido"),
    labels = c("Sí", "No",
               "Sí", "No"),
    name = NULL
  ) +
  labs(title = NULL) +
  theme_minimal(base_size = 14) +
  scale_x_discrete(expand = expansion(mult = c(0.9, 0.9))) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size=24),
    axis.text.x  = element_text(size=22, color= 'black'),
    axis.text.y  = element_text(size=22, color= 'black'),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size=40)
  )

ansiedad_estres <- graf_ansiedad + graf_estres 
ansiedad_estres

ggsave(
  filename = "plot_ans_estres.png", # nombre del archivo
  plot = ansiedad_estres, # tu objeto ggplot
  width = 10,
  height = 4.93, 
  units = 'cm',
  dpi = 400,
  bg = "transparent"
)

