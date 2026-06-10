# ============================================================
# 03_modelacion.R
# Bitacora 3 - Analisis de modelacion
# Proyecto: Uso de redes sociales y rendimiento academico
# ============================================================
#
# Este archivo:
# 1. Carga automaticamente los datos desde datos/procesados/survey_clean.csv
# 2. Limpia y prepara las variables principales
# 3. Hace analisis descriptivo completo
# 4. Ajusta modelos para responder la pregunta de investigacion
# 5. Hace diagnosticos del modelo seleccionado
# 6. Guarda tablas y figuras para la Bitacora 3
#
# ============================================================


# ------------------------------------------------------------
# 0. Paquetes
# ------------------------------------------------------------

paquetes <- c("tidyverse", "janitor", "broom")

for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    stop(paste("Hace falta instalar el paquete:", p))
  }
}


# ------------------------------------------------------------
# 1. Funcion para encontrar la raiz del proyecto
# ------------------------------------------------------------

buscar_raiz_proyecto <- function() {
  carpeta_actual <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  for (i in 1:6) {
    posible_archivo <- file.path(carpeta_actual, "datos", "procesados", "survey_clean.csv")

    if (file.exists(posible_archivo)) {
      return(carpeta_actual)
    }

    carpeta_padre <- dirname(carpeta_actual)

    if (carpeta_padre == carpeta_actual) {
      break
    }

    carpeta_actual <- carpeta_padre
  }

  stop("No se encontro datos/procesados/survey_clean.csv. Revise la ubicacion del archivo.")
}

raiz_proyecto <- buscar_raiz_proyecto()

ruta_datos <- file.path(raiz_proyecto, "datos", "procesados", "survey_clean.csv")
ruta_figuras <- file.path(raiz_proyecto, "bitacoras", "bitacora_3", "figuras")
ruta_salidas <- file.path(raiz_proyecto, "bitacoras", "bitacora_3", "salidas")

dir.create(ruta_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_salidas, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------
# 2. Carga automatica de datos
# ------------------------------------------------------------

datos_raw <- readr::read_csv(ruta_datos, show_col_types = FALSE)

# Guardamos una revision rapida de dimensiones
revision_inicial <- tibble(
  filas_originales = nrow(datos_raw),
  columnas_originales = ncol(datos_raw)
)

readr::write_csv(
  revision_inicial,
  file.path(ruta_salidas, "00_revision_inicial.csv")
)


# ------------------------------------------------------------
# 3. Limpieza y preparacion de variables
# ------------------------------------------------------------

extraer_gpa <- function(x) {
  x <- as.character(x)
  x_limpio <- str_trim(str_to_upper(x))

  gpa_num <- readr::parse_number(x_limpio)

  gpa_num <- case_when(
    x_limpio == "A" ~ 5,
    x_limpio == "A-" ~ 4.5,
    TRUE ~ gpa_num
  )

  return(gpa_num)
}

datos <- datos_raw %>%
  janitor::clean_names() %>%
  transmute(
    # Variables academicas principales
    promedio = extraer_gpa(last_academic_result_gpa_cgpa),
    horas_estudio = as.numeric(study_time_in_hours),
    asistencia = as.numeric(attendance_rate_in_percentile),

    # Variables de redes sociales
    horas_redes = as.numeric(time_spent_in_social_media_hours),
    plataforma = as.factor(social_media_platform),
    momento_uso = as.factor(most_time_spent_in_a_day),
    distraccion_acad = as.numeric(social_media_distraction_during_academic_activities),

    # Variables personales y de contexto
    edad = as.numeric(age),
    genero = as.factor(gender),
    zona_residencia = as.factor(residence_area),
    nivel_educativo = as.factor(education_level),
    actividad_fisica = as.factor(physical_activity_30_min),

    # Variables adicionales de bienestar
    alteracion_sueno = as.numeric(sleep_disturbance_on_sleep_quality),
    ansiedad = as.numeric(anxiety_scale),
    depresion = as.numeric(depression_scale),
    autoestima = as.numeric(self_esteem_scale)
  ) %>%
  # Filtros simples para evitar valores imposibles o errores evidentes de digitacion
  filter(
    !is.na(promedio),
    promedio >= 0,
    promedio <= 5,
    !is.na(horas_redes),
    horas_redes >= 0,
    horas_redes <= 20,
    !is.na(horas_estudio),
    horas_estudio >= 0,
    horas_estudio <= 16,
    !is.na(asistencia),
    asistencia >= 0,
    asistencia <= 100,
    !is.na(edad),
    edad >= 15,
    edad <= 60
  ) %>%
  mutate(
    # Variable categórica para comparar grupos de uso de redes.
    cat_horas_redes = case_when(
      horas_redes <= 2 ~ "0-2 h",
      horas_redes <= 4 ~ "2-4 h",
      horas_redes <= 6 ~ "4-6 h",
      horas_redes > 6 ~ ">6 h"
    ),
    cat_horas_redes = factor(
      cat_horas_redes,
      levels = c("0-2 h", "2-4 h", "4-6 h", ">6 h")
    ),

    # Variable binaria para prueba t.
    # Se usa 4 horas como punto de corte porque esta cerca del promedio de horas en redes.
    grupo_uso = if_else(horas_redes <= 4, "Bajo o moderado", "Alto"),
    grupo_uso = factor(grupo_uso, levels = c("Bajo o moderado", "Alto")),

    # Version en factor para el heatmap y para algunas tablas
    distraccion_grupo = factor(
      distraccion_acad,
      levels = 1:5,
      labels = c("Distr. 1", "Distr. 2", "Distr. 3", "Distr. 4", "Distr. 5")
    )
  )

revision_limpieza <- tibble(
  filas_despues_limpieza = nrow(datos),
  columnas_despues_limpieza = ncol(datos),
  filas_eliminadas = nrow(datos_raw) - nrow(datos)
)

readr::write_csv(
  revision_limpieza,
  file.path(ruta_salidas, "01_revision_limpieza.csv")
)

# Guardamos la base usada en la modelacion 
readr::write_csv(
  datos,
  file.path(ruta_salidas, "datos_modelacion.csv")
)


# ------------------------------------------------------------
# 4. Analisis descriptivo 
# ------------------------------------------------------------

# 4.1 Resumen de variables numericas
resumen_numericas <- datos %>%
  select(
    promedio, horas_redes, horas_estudio, asistencia, edad,
    distraccion_acad, alteracion_sueno, ansiedad, depresion, autoestima
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  summarise(
    n = sum(!is.na(valor)),
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    desviacion = sd(valor, na.rm = TRUE),
    minimo = min(valor, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  resumen_numericas,
  file.path(ruta_salidas, "02_resumen_numericas.csv")
)


# 4.2 Frecuencias de variables categoricas
frecuencia_categoricas <- datos %>%
  select(
    genero, zona_residencia, plataforma, momento_uso,
    cat_horas_redes, grupo_uso, distraccion_grupo
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "categoria"
  ) %>%
  count(variable, categoria, name = "n") %>%
  group_by(variable) %>%
  mutate(porcentaje = round(100 * n / sum(n), 2)) %>%
  ungroup()

readr::write_csv(
  frecuencia_categoricas,
  file.path(ruta_salidas, "03_frecuencia_categoricas.csv")
)


# 4.3 Promedio academico por grupo de uso de redes
resumen_por_uso <- datos %>%
  group_by(cat_horas_redes) %>%
  summarise(
    n = n(),
    promedio_medio = mean(promedio, na.rm = TRUE),
    mediana_promedio = median(promedio, na.rm = TRUE),
    desviacion_promedio = sd(promedio, na.rm = TRUE),
    horas_redes_media = mean(horas_redes, na.rm = TRUE),
    distraccion_media = mean(distraccion_acad, na.rm = TRUE),
    horas_estudio_media = mean(horas_estudio, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  resumen_por_uso,
  file.path(ruta_salidas, "04_resumen_por_uso_redes.csv")
)


# 4.4 Promedio academico por nivel de distraccion
resumen_por_distraccion <- datos %>%
  group_by(distraccion_grupo) %>%
  summarise(
    n = n(),
    promedio_medio = mean(promedio, na.rm = TRUE),
    horas_redes_media = mean(horas_redes, na.rm = TRUE),
    horas_estudio_media = mean(horas_estudio, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  resumen_por_distraccion,
  file.path(ruta_salidas, "05_resumen_por_distraccion.csv")
)


# ------------------------------------------------------------
# 5. Figuras descriptivas
# ------------------------------------------------------------

# Paleta
col_verde_oscuro <- "#374E55"
col_naranja <- "#DF8F44"
col_azul <- "#00A1D5"
col_rojo <- "#B24745"
col_verde <- "#79AF97"

tema_grupo <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray35"),
      legend.position = "right"
    )
}


# Figura 1: horas en redes vs promedio
fig1 <- ggplot(datos, aes(x = horas_redes, y = promedio, color = plataforma)) +
  geom_point(alpha = 0.65, size = 2) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE,
              color = col_rojo, fill = col_rojo, alpha = 0.15) +
  labs(
    title = "Horas en redes sociales vs Promedio academico",
    subtitle = "Tendencia LOESS con banda de confianza",
    x = "Horas diarias en redes sociales",
    y = "Promedio academico (escala 0-5)",
    color = "Red social"
  ) +
  tema_grupo()

ggsave(
  file.path(ruta_figuras, "figura_01_horas_redes_vs_promedio.png"),
  fig1, width = 9, height = 6, dpi = 300
)


# Figura 2: boxplot por categoria de uso de redes
fig2 <- ggplot(datos, aes(x = cat_horas_redes, y = promedio, fill = cat_horas_redes)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.5) +
  geom_jitter(width = 0.12, alpha = 0.25, size = 1.4, color = "gray35") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", color = "black") +
  scale_fill_manual(values = c(col_verde_oscuro, col_naranja, col_azul, col_rojo)) +
  labs(
    title = "Distribucion del promedio por nivel de uso de redes",
    subtitle = "El rombo blanco representa la media grupal",
    x = "Horas diarias en redes sociales",
    y = "Promedio academico (escala 0-5)"
  ) +
  guides(fill = "none") +
  tema_grupo()

ggsave(
  file.path(ruta_figuras, "figura_02_boxplot_uso_redes.png"),
  fig2, width = 9, height = 6, dpi = 300
)


# Figura 3: horas de estudio vs promedio
fig3 <- ggplot(datos, aes(x = horas_estudio, y = promedio, color = horas_redes)) +
  geom_point(alpha = 0.65, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = col_verde_oscuro, linetype = "dashed") +
  scale_color_gradient(low = col_azul, high = col_rojo) +
  labs(
    title = "Horas de estudio vs Promedio academico",
    subtitle = "Color = horas diarias en redes; linea = regresion lineal",
    x = "Horas diarias de estudio",
    y = "Promedio academico (escala 0-5)",
    color = "Horas en redes"
  ) +
  tema_grupo()

ggsave(
  file.path(ruta_figuras, "figura_03_horas_estudio_vs_promedio.png"),
  fig3, width = 9, height = 6, dpi = 300
)


# Figura 4: promedio por plataforma
resumen_plataforma <- datos %>%
  group_by(plataforma) %>%
  summarise(
    n = n(),
    promedio_medio = mean(promedio, na.rm = TRUE),
    error_estandar = sd(promedio, na.rm = TRUE) / sqrt(n),
    horas_redes_media = mean(horas_redes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(promedio_medio)

readr::write_csv(
  resumen_plataforma,
  file.path(ruta_salidas, "06_resumen_por_plataforma.csv")
)

fig4 <- ggplot(resumen_plataforma, aes(x = promedio_medio, y = reorder(plataforma, promedio_medio))) +
  geom_col(fill = col_verde_oscuro, alpha = 0.8) +
  geom_errorbar(
    aes(xmin = promedio_medio - error_estandar,
        xmax = promedio_medio + error_estandar),
    width = 0.2
  ) +
  geom_text(
    aes(label = paste0(round(horas_redes_media, 1), " h/dia  (n=", n, ")")),
    hjust = -0.05,
    size = 3.5
  ) +
  coord_cartesian(xlim = c(0, 5.4)) +
  labs(
    title = "Promedio academico medio por red social principal",
    subtitle = "Barras de error: +/- 1 error estandar",
    x = "Promedio academico medio (escala 0-5)",
    y = "Plataforma"
  ) +
  tema_grupo()

ggsave(
  file.path(ruta_figuras, "figura_04_promedio_por_plataforma.png"),
  fig4, width = 9, height = 6, dpi = 300
)


# Figura 5: heatmap de uso de redes y distraccion
datos_heatmap <- datos %>%
  group_by(cat_horas_redes, distraccion_grupo) %>%
  summarise(
    n = n(),
    promedio_medio = mean(promedio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    etiqueta = paste0(round(promedio_medio, 2), "\n(n=", n, ")")
  )

readr::write_csv(
  datos_heatmap,
  file.path(ruta_salidas, "07_heatmap_redes_distraccion.csv")
)

fig5 <- ggplot(datos_heatmap, aes(x = cat_horas_redes, y = distraccion_grupo, fill = promedio_medio)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = etiqueta), color = "white", fontface = "bold", size = 3.5) +
  scale_fill_gradient2(
    low = col_rojo,
    mid = "gray90",
    high = col_azul,
    midpoint = mean(datos$promedio, na.rm = TRUE)
  ) +
  labs(
    title = "Promedio academico por nivel de uso de redes y distraccion",
    subtitle = "Escala de distraccion academica: 1 = minima; 5 = maxima",
    x = "Horas diarias en redes sociales",
    y = "Escala de distraccion academica",
    fill = "Promedio\nmedio"
  ) +
  tema_grupo()

ggsave(
  file.path(ruta_figuras, "figura_05_heatmap_redes_distraccion.png"),
  fig5, width = 9, height = 6, dpi = 300
)


# ------------------------------------------------------------
# 6. Pruebas de hipotesis y correlaciones
# ------------------------------------------------------------

# 6.1 Correlaciones
# Pearson evalua asociacion lineal.
# Spearman evalua asociacion monotona y es mas robusta si la relacion no es lineal.

cor_pearson <- cor.test(datos$horas_redes, datos$promedio, method = "pearson")
cor_spearman <- cor.test(datos$horas_redes, datos$promedio, method = "spearman")

correlaciones <- tibble(
  metodo = c("Pearson", "Spearman"),
  estimacion = c(unname(cor_pearson$estimate), unname(cor_spearman$estimate)),
  p_valor = c(cor_pearson$p.value, cor_spearman$p.value)
)

readr::write_csv(
  correlaciones,
  file.path(ruta_salidas, "08_correlaciones_horas_redes_promedio.csv")
)


# 6.2 Prueba t: bajo/moderado uso vs alto uso
# Esta prueba ayuda a ver si el promedio academico medio cambia al dividir
# la muestra en dos grupos de intensidad de uso de redes.

prueba_t <- t.test(promedio ~ grupo_uso, data = datos)

resultado_t <- broom::tidy(prueba_t)

readr::write_csv(
  resultado_t,
  file.path(ruta_salidas, "09_prueba_t_grupo_uso.csv")
)


# 6.3 ANOVA: compara las cuatro categorias de uso de redes
# Esta prueba ayuda a evaluar si al menos una categoria tiene promedio diferente.

modelo_anova <- aov(promedio ~ cat_horas_redes, data = datos)
resultado_anova <- broom::tidy(modelo_anova)

readr::write_csv(
  resultado_anova,
  file.path(ruta_salidas, "10_anova_cat_horas_redes.csv")
)

# Si el ANOVA no fuera adecuado por supuestos, esta prueba no parametrica sirve
# como comparacion adicional.
kruskal <- kruskal.test(promedio ~ cat_horas_redes, data = datos)
resultado_kruskal <- broom::tidy(kruskal)

readr::write_csv(
  resultado_kruskal,
  file.path(ruta_salidas, "11_kruskal_cat_horas_redes.csv")
)


# ------------------------------------------------------------
# 7. Ajuste de modelos de regresion
# ------------------------------------------------------------
# Modelo 1: relacion simple entre horas de redes y promedio.
# Responde la pregunta de manera inicial, pero no controla otras variables.

modelo_1 <- lm(promedio ~ horas_redes, data = datos)

# Modelo 2: modelo ajustado con variables academicas principales.
# Este modelo responde mejor la pregunta porque considera que el rendimiento
# no depende solo de las redes, sino tambien de estudio, asistencia y distraccion.

modelo_2 <- lm(
  promedio ~ horas_redes + horas_estudio + asistencia + distraccion_acad,
  data = datos
)

# Modelo 3: modelo con interaccion.
# Este es el modelo mas importante para el enfoque de la Bitacora 3.
# Permite evaluar si la relacion entre horas en redes y promedio cambia segun
# el nivel de distraccion academica.

modelo_3 <- lm(
  promedio ~ horas_redes * distraccion_acad + horas_estudio + asistencia,
  data = datos
)

# Comparacion de modelos
resumen_modelos <- bind_rows(
  glance(modelo_1) %>% mutate(modelo = "Modelo 1: horas_redes"),
  glance(modelo_2) %>% mutate(modelo = "Modelo 2: ajustado"),
  glance(modelo_3) %>% mutate(modelo = "Modelo 3: interaccion")
) %>%
  select(modelo, r.squared, adj.r.squared, sigma, AIC, BIC, statistic, p.value, df, df.residual)

readr::write_csv(
  resumen_modelos,
  file.path(ruta_salidas, "12_comparacion_modelos.csv")
)

# Coeficientes de cada modelo
readr::write_csv(
  tidy(modelo_1, conf.int = TRUE),
  file.path(ruta_salidas, "13_coeficientes_modelo_1.csv")
)

readr::write_csv(
  tidy(modelo_2, conf.int = TRUE),
  file.path(ruta_salidas, "14_coeficientes_modelo_2.csv")
)

readr::write_csv(
  tidy(modelo_3, conf.int = TRUE),
  file.path(ruta_salidas, "15_coeficientes_modelo_3.csv")
)

# Guardamos un resumen en texto para pegar o revisar rapidamente
sink(file.path(ruta_salidas, "16_resumen_modelos.txt"))
cat("MODELO 1: promedio ~ horas_redes\n")
print(summary(modelo_1))
cat("\n\nMODELO 2: promedio ~ horas_redes + horas_estudio + asistencia + distraccion_acad\n")
print(summary(modelo_2))
cat("\n\nMODELO 3: promedio ~ horas_redes * distraccion_acad + horas_estudio + asistencia\n")
print(summary(modelo_3))
cat("\n\nCOMPARACION ANOVA ENTRE MODELOS\n")
print(anova(modelo_1, modelo_2, modelo_3))
sink()


# ------------------------------------------------------------
# 8. Figura de predicciones del modelo seleccionado
# ------------------------------------------------------------
# Usamos el Modelo 3 para mostrar la relacion estimada entre horas en redes,
# promedio y niveles de distraccion. Esta figura es util para la Bitacora 3
# porque resume el enfoque innovador del proyecto.

nuevo_dato <- expand_grid(
  horas_redes = seq(
    min(datos$horas_redes, na.rm = TRUE),
    max(datos$horas_redes, na.rm = TRUE),
    length.out = 100
  ),
  distraccion_acad = c(1, 3, 5),
  horas_estudio = mean(datos$horas_estudio, na.rm = TRUE),
  asistencia = mean(datos$asistencia, na.rm = TRUE)
)

predicciones <- predict(modelo_3, newdata = nuevo_dato, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(nuevo_dato) %>%
  mutate(
    distraccion_acad = factor(
      distraccion_acad,
      levels = c(1, 3, 5),
      labels = c("Distraccion baja", "Distraccion media", "Distraccion alta")
    )
  )

readr::write_csv(
  predicciones,
  file.path(ruta_salidas, "17_predicciones_modelo_3.csv")
)

fig6 <- ggplot(predicciones, aes(x = horas_redes, y = fit, color = distraccion_acad, fill = distraccion_acad)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Promedio academico predicho segun redes y distraccion",
    subtitle = "Predicciones del modelo con interaccion; estudio y asistencia se mantienen en su media",
    x = "Horas diarias en redes sociales",
    y = "Promedio academico predicho",
    color = "Nivel de distraccion",
    fill = "Nivel de distraccion"
  ) +
  tema_grupo()

ggsave(
  file.path(ruta_figuras, "figura_06_predicciones_modelo_interaccion.png"),
  fig6, width = 9, height = 6, dpi = 300
)


# ------------------------------------------------------------
# 9. Diagnosticos del modelo seleccionado
# ------------------------------------------------------------
# Seleccionamos el Modelo 3 porque responde mejor la pregunta de investigacion:
# evalua las horas en redes, controla por estudio y asistencia, e incorpora
# la interaccion con distraccion academica.
#
# Los diagnosticos no prueban que el modelo sea perfecto, pero ayudan a saber
# si sus supuestos son razonables o si debemos interpretar con cautela.

modelo_seleccionado <- modelo_3

datos_diagnostico <- augment(modelo_seleccionado) %>%
  mutate(
    indice = row_number()
  )

readr::write_csv(
  datos_diagnostico,
  file.path(ruta_salidas, "18_datos_diagnostico_modelo_3.csv")
)


# Diagnostico 1: residuales vs ajustados
# Sirve para revisar linealidad y varianza aproximadamente constante.
fig_diag1 <- ggplot(datos_diagnostico, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6, color = col_verde_oscuro) +
  geom_hline(yintercept = 0, linetype = "dashed", color = col_rojo) +
  geom_smooth(method = "loess", se = FALSE, color = col_azul) +
  labs(
    title = "Diagnostico 1: Residuales vs valores ajustados",
    subtitle = "Se espera una nube sin patron claro alrededor de cero",
    x = "Valores ajustados",
    y = "Residuales"
  ) +
  tema_grupo()

ggsave(
  file.path(ruta_figuras, "diagnostico_01_residuales_vs_ajustados.png"),
  fig_diag1, width = 8, height = 5.5, dpi = 300
)


# Diagnostico 2: QQ plot
# Sirve para revisar si los residuales se parecen aproximadamente a una normal.
fig_diag2 <- ggplot(datos_diagnostico, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.6, color = col_verde_oscuro) +
  stat_qq_line(color = col_rojo) +
  labs(
    title = "Diagnostico 2: QQ plot de residuales estandarizados",
    subtitle = "Si los puntos siguen la linea, la normalidad es razonable",
    x = "Cuantiles teoricos",
    y = "Cuantiles muestrales"
  ) +
  tema_grupo()

ggsave(
  file.path(ruta_figuras, "diagnostico_02_qqplot_residuales.png"),
  fig_diag2, width = 8, height = 5.5, dpi = 300
)


# Diagnostico 3: histograma de residuales
fig_diag3 <- ggplot(datos_diagnostico, aes(x = .resid)) +
  geom_histogram(bins = 25, fill = col_azul, color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = col_rojo) +
  labs(
    title = "Diagnostico 3: Distribucion de residuales",
    subtitle = "Permite observar asimetria o valores extremos",
    x = "Residuales",
    y = "Frecuencia"
  ) +
  tema_grupo()

ggsave(
  file.path(ruta_figuras, "diagnostico_03_histograma_residuales.png"),
  fig_diag3, width = 8, height = 5.5, dpi = 300
)


# Diagnostico 4: Distancia de Cook
# Sirve para detectar observaciones que tienen mucha influencia en el modelo.
fig_diag4 <- ggplot(datos_diagnostico, aes(x = indice, y = .cooksd)) +
  geom_col(fill = col_naranja, alpha = 0.8) +
  geom_hline(yintercept = 4 / nrow(datos_diagnostico), linetype = "dashed", color = col_rojo) +
  labs(
    title = "Diagnostico 4: Distancia de Cook",
    subtitle = "Barras altas pueden indicar observaciones influyentes",
    x = "Indice de observacion",
    y = "Distancia de Cook"
  ) +
  tema_grupo()

ggsave(
  file.path(ruta_figuras, "diagnostico_04_distancia_cook.png"),
  fig_diag4, width = 8, height = 5.5, dpi = 300
)


# Prueba de normalidad de Shapiro-Wilk sobre residuales.
# Nota: con muestras grandes, esta prueba puede rechazar normalidad por diferencias pequenas.
shapiro <- shapiro.test(residuals(modelo_seleccionado))

resultado_shapiro <- tibble(
  prueba = "Shapiro-Wilk",
  estadistico = unname(shapiro$statistic),
  p_valor = shapiro$p.value
)

readr::write_csv(
  resultado_shapiro,
  file.path(ruta_salidas, "19_shapiro_residuales_modelo_3.csv")
)


# Diagnostico de multicolinealidad sencillo.
# Calculamos VIF de forma manual para no depender del paquete car.
# Valores cercanos a 1 son buenos; valores mayores a 5 o 10 pueden indicar problema.

calcular_vif <- function(modelo) {
  matriz_x <- model.matrix(modelo)
  matriz_x <- matriz_x[, colnames(matriz_x) != "(Intercept)", drop = FALSE]

  if (ncol(matriz_x) < 2) {
    return(tibble(variable = colnames(matriz_x), vif = NA_real_))
  }

  resultados <- map_dfr(seq_len(ncol(matriz_x)), function(j) {
    y <- matriz_x[, j]
    x <- matriz_x[, -j, drop = FALSE]
    ajuste <- lm(y ~ x)
    r2 <- summary(ajuste)$r.squared
    tibble(
      variable = colnames(matriz_x)[j],
      vif = 1 / (1 - r2)
    )
  })

  return(resultados)
}

vif_modelo <- calcular_vif(modelo_seleccionado)

readr::write_csv(
  vif_modelo,
  file.path(ruta_salidas, "20_vif_modelo_3.csv")
)


# Medidas simples de error del modelo seleccionado
rmse <- sqrt(mean(residuals(modelo_seleccionado)^2))
mae <- mean(abs(residuals(modelo_seleccionado)))

metricas_modelo <- glance(modelo_seleccionado) %>%
  mutate(
    rmse = rmse,
    mae = mae,
    modelo = "Modelo 3: interaccion"
  ) %>%
  select(modelo, r.squared, adj.r.squared, sigma, rmse, mae, AIC, BIC, p.value, df.residual)

readr::write_csv(
  metricas_modelo,
  file.path(ruta_salidas, "21_metricas_modelo_seleccionado.csv")
)


