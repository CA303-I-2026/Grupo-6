# 01_limpieza.R
# Limpieza y preparación de los datos crudos
# Autor: Grupo 6
# Fecha:6-5-2026

library(tidyverse)
library(readr)

# 1. Carga de datos

df_raw <- read_csv(
  "survey_clean.csv",
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

cat("Filas originales:", nrow(df_raw), "\n")
cat("Columnas originales:", ncol(df_raw), "\n")


# 2. Función para extraer GPA numérico del texto libre

parse_gpa <- function(x) {
  x <- tolower(trimws(as.character(x)))
  
  if (is.na(x) || x == "") {
    return(NA_real_)
  }
  
  # Caso: fracción X/Y → usar numerador
  m <- regmatches(x, regexpr("[0-9]+\\.?[0-9]*/[0-9]+\\.?[0-9]*", x))
  
  if (length(m) > 0 && m != "") {
    partes <- as.numeric(strsplit(m, "/")[[1]])
    return(partes[1])
  }
  
  # Extraer números entre 0.5 y 5.5
  nums <- as.numeric(regmatches(x, gregexpr("[0-9]+\\.?[0-9]*", x))[[1]])
  nums <- nums[nums >= 0.5 & nums <= 5.5]
  
  if (length(nums) > 0) {
    return(nums[1])
  }
  
  return(NA_real_)
}


# 3.1. Limpiar horas de estudio

parse_horas <- function(x) {
  x <- tolower(trimws(as.character(x)))
  
  if (is.na(x) || x == "") {
    return(NA_real_)
  }
  
  # Caso <1
  if (str_detect(x, "<\\s*1")) {
    return(0.5)
  }
  
  # Caso rangos con guion: 4-5, 6-7 hour
  if (str_detect(x, "[0-9]+\\.?[0-9]*\\s*-\\s*[0-9]+\\.?[0-9]*")) {
    nums <- as.numeric(unlist(str_extract_all(x, "[0-9]+\\.?[0-9]*")))
    return(mean(nums[1:2], na.rm = TRUE))
  }
  
  # Caso tipo 3/4 o 7/8, interpretado como rango 3 a 4 o 7 a 8
  if (str_detect(x, "^[0-9]+\\s*/\\s*[0-9]+$")) {
    nums <- as.numeric(unlist(strsplit(x, "/")))
    return(mean(nums, na.rm = TRUE))
  }
  
  # Caso normal: primer número
  nums <- as.numeric(unlist(str_extract_all(x, "[0-9]+\\.?[0-9]*")))
  
  if (length(nums) > 0) {
    return(nums[1])
  }
  
  return(NA_real_)
}


# 3. Renombrar columnas a español para facilitar

df <- df_raw %>%
  rename(
    marca_tiempo     = Timestamp,
    edad             = Age,
    genero           = Gender,
    zona_residencia  = Residence.Area,
    nivel_educativo  = Education.Level,
    nivel_socioeco   = `Socioeconomic.status..Parent.s.education.level.`,
    horas_estudio    = `Study.time..In.Hours.`,
    asistencia       = `Attendance.rate..In.Percentile.`,
    plataforma       = Social.Media.Platform,
    horas_redes      = `Time.spent.in.social.media..hours.`,
    momento_uso      = Most.time.spent.in.a.day,
    actividad_fisica = `Physical.activity..30.min..`,
    sintomas_abstine = `Withdrawal.symptoms..Side.effects.of.not.using.social.media.`,
    alteracion_sueno = Sleep.Disturbance.on.Sleep.Quality,
    modificacion_ani = Mood.Modification.Scale,
    ansiedad         = Anxiety.Scale,
    depresion        = Depression.Scale,
    autoestima       = Self.esteem.Scale,
    promedio_raw     = `Last.Academic.Result..GPA.CGPA.`,
    distraccion_acad = Social.Media.Distraction.During.Academic.Activities
  )


# 4. Transformaciones

df <- df %>%
  mutate(
    # GPA numérico
    promedio = map_dbl(promedio_raw, parse_gpa),
    
    # Horas en redes
    horas_redes = as.numeric(horas_redes),
    horas_redes = if_else(horas_redes > 20, NA_real_, horas_redes),
    
    # Horas de estudio limpias
    horas_estudio = map_dbl(horas_estudio, parse_horas),
    horas_estudio = if_else(horas_estudio > 20, NA_real_, horas_estudio),
    
    # Variable categórica ordinal de uso de redes
    cat_horas_redes = cut(
      horas_redes,
      breaks = c(-Inf, 2, 4, 6, Inf),
      labels = c("0–2 h", "2–4 h", "4–6 h", ">6 h"),
      right  = TRUE,
      ordered_result = TRUE
    ),
    
    # Plataformas con menos de 10 observaciones agrupadas como Otro
    plataforma = fct_lump_min(plataforma, min = 10, other_level = "Otro"),
    
    # Escalas ordinales como enteros
    distraccion_acad = as.integer(distraccion_acad),
    ansiedad         = as.integer(ansiedad),
    depresion        = as.integer(depresion),
    autoestima       = as.integer(autoestima),
    alteracion_sueno = as.integer(alteracion_sueno),
    modificacion_ani = as.integer(modificacion_ani),
    
    # Asistencia como numérico
    asistencia = as.numeric(asistencia)
  )


# 5. Valores perdidos antes y después

cat("\nValores NA\n")

cols_clave <- c(
  "promedio",
  "horas_redes",
  "horas_estudio",
  "asistencia",
  "distraccion_acad",
  "ansiedad",
  "depresion",
  "autoestima"
)

for (col in cols_clave) {
  n_na <- sum(is.na(df[[col]]))
  cat(sprintf("  %-20s: %d NA  (%.1f%%)\n", col, n_na, 100 * n_na / nrow(df)))
}


df_verificacion <- read_csv("survey_limpio.csv", show_col_types = FALSE)

cat("Filas del archivo limpio:", nrow(df_verificacion), "\n")
cat("Columnas del archivo limpio:", ncol(df_verificacion), "\n")

View(df_verificacion)


# 6. Guardar CSV limpio

# Guardar el dataframe limpio
write_csv(df, "survey.limpio.csv")

cat(sprintf(
  "\nGuardado: survey.limpio.csv  (%d filas, %d columnas)\n",
  nrow(df), ncol(df)
))

cat("Columnas disponibles:\n")
cat(paste0("  ", names(df), collapse = "\n"), "\n")

# Crear dataframe de verificación leyendo el archivo limpio
df_verificacion <- read_csv(
  "survey.limpio.csv",
  show_col_types = FALSE
)

# Guardar dataframe de verificación en un nuevo CSV
write_csv(df_verificacion, "survey.verificacion.csv")

cat(sprintf(
  "\nGuardado: survey.verificacion.csv  (%d filas, %d columnas)\n",
  nrow(df_verificacion), ncol(df_verificacion)
))