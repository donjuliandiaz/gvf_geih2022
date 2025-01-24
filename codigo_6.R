rm(list = ls())

# Cargar librerías necesarias
library(readr)
library(dplyr)
library(sae)
library(ggplot2)
library(survey)
library(srvyr)
library(rlang)
library(readxl)

# Lectura de la base de datos inicial
base_geih_12 <- read_delim("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/base_geih_12.csv", 
                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)

# Cargar bases adicionales
base_estimador_real_mes <- read_delim(
  "C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/estimacion directa data/base_estimador_real.csv", 
  delim = "\t", escape_double = FALSE, trim_ws = TRUE
)

base_estimador_real_trim <- read_excel(
  "C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/estimacion directa data/trimestral/base_estimador_real_trimestral_completo.xlsx"
) %>%
  dplyr::select(-trim, -orden, -indice)

# Convertir columnas al mismo tipo (ejemplo: convertir todas las columnas problemáticas a carácter)
base_estimador_real_mes <- base_estimador_real_mes %>%
  mutate(across(where(is.numeric), as.character))

base_estimador_real_trim <- base_estimador_real_trim %>%
  mutate(across(where(is.numeric), as.character))

# Combinar las bases de datos
base_final <- bind_rows(base_estimador_real_mes, base_estimador_real_trim)

# Guardar la base final
write_delim(
  base_final,
  file = "C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/base_final.csv",
  delim = "\t"
)
