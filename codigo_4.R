rm(list = ls())

# Cargar librerías necesarias
library(dplyr)
library(readr)

# Establecer directorio de trabajo
setwd("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/estimacion directa data")

# Leer y combinar las 12 tablas en una sola
files <- sprintf("%02d_geih.csv", 1:12)
base_geih_12 <- lapply(files, function(x) read_delim(x, delim = "\t", escape_double = FALSE, trim_ws = TRUE))

# Unir todas las tablas en una
base_geih_12 <- bind_rows(base_geih_12) %>%
  mutate(across(everything(), as.character), # Convertir todas las columnas a character
         mes = rep(1:12, each = nrow(.) / length(unique(files))), # Agregar columna de mes
         CLASE = "Nacional", # Agregar columna CLASE
         nivel = rep(c("nacional", "13_ciudad"), times = nrow(.) / 2), # Nivel alternado
         periodicidad = "Mensual") # Agregar periodicidad

# Guardar el resultado en un archivo CSV
write_delim(base_geih_12,
            file = "C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/estimacion directa data/base_estimador_real.csv",
            delim = "\t")

