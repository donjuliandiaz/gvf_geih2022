# Código reducido para unir 12 bases GEIH
rm(list = ls())
library(dplyr)

setwd("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/union")

# Leer y combinar archivos en una sola línea
base_geih_12 <- lapply(sprintf("%02d_geih.csv", 1:12), read_delim, delim = "\t", escape_double = FALSE, col_types = cols(MES = col_character()), trim_ws = TRUE) %>%
  bind_rows() %>% mutate(MES = ifelse(MES == '1','01',MES)) %>%
  mutate(across(everything(), as.character))


# Guardar el archivo combinado
write_delim(base_geih_12, 
            file = "C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/base_geih_12.csv", 
            delim = "\t")

