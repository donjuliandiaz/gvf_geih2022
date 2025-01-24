rm(list = ls())

# 1. Cargar librerías y leer la base

library(readr)
library(dplyr)
library(survey)
library(srvyr)
library(rlang)
library(openxlsx)

# Leer la base principal
data <- read_delim(
  "C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/base_geih_12.csv",
  delim = "\t", escape_double = FALSE, trim_ws = TRUE
) %>%
  mutate(MES = if_else(MES == '1', '01', MES))  # Ajuste puntual



# 2. Definir los trimestres como una lista

TRIs <- list(
  TRI01 = c("01", "02", "03"),
  TRI02 = c("02", "03", "04"),
  TRI03 = c("03", "04", "05"),
  TRI04 = c("04", "05", "06"),
  TRI05 = c("05", "06", "07"),
  TRI06 = c("06", "07", "08"),
  TRI07 = c("07", "08", "09"),
  TRI08 = c("08", "09", "10"),
  TRI09 = c("09", "10", "11"),
  TRI10 = c("10", "11", "12")
)



# 3. Crear la función que calcule todos los indicadores

calc_indicators <- function(data, trimestre) {
  
  # -----------------------------
  # TASA DE DESEMPLEO (tsd)
  # -----------------------------
  data_tsd <- data %>%
    filter(FRZTRA == "si") %>%
    filter(MES %in% trimestre) %>%
    mutate(
      DESOCUP = ifelse(is.na(DESOCUP), 0, 1),
      ACTIVA  = ifelse(is.na(OCUP_EST) & DESOCUP == 0, 0, 1)
    )
  
  dstrata <- data_tsd %>%
    as_survey_design(weights = FEX_C18, nest = FALSE)
  
  ts_des1 <- dstrata %>%
    group_by(PER) %>%
    summarise(
      n       = n(),
      N       = sum(FEX_C18),
      C_DES   = sum(DESOCUP * FEX_C18),
      C_PEA   = sum(ACTIVA  * FEX_C18),
      TAS_DES = survey_ratio(DESOCUP, ACTIVA, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = TAS_DES,
      estimacion_se  = TAS_DES_se,
      estimacion_low = TAS_DES_low,
      estimacion_upp = TAS_DES_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "tsd", des = "ratio",
      CLASE = "Nacional", nivel = "nacional",
      periodicidad = "Trimestre", mes = 0
    )
  
  ts_des2 <- dstrata %>%
    group_by(CLASE) %>%
    summarise(
      n       = n(),
      N       = sum(FEX_C18),
      C_DES   = sum(DESOCUP * FEX_C18),
      C_PEA   = sum(ACTIVA  * FEX_C18),
      TAS_DES = survey_ratio(DESOCUP, ACTIVA, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = TAS_DES,
      estimacion_se  = TAS_DES_se,
      estimacion_low = TAS_DES_low,
      estimacion_upp = TAS_DES_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp, CLASE) %>%
    mutate(
      se = "", ind = "tsd", des = "ratio",
      nivel = "nacional", periodicidad = "Trimestre", mes = 0
    )
  
  ts_des3 <- dstrata %>%
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05")) %>%
    group_by(PER) %>%
    summarise(
      n       = n(),
      N       = sum(FEX_C18),
      C_DES   = sum(DESOCUP * FEX_C18),
      C_PEA   = sum(ACTIVA  * FEX_C18),
      TAS_DES = survey_ratio(DESOCUP, ACTIVA, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = TAS_DES,
      estimacion_se  = TAS_DES_se,
      estimacion_low = TAS_DES_low,
      estimacion_upp = TAS_DES_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "tsd", des = "ratio",
      CLASE = "Nacional", nivel = "13_ciudad",
      periodicidad = "Trimestre", mes = 0
    )
  
  # -----------------------------
  # PORCENTAJE DE OCUPACIÓN (prob)
  # -----------------------------
  data_po <- data %>%
    filter(MES %in% trimestre) %>%
    mutate(
      OCUP = ifelse(is.na(OCUP_EST), 0, 1),
      PET  = ifelse(is.na(PET), 0, 1)
    )
  
  strata_po <- data_po %>%
    as_survey_design(weights = FEX_C18, nest = FALSE)
  
  prop_1 <- strata_po %>%
    group_by(PER) %>%
    summarise(
      n      = n(),
      N      = sum(FEX_C18),
      C_OCU  = sum(OCUP * FEX_C18),
      C_PET  = sum(PET  * FEX_C18),
      prop   = survey_ratio(OCUP, PET, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = prop,
      estimacion_se  = prop_se,
      estimacion_low = prop_low,
      estimacion_upp = prop_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "prob", des = "ratio",
      CLASE = "Nacional", nivel = "nacional",
      periodicidad = "Trimestre", mes = 0
    )
  
  prop_2 <- strata_po %>%
    group_by(CLASE) %>%
    summarise(
      n      = n(),
      N      = sum(FEX_C18),
      C_OCU  = sum(OCUP * FEX_C18),
      C_PET  = sum(PET  * FEX_C18),
      prop   = survey_ratio(OCUP, PET, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = prop,
      estimacion_se  = prop_se,
      estimacion_low = prop_low,
      estimacion_upp = prop_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp, CLASE) %>%
    mutate(
      se = "", ind = "prob", des = "ratio",
      nivel = "nacional", periodicidad = "Trimestre", mes = 0
    )
  
  prop_3 <- strata_po %>%
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05")) %>%
    group_by(PER) %>%
    summarise(
      n      = n(),
      N      = sum(FEX_C18),
      C_OCU  = sum(OCUP * FEX_C18),
      C_PET  = sum(PET  * FEX_C18),
      prop   = survey_ratio(OCUP, PET, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = prop,
      estimacion_se  = prop_se,
      estimacion_low = prop_low,
      estimacion_upp = prop_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "prob", des = "ratio",
      CLASE = "Nacional", nivel = "13_ciudad",
      periodicidad = "Trimestre", mes = 0
    )
  
  # -----------------------------
  # TOTALES DE PERSONAS OCUPADAS (tot_pob)
  # -----------------------------
  data_tpt <- data %>%
    filter(MES %in% trimestre) %>%
    mutate(trabajando = ifelse(is.na(OCUP_EST), 0, 1))
  
  data_tpt_des1 <- data_tpt %>%
    as_survey_design(weights = FEX_C18, nest = FALSE)
  
  tot_pob1 <- data_tpt_des1 %>%
    group_by(PER) %>%
    summarise(
      n     = n(),
      N     = sum(FEX_C18),
      total = survey_total(trabajando, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = total,
      estimacion_se  = total_se,
      estimacion_low = total_low,
      estimacion_upp = total_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "tot_pob", des = "total",
      CLASE = "Nacional", nivel = "nacional",
      periodicidad = "Trimestre", mes = 0
    )
  
  tot_pob2 <- data_tpt_des1 %>%
    group_by(CLASE) %>%
    summarise(
      n     = n(),
      N     = sum(FEX_C18),
      total = survey_total(trabajando, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = total,
      estimacion_se  = total_se,
      estimacion_low = total_low,
      estimacion_upp = total_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp, CLASE) %>%
    mutate(
      se = "", ind = "tot_pob", des = "total",
      nivel = "nacional", periodicidad = "Trimestre", mes = 0
    )
  
  tot_pob3 <- data_tpt_des1 %>%
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05")) %>%
    group_by(PER) %>%
    summarise(
      n     = n(),
      N     = sum(FEX_C18),
      total = survey_total(trabajando, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = total,
      estimacion_se  = total_se,
      estimacion_low = total_low,
      estimacion_upp = total_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "tot_pob", des = "total",
      CLASE = "Nacional", nivel = "13_ciudad",
      periodicidad = "Trimestre", mes = 0
    )
  
  # -----------------------------
  # TOTALES DE PERSONAS DESOCUPADAS (tot_deso)
  # -----------------------------
  data_tpnt <- data %>%
    filter(MES %in% trimestre) %>%
    mutate(no_trabajando = ifelse(is.na(DESOCUP), 0, 1))
  
  data_tpnt_des1 <- data_tpnt %>%
    as_survey_design(weights = FEX_C18, nest = FALSE)
  
  tot_deso1 <- data_tpnt_des1 %>%
    group_by(PER) %>%
    summarise(
      n     = n(),
      N     = sum(FEX_C18),
      total = survey_total(no_trabajando, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = total,
      estimacion_se  = total_se,
      estimacion_low = total_low,
      estimacion_upp = total_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "tot_deso", des = "total",
      CLASE = "Nacional", nivel = "nacional",
      periodicidad = "Trimestre", mes = 0
    )
  
  tot_deso2 <- data_tpnt_des1 %>%
    group_by(CLASE) %>%
    summarise(
      n     = n(),
      N     = sum(FEX_C18),
      total = survey_total(no_trabajando, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = total,
      estimacion_se  = total_se,
      estimacion_low = total_low,
      estimacion_upp = total_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp, CLASE) %>%
    mutate(
      se = "", ind = "tot_deso", des = "total",
      nivel = "nacional", periodicidad = "Trimestre", mes = 0
    )
  
  tot_deso3 <- data_tpnt_des1 %>%
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05")) %>%
    group_by(PER) %>%
    summarise(
      n     = n(),
      N     = sum(FEX_C18),
      total = survey_total(no_trabajando, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = total,
      estimacion_se  = total_se,
      estimacion_low = total_low,
      estimacion_upp = total_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "tot_deso", des = "total",
      CLASE = "Nacional", nivel = "13_ciudad",
      periodicidad = "Trimestre", mes = 0
    )
  
  # -----------------------------
  # TASA GLOBAL DE PARTICIPACIÓN (tgp)
  # -----------------------------
  data_tgp <- data %>%
    filter(MES %in% trimestre) %>%
    mutate(
      PET    = ifelse(is.na(PET), 0, 1),
      FRZTRA = ifelse(is.na(OCUP_EST) & DESOCUP == 0, 0, 1),
      FRZTRA = ifelse(is.na(FRZTRA), 0, FRZTRA)  # Por si queda algún NA
    )
  
  strata_tgp <- data_tgp %>%
    as_survey_design(weights = FEX_C18, nest = FALSE)
  
  TGP_1 <- strata_tgp %>%
    group_by(PER) %>%
    summarise(
      n        = n(),
      N        = sum(FEX_C18),
      C_FRZTRA = sum(FRZTRA * FEX_C18),
      C_PET    = sum(PET    * FEX_C18),
      TGP      = survey_ratio(FRZTRA, PET, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = TGP,
      estimacion_se  = TGP_se,
      estimacion_low = TGP_low,
      estimacion_upp = TGP_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "tgp", des = "ratio",
      CLASE = "Nacional", nivel = "nacional",
      periodicidad = "Trimestre", mes = 0
    )
  
  TGP_2 <- strata_tgp %>%
    group_by(CLASE) %>%
    summarise(
      n        = n(),
      N        = sum(FEX_C18),
      C_FRZTRA = sum(FRZTRA * FEX_C18),
      C_PET    = sum(PET    * FEX_C18),
      TGP      = survey_ratio(FRZTRA, PET, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = TGP,
      estimacion_se  = TGP_se,
      estimacion_low = TGP_low,
      estimacion_upp = TGP_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp, CLASE) %>%
    mutate(
      se = "", ind = "tgp", des = "ratio",
      nivel = "nacional", periodicidad = "Trimestre", mes = 0
    )
  
  TGP_3 <- strata_tgp %>%
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05")) %>%
    group_by(PER) %>%
    summarise(
      n        = n(),
      N        = sum(FEX_C18),
      C_FRZTRA = sum(FRZTRA * FEX_C18),
      C_PET    = sum(PET    * FEX_C18),
      TGP      = survey_ratio(FRZTRA, PET, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = TGP,
      estimacion_se  = TGP_se,
      estimacion_low = TGP_low,
      estimacion_upp = TGP_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "tgp", des = "ratio",
      CLASE = "Nacional", nivel = "13_ciudad",
      periodicidad = "Trimestre", mes = 0
    )
  
  # -----------------------------
  # POBLACIÓN FUERA DE LA FUERZA LABORAL (pffl)
  # -----------------------------
  data_pffl <- data %>%
    filter(FRZTRA == "si") %>%
    filter(MES %in% trimestre) %>%
    mutate(
      PFFL = ifelse(is.na(OCUP_EST) & DESOCUP == 0, 0, 1),
      PFFL = ifelse(is.na(PFFL), 1, PFFL)
    )
  
  data_pffl_des1 <- data_pffl %>%
    as_survey_design(weights = FEX_C18, nest = FALSE)
  
  tot_pffl1 <- data_pffl_des1 %>%
    group_by(PER) %>%
    summarise(
      n     = n(),
      N     = sum(FEX_C18),
      total = survey_total(PFFL, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = total,
      estimacion_se  = total_se,
      estimacion_low = total_low,
      estimacion_upp = total_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "pffl", des = "total",
      CLASE = "Nacional", nivel = "nacional",
      periodicidad = "Trimestre", mes = 0
    )
  
  tot_pffl2 <- data_pffl_des1 %>%
    group_by(CLASE) %>%
    summarise(
      n     = n(),
      N     = sum(FEX_C18),
      total = survey_total(PFFL, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = total,
      estimacion_se  = total_se,
      estimacion_low = total_low,
      estimacion_upp = total_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp, CLASE) %>%
    mutate(
      se = "", ind = "pffl", des = "total",
      nivel = "nacional", periodicidad = "Trimestre", mes = 0
    )
  
  tot_pffl3 <- data_pffl_des1 %>%
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05")) %>%
    group_by(PER) %>%
    summarise(
      n     = n(),
      N     = sum(FEX_C18),
      total = survey_total(PFFL, vartype = c("var","cv","se","ci"), deff = TRUE)
    ) %>%
    mutate(
      estimacion     = total,
      estimacion_se  = total_se,
      estimacion_low = total_low,
      estimacion_upp = total_upp
    ) %>%
    dplyr::select(n, N, estimacion, estimacion_se, estimacion_low, estimacion_upp) %>%
    mutate(
      se = "", ind = "pffl", des = "total",
      CLASE = "Nacional", nivel = "13_ciudad",
      periodicidad = "Trimestre", mes = 0
    )
  
  # -----------------------------
  # Unir todas las tablas en un solo objeto
  # -----------------------------
  modelo <- bind_rows(
    prop_1, prop_2, prop_3,
    ts_des1, ts_des2, ts_des3,
    tot_pob1, tot_pob2, tot_pob3,
    tot_deso1, tot_deso2, tot_deso3,
    TGP_1, TGP_2, TGP_3,
    tot_pffl1, tot_pffl2, tot_pffl3
  )
  
  # Retornar la tabla resultante
  return(modelo)
}



# 4. Iterar sobre la lista de trimestres y guardar resultados

# Ruta base donde se guardarán los archivos
out_dir <- "C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/estimacion directa data/trimestral"

# Creamos los 10 archivos (01TRI_geih.csv, 02TRI_geih.csv, etc.)
for (k in seq_along(TRIs)) {
  
  # 1) Tomamos el trimestre k-ésimo
  trimestre_k <- TRIs[[k]]
  
  # 2) Calculamos los indicadores
  modelo_k <- calc_indicators(data, trimestre_k)
  
  # 3) Armamos el nombre del archivo
  nombre_out <- file.path(out_dir, sprintf("%02dTRI_geih.csv", k))
  
  # 4) Guardamos en CSV
  write_delim(
    modelo_k,
    file = nombre_out,
    delim = "\t"
  )
  
  # Mensaje de control
  cat("Archivo guardado:", nombre_out, "\n")
}



# 5. (Opcional) Unir las tablas generadas y guardarlas en Excel

# Si quieres unir en un solo objeto:
setwd(out_dir)  # Directorio donde guardaste los CSV
files <- sprintf("%02dTRI_geih.csv", 1:10)

base_geih_trim <- lapply(files, function(x) {
  df <- read_delim(x, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
  df <- mutate(df, across(everything(), as.character))  # Convierte todo a character
  return(df)
}) %>%
  bind_rows()

# Ajuste específico si 'des' == 'total'
base_geih_trim <- base_geih_trim %>%
  mutate(
    estimacion = as.numeric(estimacion),
    estimacion = ifelse(des == "total", estimacion / 3, estimacion)
  )

# Agregamos la columna "trim" para identificar
# (24 filas por cada uno de los 10 trimestres en tu ejemplo)
base_geih_trim$trim <- rep(1:10, each = 24)

# Guardar en Excel
nombre <- file.path(out_dir, "base_estimador_real_trimestral.xlsx")
write.xlsx(base_geih_trim, file = nombre)
cat("Archivo Excel unificado guardado:", nombre, "\n")
