rm(list = ls())
library(readr)
library(readxl)
library(dplyr)
require(sae)
library(ggplot2)
library(survey)
library(srvyr)
library(rlang)
library(readxl)

# Cargar datos
data = read_delim("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/base_geih_12.csv", 
                  delim = "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(MES = if_else(MES == '1', '01', MES))

est_error_dane = read_excel("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/estimacion directa data/error_estandar_dane_documentos.xlsx")  

# Lista de valores para la variable indicador
valores_indicador <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

# Iterar sobre los valores del indicador
for (indicador in valores_indicador) {
  
  # Tasa de desempleo
  data_tsd = data %>% filter(FRZTRA == "si") %>% filter(MES == indicador) %>%
    mutate(DESOCUP = ifelse(is.na(DESOCUP), 0, 1),
           ACTIVA = ifelse(is.na(OCUP_EST) & DESOCUP == 0, 0, 1))
  
  dstrata <- data_tsd %>%
    as_survey_design(weights = FEX_C18, nest = F)
  
  ts_des0 <- dstrata %>%
    group_by(PER) %>%
    summarise(n = n(), N = sum(FEX_C18), C_DES = sum(DESOCUP * FEX_C18), C_PEA = sum(ACTIVA * FEX_C18),
              TAS_DES = survey_ratio(DESOCUP, ACTIVA, vartype = c("var", "cv", "se", "ci"), deff = T)) %>%
    mutate(estimacion = TAS_DES, estimacion_se = TAS_DES_se, estimacion_low = TAS_DES_low, estimacion_upp = TAS_DES_upp) %>% 
    dplyr::select(c('n', 'N', 'estimacion', 'estimacion_se', 'estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == 'nacional') %>% pull(ts_des), 
           ind = "tsd", des = 'ratio')
  
  ts_des00 <- dstrata %>%
    filter(MUNICIPIO %in% c("76", "73", "68", "66", "54", "52", "50", "23", "17", "13", "11", "08", "05")) %>%
    group_by(PER) %>%
    summarise(n = n(), N = sum(FEX_C18), C_DES = sum(DESOCUP * FEX_C18), C_PEA = sum(ACTIVA * FEX_C18),
              TAS_DES = survey_ratio(DESOCUP, ACTIVA, vartype = c("var", "cv", "se", "ci"), deff = T)) %>%
    mutate(estimacion = TAS_DES, estimacion_se = TAS_DES_se, estimacion_low = TAS_DES_low, estimacion_upp = TAS_DES_upp) %>% 
    dplyr::select(c('n', 'N', 'estimacion', 'estimacion_se', 'estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == '13_ciudad') %>% pull(ts_des),
           ind = "tsd", des = 'ratio')
  
  # Porcentaje de ocupación-------------------------------------------------------
  
  data_po = data %>%  filter(MES == indicador) %>%
    mutate(OCUP = ifelse(is.na(OCUP_EST),0,1),
           PET = ifelse(is.na(PET),0,1))
  
  strata_po <- data_po %>%
    as_survey_design(#ds = id,
      #strata = estrato,
      weights = FEX_C18,
      nest = F
    )
  
  prop_0 <- strata_po %>%
    group_by(PER) %>%
    summarise(n=n(),N = sum(FEX_C18),C_OCU=sum(OCUP*FEX_C18),C_PET=sum(PET*FEX_C18),
              prop = survey_ratio(OCUP,PET,vartype = c("var","cv","se","ci"),deff=T))%>%
    mutate(estimacion = prop, estimacion_se = prop_se, estimacion_low = prop_low, estimacion_upp = prop_upp) %>% 
    dplyr::select(c('n','N','estimacion','estimacion_se','estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == 'nacional')%>% pull(prop), ind = "prob",des='ratio')
  prop_0
  
  prop_00 <- strata_po %>%
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05"))%>%
    group_by(PER) %>%
    summarise(n=n(),N = sum(FEX_C18),C_OCU=sum(OCUP*FEX_C18),C_PET=sum(PET*FEX_C18),
              prop = survey_ratio(OCUP,PET,vartype = c("var","cv","se","ci"),deff=T))%>%
    mutate(estimacion = prop, estimacion_se = prop_se, estimacion_low = prop_low, estimacion_upp = prop_upp) %>% 
    dplyr::select(c('n','N','estimacion','estimacion_se','estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == '13_ciudad')%>% pull(prop), ind = "prob",des='ratio')
  prop_00
  
  # Totales de personas ocupadas--------------------------------------------------
  
  data_tpt <- data %>% filter(MES == indicador) %>%
    mutate(trabajando = ifelse(is.na(OCUP_EST),0,1))
  
  data_tpt.des1 <- data_tpt %>%
    as_survey_design( #ids = id3, 
      #strata = 1/FEX_C18,
      weights = FEX_C18,
      nest = F)
  
  tot_pob0 = data_tpt.des1 %>% 
    group_by(PER) %>%
    summarise(n=n(),N = sum(FEX_C18),
              total = survey_total(trabajando,vartype = c("var","cv", "se", "ci"),deff=T))%>%
    mutate(estimacion = total, estimacion_se = total_se, estimacion_low = total_low, estimacion_upp = total_upp) %>% 
    dplyr::select(c('n','N','estimacion','estimacion_se','estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == 'nacional')%>% pull(tot_pob), ind = "tot_pob",des='total')
  tot_pob0
  
  tot_pob00 = data_tpt.des1 %>%
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05"))%>% 
    group_by(PER) %>%
    summarise(n=n(),N = sum(FEX_C18),
              total = survey_total(trabajando,vartype = c("var","cv", "se", "ci"),deff=T))%>%
    mutate(estimacion = total, estimacion_se = total_se, estimacion_low = total_low, estimacion_upp = total_upp) %>% 
    dplyr::select(c('n','N','estimacion','estimacion_se','estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == '13_ciudad')%>% pull(tot_pob), ind = "tot_pob",des='total')
  tot_pob00
  
  # Totales de personas desocupadas-----------------------------------------------
  
  data_tpnt <- data %>%  filter(MES == indicador) %>%
    mutate(no_trabajando = ifelse(is.na(DESOCUP),0,1))
  
  data_tpnt.des1 <- data_tpnt %>%
    as_survey_design( #ids = id3, 
      #strata = 1/FEX_C18,
      weights = FEX_C18,
      nest = F)
  
  tot_deso0 = data_tpnt.des1 %>% 
    group_by(PER) %>%
    summarise(n=n(),N = sum(FEX_C18),
              total = survey_total(no_trabajando,vartype = c("var","cv", "se", "ci"),deff=T))%>%
    mutate(estimacion = total, estimacion_se = total_se, estimacion_low = total_low, estimacion_upp = total_upp) %>% 
    dplyr::select(c('n','N','estimacion','estimacion_se','estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == 'nacional')%>% pull(tot_deso), ind = "tot_deso",des='total')
  tot_deso0
  
  tot_deso00 = data_tpnt.des1 %>%
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05"))%>% 
    group_by(PER) %>%
    summarise(n=n(),N = sum(FEX_C18),
              total = survey_total(no_trabajando,vartype = c("var","cv", "se", "ci"),deff=T))%>%
    mutate(estimacion = total, estimacion_se = total_se, estimacion_low = total_low, estimacion_upp = total_upp) %>% 
    dplyr::select(c('n','N','estimacion','estimacion_se','estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == '13_ciudad')%>% pull(tot_deso), ind = "tot_deso",des='total')
  tot_deso00
  
  # Tasa global de participación -------------------------------------------------
  
  data_tgp = data %>% filter(MES == indicador) %>%
    mutate(PET = ifelse(is.na(PET),0,1),
           #PET = ifelse(is.na(PET),0,1))
           FRZTRA = ifelse(is.na(OCUP_EST) & DESOCUP==0,0,1),
           FRZTRA = ifelse(is.na(FRZTRA),0,1))
  
  strata_tgp <- data_tgp %>%
    as_survey_design(#ds = id,
      #strata = estrato,
      weights = FEX_C18,
      nest = F
    )
  
  tgp_0 <- strata_tgp %>%
    group_by(PER) %>%
    summarise(n=n(),N = sum(FEX_C18),C_FRZTRA=sum(FRZTRA*FEX_C18),C_PET=sum(PET*FEX_C18),
              tgp = survey_ratio(FRZTRA,PET,vartype = c("var","cv","se","ci"),deff=T))%>%
    mutate(estimacion = tgp, estimacion_se = tgp_se, estimacion_low = tgp_low, estimacion_upp = tgp_upp) %>% 
    dplyr::select(c('n','N','estimacion','estimacion_se','estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == 'nacional')%>% pull(TGP), ind = "tgp",des='ratio')
  tgp_0
  
  tgp_00 <- strata_tgp %>%
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05"))%>%
    group_by(PER) %>%
    summarise(n=n(),N = sum(FEX_C18),C_FRZTRA=sum(FRZTRA*FEX_C18),C_PET=sum(PET*FEX_C18),
              tgp = survey_ratio(FRZTRA,PET,vartype = c("var","cv","se","ci"),deff=T))%>%
    mutate(estimacion = tgp, estimacion_se = tgp_se, estimacion_low = tgp_low, estimacion_upp = tgp_upp) %>% 
    dplyr::select(c('n','N','estimacion','estimacion_se','estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == '13_ciudad')%>% pull(TGP), ind = "tgp",des='ratio')
  tgp_00
  
  # Población fuera de la fuerza laboral (PFFL) ---------------------------------
  
  data_pffl <- data %>% filter(FRZTRA == 'si') %>%  filter(MES == indicador) %>%
    mutate(
      PFFL = ifelse(is.na(OCUP_EST) & DESOCUP==0,0,1),
      PFFL = ifelse(is.na(PFFL),1,0))
  
  data_pffl.des1 <- data_pffl %>%
    as_survey_design( #ids = id3, 
      #strata = 1/FEX_C18,
      weights = FEX_C18,
      nest = F)
  
  tot_pffl0 = data_pffl.des1 %>% 
    group_by(PER) %>%
    summarise(n=n(),N = sum(FEX_C18),
              total = survey_total(PFFL,vartype = c("var","cv", "se", "ci"),deff=T))%>%
    mutate(estimacion = total, estimacion_se = total_se, estimacion_low = total_low, estimacion_upp = total_upp) %>% 
    dplyr::select(c('n','N','estimacion','estimacion_se','estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == 'nacional')%>% pull(tot_pffl), ind = "pffl",des='total')
  tot_pffl0
  
  tot_pffl00 = data_pffl.des1 %>% 
    filter(MUNICIPIO %in% c("76","73","68","66","54","52","50","23","17","13","11","08","05"))%>% 
    group_by(PER) %>%
    summarise(n=n(),N = sum(FEX_C18),
              total = survey_total(PFFL,vartype = c("var","cv", "se", "ci"),deff=T))%>%
    mutate(estimacion = total, estimacion_se = total_se, estimacion_low = total_low, estimacion_upp = total_upp) %>% 
    dplyr::select(c('n','N','estimacion','estimacion_se','estimacion_low', 'estimacion_upp')) %>% 
    mutate(se = est_error_dane %>% filter(Mes == indicador & Estimador == '13_ciudad')%>% pull(tot_pffl), ind = "pffl",des='total')
  tot_pffl00
  
  # Une todos los resultados
  modelo <- rbind(prop_0, prop_00, ts_des0, ts_des00, tot_pob0, tot_pob00, tot_deso0, tot_deso00, tgp_0, tgp_00, tot_pffl0, tot_pffl00)
  
  # Guardar resultados en archivo
  nombre <- paste0("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/estimacion directa data/", indicador, "_geih.csv")
  write_delim(modelo, file = nombre, delim = "\t")
}
