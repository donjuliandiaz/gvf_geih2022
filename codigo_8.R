# Cargar librerías necesarias

rm(list = ls())
library(readr)
library(dplyr)

# Leer la base principal y procesarla
base_final <- read_delim("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/base_final.csv", 
                         delim = "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  filter(!is.na(se)) %>% 
  mutate(atipico = ifelse(mes %in% c(3:5) & periodicidad == 'Mensual', 'atip', 'no_atip'),
         CLASE = ifelse(nivel == '13_ciudad', "13 M.C.", CLASE), 
         CLASE = ifelse(CLASE == 'Urbano', 'Urban', CLASE),
         CLASE = ifelse(CLASE == 'Nacional', 'National', CLASE),
         periodicidad = ifelse(periodicidad == "Mensual", "Monthly", "Quarterly"))

# Parámetro de ajuste
n_ajust <- 30

# Crear base_final_propose_ratio
base_final_propose_ratio <- base_final %>%
  filter(n >= n_ajust & se != 0, des == "ratio") %>%
  mutate(vi_estimacion = se^2 / (estimacion^2),
         log_vi_estimacion = log(vi_estimacion),
         y_est_inv_estimacion = estimacion^-1,
         y_est_inv_sqrt_estimacion = estimacion^(-1/2),
         n_inv_estimacion = 1 / n,
         n_y_estimacion = (n * estimacion)^(1/2))

# Crear base_final_propose_total
base_final_propose_total <- base_final %>%
  filter(n >= n_ajust & se != 0, des == "total") %>%
  mutate(vi_estimacion = se^2 / (estimacion^2),
         log_vi_estimacion = log(vi_estimacion),
         y_est_inv_estimacion = estimacion^-1,
         y_est_inv_sqrt_estimacion = estimacion^(-1/2),
         n_inv_estimacion = 1 / n,
         n_y_estimacion = (n * estimacion)^(1/2))

# Leer base_2023 y procesarla para 'ratio'
base_2023 <- read_delim("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2023/estimacion directa data/base_estimador_real_completo.csv", 
                        delim = "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  filter(!is.na(se))

base_2023_ratio <- base_2023 %>% filter(des == 'ratio')
dummy_vars1 <- model.matrix(~ind - 1, data = base_2023_ratio)
dummy_df1 <- as.data.frame(dummy_vars1)
base_2023_ratio <- cbind(base_2023_ratio, dummy_df1)

# Modelo para 'ratio'
gvf.propose9_v1 <- glm(log_vi_estimacion ~ atipico + ind + n + estimacion, 
                       family = gaussian(link = "identity"), data = base_final_propose_ratio)

# Calcular se_propose9 para 'ratio'
base_2023_ratio <- base_2023_ratio %>% 
  mutate(se_propose9 = sqrt(exp(
    gvf.propose9_v1$coefficients[1] +
      gvf.propose9_v1$coefficients[2]*1 +
      gvf.propose9_v1$coefficients[3]*indtgp +
      gvf.propose9_v1$coefficients[4]*indtsd +
      0*indprob +
      gvf.propose9_v1$coefficients[5]*n +
      gvf.propose9_v1$coefficients[6]*estimacion
  ) * exp(gvf.propose9_v1$deviance / gvf.propose9_v1$df.residual / 2) * (estimacion)^2))

# Seleccionar columnas relevantes
base_2023_ratio <- base_2023_ratio %>% 
  dplyr::select(n, N, estimacion, ind, des, mes, CLASE, nivel, periodicidad, estimacion_se, se, se_propose9)

# Procesar datos para 'total'
base_2023_total <- base_2023 %>% filter(des == 'total')
dummy_vars2 <- model.matrix(~ind - 1, data = base_2023_total)
dummy_df2 <- as.data.frame(dummy_vars2)
base_2023_total <- cbind(base_2023_total, dummy_df2)

# Modelo para 'total'
gvf.propose9_v2 <- glm(log_vi_estimacion ~ atipico + ind + n + estimacion, 
                       family = gaussian(link = "identity"), data = base_final_propose_total)

# Calcular se_propose9 para 'total'
base_2023_total <- base_2023_total %>% 
  mutate(se_propose9 = sqrt(exp(
    gvf.propose9_v2$coefficients[1] + 
      gvf.propose9_v2$coefficients[2]*1 +   
      gvf.propose9_v2$coefficients[3]*indtot_deso +     
      gvf.propose9_v2$coefficients[4]*indtot_pob +     
      0*indpffl +
      gvf.propose9_v2$coefficients[5]*n +    
      gvf.propose9_v2$coefficients[6]*estimacion
  ) * exp(gvf.propose9_v2$deviance / gvf.propose9_v2$df.residual / 2) * (estimacion)^2))

# Seleccionar columnas relevantes
base_2023_total <- base_2023_total %>% 
  dplyr::select(n, N, estimacion, ind, des, mes, CLASE, nivel, periodicidad, estimacion_se, se, se_propose9)

# Combinar las bases procesadas
base_2023_final <- rbind(base_2023_ratio, base_2023_total)

# Guardar los resultados en un archivo
nombre <- paste0("C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/validacion 2023/base_test_2023.csv")
write_delim(base_2023_final, file = nombre, delim = "\t")

# Evaluación del modelo para 'ratio'
base_test_2023 <- read_delim(nombre, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
base_test_2023_ratio <- base_test_2023 %>% filter(des == 'ratio')

# Métricas para 'ratio'
residuals_ratio <- base_test_2023_ratio$se_propose9 - base_test_2023_ratio$se
mse_ratio <- mean(residuals_ratio^2)
rmse_ratio <- sqrt(mse_ratio)
mae_ratio <- mean(abs(residuals_ratio))
mape_ratio <- mean(abs(residuals_ratio / base_test_2023_ratio$se)) * 100
min_max_accuracy_ratio <- 1 - (sum(abs(residuals_ratio)) / sum(abs(base_test_2023_ratio$se - mean(base_test_2023_ratio$se))))
nrmse_ratio <- rmse_ratio / (max(base_test_2023_ratio$se) - min(base_test_2023_ratio$se))
efron_r2_ratio <- 1 - sum(residuals_ratio^2) / sum((base_test_2023_ratio$se - mean(base_test_2023_ratio$se))^2)

# Imprimir métricas para 'ratio'
cat("Ratio Metrics:\n")
cat(paste("MSE:", mse_ratio, "RMSE:", rmse_ratio, "MAE:", mae_ratio, "MAPE:", mape_ratio, 
          "Min-Max Accuracy:", min_max_accuracy_ratio, "NRMSE:", nrmse_ratio, "Efron's R2:", efron_r2_ratio, "\n"))

# Evaluación del modelo para 'total'
base_test_2023_total <- base_test_2023 %>% filter(des == 'total')
residuals_total <- base_test_2023_total$se_propose9 - base_test_2023_total$se
mse_total <- mean(residuals_total^2)
rmse_total <- sqrt(mse_total)
mae_total <- mean(abs(residuals_total))
mape_total <- mean(abs(residuals_total / base_test_2023_total$se)) * 100
min_max_accuracy_total <- 1 - (sum(abs(residuals_total)) / sum(abs(base_test_2023_total$se - mean(base_test_2023_total$se))))
nrmse_total <- rmse_total / (max(base_test_2023_total$se) - min(base_test_2023_total$se))
efron_r2_total <- 1 - sum(residuals_total^2) / sum((base_test_2023_total$se - mean(base_test_2023_total$se))^2)

# Imprimir métricas para 'total'
cat("Total Metrics:\n")
cat(paste("MSE:", mse_total, "RMSE:", rmse_total, "MAE:", mae_total, "MAPE:", mape_total, 
          "Min-Max Accuracy:", min_max_accuracy_total, "NRMSE:", nrmse_total, "Efron's R2:", efron_r2_total, "\n"))
