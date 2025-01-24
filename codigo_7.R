# Base inicial optimizada para cálculos posteriores

rm(list = ls())
library(readxl)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(broom)
library(purrr)
library(performance)
library(car)
library(cowplot)

# Cargar la base de datos
base_final <- read_delim(
  "C:/Users/USER/Documents/USTA/Modalidad de grado/gvf_geih2022/Estimaciones DANE-2022/base_final.csv",
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  filter(!is.na(se)) %>%
  mutate(
    atipico = ifelse(mes %in% 3:5 & periodicidad == 'Mensual', 'atip', 'no_atip'),
    CLASE = case_when(
      nivel == '13_ciudad' ~ '13 M.C.',
      CLASE == 'Urbano' ~ 'Urban',
      CLASE == 'Nacional' ~ 'National',
      TRUE ~ CLASE
    ),
    periodicidad = ifelse(periodicidad == 'Mensual', 'Monthly', 'Quarterly')
  )

# Modelos teóricos
## Ratio
base_final_teoric_ratio <- base_final %>%
  filter(n >= 30 & se != 0, des == "ratio") %>%
  mutate(
    vi_estimacion = se^2 / (estimacion^2),
    log_vi_estimacion = log(vi_estimacion),
    y_est_inv_estimacion = estimacion^-1,
    y_est_inv_sqrt_estimacion = estimacion^(-1/2),
    n_inv_estimacion = 1 / n,
    n_y_estimacion = (n * estimacion)^(1/2)
  )

gvf.nule <- glm(log_vi_estimacion ~ 1, family = gaussian(link = "identity"), data = base_final_teoric_ratio)

gvf.krenzke <- glm(
  log_vi_estimacion ~ y_est_inv_estimacion + y_est_inv_sqrt_estimacion,
  family = gaussian(link = "identity"),
  data = base_final_teoric_ratio
)

gvf.comun <- glm(
  log_vi_estimacion ~ n + estimacion + estimacion:n,
  family = gaussian(link = "identity"),
  data = base_final_teoric_ratio
)

gvf.propose <- try(glm(
  log_vi_estimacion ~ n + estimacion + n_y_estimacion,
  family = gaussian(link = "identity"),
  data = base_final_teoric_ratio
))

gvf.teoric1 <- glm(
  log_vi_estimacion ~ -1 + I(estimacion^2) + estimacion,
  family = gaussian(link = "identity"),
  data = base_final_teoric_ratio
)

gvf.teoric2 <- glm(
  log_vi_estimacion ~ 1 + y_est_inv_estimacion,
  family = gaussian(link = "identity"),
  data = base_final_teoric_ratio
)

gvf.teoric3 <- glm(
  log_vi_estimacion ~ 1 + y_est_inv_estimacion + y_est_inv_estimacion^2,
  family = gaussian(link = "identity"),
  data = base_final_teoric_ratio
)

gvf.teoric4 <- glm(
  log_vi_estimacion ~ 1 + log(estimacion),
  family = gaussian(link = "identity"),
  data = base_final_teoric_ratio
)

gvf.teoric5 <- glm(
  log_vi_estimacion ~ estimacion,
  family = gaussian(link = "identity"),
  data = base_final_teoric_ratio
)

gvf.teoric6 <- glm(
  log_vi_estimacion ~ estimacion + n,
  family = gaussian(link = "identity"),
  data = base_final_teoric_ratio
)

# Calcular errores estándar ajustados
v <- gvf.nule$deviance / gvf.nule$df.residual
base_final_teoric_ratio$se.nule <- sqrt(exp(v / 2) * exp(gvf.nule$fitted.values) * base_final_teoric_ratio$estimacion^2)

v <- gvf.krenzke$deviance / gvf.krenzke$df.residual
base_final_teoric_ratio$se.krenzke <- sqrt(exp(v / 2) * exp(gvf.krenzke$fitted.values) * base_final_teoric_ratio$estimacion^2)

v <- gvf.comun$deviance / gvf.comun$df.residual
base_final_teoric_ratio$se.comun <- sqrt(exp(v / 2) * exp(gvf.comun$fitted.values) * base_final_teoric_ratio$estimacion^2)

v <- gvf.propose$deviance / gvf.propose$df.residual
base_final_teoric_ratio$se.propose <- sqrt(exp(v / 2) * exp(gvf.propose$fitted.values) * base_final_teoric_ratio$estimacion^2)

v <- gvf.teoric1$deviance / gvf.teoric1$df.residual
base_final_teoric_ratio$se.teoric1 <- sqrt(exp(v / 2) * exp(gvf.teoric1$fitted.values) * base_final_teoric_ratio$estimacion^2)

v <- gvf.teoric2$deviance / gvf.teoric2$df.residual
base_final_teoric_ratio$se.teoric2 <- sqrt(exp(v / 2) * exp(gvf.teoric2$fitted.values) * base_final_teoric_ratio$estimacion^2)

v <- gvf.teoric3$deviance / gvf.teoric3$df.residual
base_final_teoric_ratio$se.teoric3 <- sqrt(exp(v / 2) * exp(gvf.teoric3$fitted.values) * base_final_teoric_ratio$estimacion^2)

v <- gvf.teoric4$deviance / gvf.teoric4$df.residual
base_final_teoric_ratio$se.teoric4 <- sqrt(exp(v / 2) * exp(gvf.teoric4$fitted.values) * base_final_teoric_ratio$estimacion^2)

v <- gvf.teoric5$deviance / gvf.teoric5$df.residual
base_final_teoric_ratio$se.teoric5 <- sqrt(exp(v / 2) * exp(gvf.teoric5$fitted.values) * base_final_teoric_ratio$estimacion^2)

v <- gvf.teoric6$deviance / gvf.teoric6$df.residual
base_final_teoric_ratio$se.teoric6 <- sqrt(exp(v / 2) * exp(gvf.teoric6$fitted.values) * base_final_teoric_ratio$estimacion^2)

# Diagnósticos de los modelos
diag.models <- lapply(
  list(
    gvf.nule, gvf.krenzke, gvf.comun, gvf.propose,
    gvf.teoric1, gvf.teoric2, gvf.teoric3, gvf.teoric4, gvf.teoric5, gvf.teoric6
  ),
  glance
)

diag.models_estimacion <- diag.models %>%
  bind_rows(.id = "model") %>%
  mutate(
    rmse = c(
      sqrt(sum((base_final_teoric_ratio$se - base_final_teoric_ratio$se.nule)^2)),
      sqrt(sum((base_final_teoric_ratio$se - base_final_teoric_ratio$se.krenzke)^2)),
      sqrt(sum((base_final_teoric_ratio$se - base_final_teoric_ratio$se.comun)^2)),
      sqrt(sum((base_final_teoric_ratio$se - base_final_teoric_ratio$se.propose)^2)),
      sqrt(sum((base_final_teoric_ratio$se - base_final_teoric_ratio$se.teoric1)^2)),
      sqrt(sum((base_final_teoric_ratio$se - base_final_teoric_ratio$se.teoric2)^2)),
      sqrt(sum((base_final_teoric_ratio$se - base_final_teoric_ratio$se.teoric3)^2)),
      sqrt(sum((base_final_teoric_ratio$se - base_final_teoric_ratio$se.teoric4)^2)),
      sqrt(sum((base_final_teoric_ratio$se - base_final_teoric_ratio$se.teoric5)^2)),
      sqrt(sum((base_final_teoric_ratio$se - base_final_teoric_ratio$se.teoric6)^2))
    ),
    r2_nag = c(
      r2_nagelkerke(gvf.nule),
      r2_nagelkerke(gvf.krenzke),
      r2_nagelkerke(gvf.comun),
      r2_nagelkerke(gvf.propose),
      r2_nagelkerke(gvf.teoric1),
      r2_nagelkerke(gvf.teoric2),
      r2_nagelkerke(gvf.teoric3),
      r2_nagelkerke(gvf.teoric4),
      r2_nagelkerke(gvf.teoric5),
      r2_nagelkerke(gvf.teoric6)
    )
  )

coef.gvf.models_estimacion <- lapply(
  list(
    gvf.nule, gvf.krenzke, gvf.comun, gvf.propose,
    gvf.teoric1, gvf.teoric2, gvf.teoric3, gvf.teoric4, gvf.teoric5, gvf.teoric6
  ),
  coef
)

models.gvf_estimacion <- list(
  nule = gvf.nule, krenzke = gvf.krenzke, comun = gvf.comun, propose = gvf.propose,
  teoric1 = gvf.teoric1, teoric2 = gvf.teoric2, teoric3 = gvf.teoric3,
  teoric4 = gvf.teoric4, teoric5 = gvf.teoric5, teoric6 = gvf.teoric6
)

results_estimacion_ratio <- list(
  base_final_teoric_ratio = base_final_teoric_ratio,
  models.gvf = models.gvf_estimacion,
  models.diag = diag.models_estimacion,
  models.coef = coef.gvf.models_estimacion
)

results_estimacion_ratio$models.diag$model <- c(
  "gvf.nule", "gvf.krenzke", "gvf.comun", "gvf.propose",
  "gvf.teoric1", "gvf.teoric2", "gvf.teoric3", "gvf.teoric4",
  "gvf.teoric5", "gvf.teoric6"
)

results_estimacion_ratio$models.diag$param <- rep("ratio", 10)

# Modelos teóricos
## Total
base_final_teoric_total <- base_final %>%
  filter(n >= 30 & se != 0, des == "total") %>%
  mutate(
    vi_estimacion = se^2 / (estimacion^2),
    log_vi_estimacion = log(vi_estimacion),
    y_est_inv_estimacion = estimacion^-1,
    y_est_inv_sqrt_estimacion = estimacion^(-1/2),
    n_inv_estimacion = 1 / n,
    n_y_estimacion = (n * estimacion)^(1/2)
  )

gvf.nule <- glm(log_vi_estimacion ~ 1, family = gaussian(link = "identity"), data = base_final_teoric_total)

gvf.krenzke <- glm(
  log_vi_estimacion ~ y_est_inv_estimacion + y_est_inv_sqrt_estimacion,
  family = gaussian(link = "identity"),
  data = base_final_teoric_total
)

gvf.comun <- glm(
  log_vi_estimacion ~ n + estimacion + estimacion:n,
  family = gaussian(link = "identity"),
  data = base_final_teoric_total
)

gvf.propose <- try(glm(
  log_vi_estimacion ~ n + estimacion + n_y_estimacion,
  family = gaussian(link = "identity"),
  data = base_final_teoric_total
))

gvf.teoric1 <- glm(
  log_vi_estimacion ~ -1 + I(estimacion^2) + estimacion,
  family = gaussian(link = "identity"),
  data = base_final_teoric_total
)

gvf.teoric2 <- glm(
  log_vi_estimacion ~ 1 + y_est_inv_estimacion,
  family = gaussian(link = "identity"),
  data = base_final_teoric_total
)

gvf.teoric3 <- glm(
  log_vi_estimacion ~ 1 + y_est_inv_estimacion + y_est_inv_estimacion^2,
  family = gaussian(link = "identity"),
  data = base_final_teoric_total
)

gvf.teoric4 <- glm(
  log_vi_estimacion ~ 1 + log(estimacion),
  family = gaussian(link = "identity"),
  data = base_final_teoric_total
)

gvf.teoric5 <- glm(
  log_vi_estimacion ~ estimacion,
  family = gaussian(link = "identity"),
  data = base_final_teoric_total
)

gvf.teoric6 <- glm(
  log_vi_estimacion ~ estimacion + n,
  family = gaussian(link = "identity"),
  data = base_final_teoric_total
)

# Calcular errores estándar ajustados
v <- gvf.nule$deviance / gvf.nule$df.residual
base_final_teoric_total$se.nule <- sqrt(exp(v / 2) * exp(gvf.nule$fitted.values) * base_final_teoric_total$estimacion^2)

v <- gvf.krenzke$deviance / gvf.krenzke$df.residual
base_final_teoric_total$se.krenzke <- sqrt(exp(v / 2) * exp(gvf.krenzke$fitted.values) * base_final_teoric_total$estimacion^2)

v <- gvf.comun$deviance / gvf.comun$df.residual
base_final_teoric_total$se.comun <- sqrt(exp(v / 2) * exp(gvf.comun$fitted.values) * base_final_teoric_total$estimacion^2)

v <- gvf.propose$deviance / gvf.propose$df.residual
base_final_teoric_total$se.propose <- sqrt(exp(v / 2) * exp(gvf.propose$fitted.values) * base_final_teoric_total$estimacion^2)

v <- gvf.teoric1$deviance / gvf.teoric1$df.residual
base_final_teoric_total$se.teoric1 <- sqrt(exp(v / 2) * exp(gvf.teoric1$fitted.values) * base_final_teoric_total$estimacion^2)

v <- gvf.teoric2$deviance / gvf.teoric2$df.residual
base_final_teoric_total$se.teoric2 <- sqrt(exp(v / 2) * exp(gvf.teoric2$fitted.values) * base_final_teoric_total$estimacion^2)

v <- gvf.teoric3$deviance / gvf.teoric3$df.residual
base_final_teoric_total$se.teoric3 <- sqrt(exp(v / 2) * exp(gvf.teoric3$fitted.values) * base_final_teoric_total$estimacion^2)

v <- gvf.teoric4$deviance / gvf.teoric4$df.residual
base_final_teoric_total$se.teoric4 <- sqrt(exp(v / 2) * exp(gvf.teoric4$fitted.values) * base_final_teoric_total$estimacion^2)

v <- gvf.teoric5$deviance / gvf.teoric5$df.residual
base_final_teoric_total$se.teoric5 <- sqrt(exp(v / 2) * exp(gvf.teoric5$fitted.values) * base_final_teoric_total$estimacion^2)

v <- gvf.teoric6$deviance / gvf.teoric6$df.residual
base_final_teoric_total$se.teoric6 <- sqrt(exp(v / 2) * exp(gvf.teoric6$fitted.values) * base_final_teoric_total$estimacion^2)

# Diagnósticos de los modelos
diag.models <- lapply(
  list(
    gvf.nule, gvf.krenzke, gvf.comun, gvf.propose,
    gvf.teoric1, gvf.teoric2, gvf.teoric3, gvf.teoric4, gvf.teoric5, gvf.teoric6
  ),
  glance
)

diag.models_estimacion <- diag.models %>%
  bind_rows(.id = "model") %>%
  mutate(
    rmse = c(
      sqrt(sum((base_final_teoric_total$se - base_final_teoric_total$se.nule)^2)),
      sqrt(sum((base_final_teoric_total$se - base_final_teoric_total$se.krenzke)^2)),
      sqrt(sum((base_final_teoric_total$se - base_final_teoric_total$se.comun)^2)),
      sqrt(sum((base_final_teoric_total$se - base_final_teoric_total$se.propose)^2)),
      sqrt(sum((base_final_teoric_total$se - base_final_teoric_total$se.teoric1)^2)),
      sqrt(sum((base_final_teoric_total$se - base_final_teoric_total$se.teoric2)^2)),
      sqrt(sum((base_final_teoric_total$se - base_final_teoric_total$se.teoric3)^2)),
      sqrt(sum((base_final_teoric_total$se - base_final_teoric_total$se.teoric4)^2)),
      sqrt(sum((base_final_teoric_total$se - base_final_teoric_total$se.teoric5)^2)),
      sqrt(sum((base_final_teoric_total$se - base_final_teoric_total$se.teoric6)^2))
    ),
    r2_nag = c(
      r2_nagelkerke(gvf.nule),
      r2_nagelkerke(gvf.krenzke),
      r2_nagelkerke(gvf.comun),
      r2_nagelkerke(gvf.propose),
      r2_nagelkerke(gvf.teoric1),
      r2_nagelkerke(gvf.teoric2),
      r2_nagelkerke(gvf.teoric3),
      r2_nagelkerke(gvf.teoric4),
      r2_nagelkerke(gvf.teoric5),
      r2_nagelkerke(gvf.teoric6)
    )
  )

coef.gvf.models_estimacion <- lapply(
  list(
    gvf.nule, gvf.krenzke, gvf.comun, gvf.propose,
    gvf.teoric1, gvf.teoric2, gvf.teoric3, gvf.teoric4, gvf.teoric5, gvf.teoric6
  ),
  coef
)

models.gvf_estimacion <- list(
  nule = gvf.nule, krenzke = gvf.krenzke, comun = gvf.comun, propose = gvf.propose,
  teoric1 = gvf.teoric1, teoric2 = gvf.teoric2, teoric3 = gvf.teoric3,
  teoric4 = gvf.teoric4, teoric5 = gvf.teoric5, teoric6 = gvf.teoric6
)

results_estimacion_total <- list(
  base_final_teoric_total = base_final_teoric_total,
  models.gvf = models.gvf_estimacion,
  models.diag = diag.models_estimacion,
  models.coef = coef.gvf.models_estimacion
)

results_estimacion_total$models.diag$model <- c(
  "gvf.nule", "gvf.krenzke", "gvf.comun", "gvf.propose",
  "gvf.teoric1", "gvf.teoric2", "gvf.teoric3", "gvf.teoric4",
  "gvf.teoric5", "gvf.teoric6"
)

results_estimacion_total$models.diag$param <- rep("total", 10)

base_final_propose_ratio <- base_final %>%
  filter(n >= 30 & se != 0, des == "ratio") %>%
  mutate(
    vi_estimacion = se^2 / (estimacion^2),
    log_vi_estimacion = log(vi_estimacion),
    y_est_inv_estimacion = estimacion^-1,
    y_est_inv_sqrt_estimacion = estimacion^(-1/2),
    n_inv_estimacion = 1 / n,
    n_y_estimacion = (n * estimacion)^(1/2)
  )

# Modelos desplegados
gvf.nule <- glm(log_vi_estimacion ~ 1, family = gaussian(link = "identity"), data = base_final_propose_ratio)

gvf.propose1 <- glm(
  log_vi_estimacion ~ y_est_inv_estimacion + y_est_inv_sqrt_estimacion + atipico,
  family = gaussian(link = "identity"),
  data = base_final_propose_ratio
)

gvf.propose2 <- glm(
  log_vi_estimacion ~ estimacion + n + estimacion:n + atipico,
  family = gaussian(link = "identity"),
  data = base_final_propose_ratio
)

gvf.propose3 <- glm(
  log_vi_estimacion ~ n + estimacion + n_y_estimacion + atipico,
  family = gaussian(link = "identity"),
  data = base_final_propose_ratio
)

gvf.propose4 <- glm(
  log_vi_estimacion ~ periodicidad + y_est_inv_estimacion + n,
  family = gaussian(link = "identity"),
  data = base_final_propose_ratio
)

gvf.propose5 <- glm(
  log_vi_estimacion ~ y_est_inv_estimacion + periodicidad + n_y_estimacion,
  family = gaussian(link = "identity"),
  data = base_final_propose_ratio
)

gvf.propose6 <- glm(
  log_vi_estimacion ~ y_est_inv_sqrt_estimacion + periodicidad + n * y_est_inv_estimacion,
  family = gaussian(link = "identity"),
  data = base_final_propose_ratio
)

gvf.propose7 <- glm(
  log_vi_estimacion ~ atipico + n + estimacion,
  family = gaussian(link = "identity"),
  data = base_final_propose_ratio
)

gvf.propose8 <- glm(
  log_vi_estimacion ~ atipico + n + estimacion + periodicidad,
  family = gaussian(link = "identity"),
  data = base_final_propose_ratio
)

gvf.propose9 <- glm(
  log_vi_estimacion ~ atipico + ind + n + estimacion,
  family = gaussian(link = "identity"),
  data = base_final_propose_ratio
)

# Calcular errores estándar ajustados
v <- gvf.nule$deviance / gvf.nule$df.residual
base_final_propose_ratio$se.nule <- sqrt(exp(v / 2) * exp(gvf.nule$fitted.values) * base_final_propose_ratio$estimacion^2)

v <- gvf.propose1$deviance / gvf.propose1$df.residual
base_final_propose_ratio$se.propose1 <- sqrt(exp(v / 2) * exp(gvf.propose1$fitted.values) * base_final_propose_ratio$estimacion^2)

v <- gvf.propose2$deviance / gvf.propose2$df.residual
base_final_propose_ratio$se.propose2 <- sqrt(exp(v / 2) * exp(gvf.propose2$fitted.values) * base_final_propose_ratio$estimacion^2)

v <- gvf.propose3$deviance / gvf.propose3$df.residual
base_final_propose_ratio$se.propose3 <- sqrt(exp(v / 2) * exp(gvf.propose3$fitted.values) * base_final_propose_ratio$estimacion^2)

v <- gvf.propose4$deviance / gvf.propose4$df.residual
base_final_propose_ratio$se.propose4 <- sqrt(exp(v / 2) * exp(gvf.propose4$fitted.values) * base_final_propose_ratio$estimacion^2)

v <- gvf.propose5$deviance / gvf.propose5$df.residual
base_final_propose_ratio$se.propose5 <- sqrt(exp(v / 2) * exp(gvf.propose5$fitted.values) * base_final_propose_ratio$estimacion^2)

v <- gvf.propose6$deviance / gvf.propose6$df.residual
base_final_propose_ratio$se.propose6 <- sqrt(exp(v / 2) * exp(gvf.propose6$fitted.values) * base_final_propose_ratio$estimacion^2)

v <- gvf.propose7$deviance / gvf.propose7$df.residual
base_final_propose_ratio$se.propose7 <- sqrt(exp(v / 2) * exp(gvf.propose7$fitted.values) * base_final_propose_ratio$estimacion^2)

v <- gvf.propose8$deviance / gvf.propose8$df.residual
base_final_propose_ratio$se.propose8 <- sqrt(exp(v / 2) * exp(gvf.propose8$fitted.values) * base_final_propose_ratio$estimacion^2)

v <- gvf.propose9$deviance / gvf.propose9$df.residual
base_final_propose_ratio$se.propose9 <- sqrt(exp(v / 2) * exp(gvf.propose9$fitted.values) * base_final_propose_ratio$estimacion^2)

# Diagnósticos de los modelos
diag.models <- lapply(
  list(
    gvf.nule, gvf.propose1, gvf.propose2, gvf.propose3,
    gvf.propose4, gvf.propose5, gvf.propose6,
    gvf.propose7, gvf.propose8, gvf.propose9
  ),
  glance
)

diag.models_estimacion <- diag.models %>%
  bind_rows(.id = "model") %>%
  mutate(
    rmse = c(
      sqrt(sum((base_final_propose_ratio$se - base_final_propose_ratio$se.nule)^2)),
      sqrt(sum((base_final_propose_ratio$se - base_final_propose_ratio$se.propose1)^2)),
      sqrt(sum((base_final_propose_ratio$se - base_final_propose_ratio$se.propose2)^2)),
      sqrt(sum((base_final_propose_ratio$se - base_final_propose_ratio$se.propose3)^2)),
      sqrt(sum((base_final_propose_ratio$se - base_final_propose_ratio$se.propose4)^2)),
      sqrt(sum((base_final_propose_ratio$se - base_final_propose_ratio$se.propose5)^2)),
      sqrt(sum((base_final_propose_ratio$se - base_final_propose_ratio$se.propose6)^2)),
      sqrt(sum((base_final_propose_ratio$se - base_final_propose_ratio$se.propose7)^2)),
      sqrt(sum((base_final_propose_ratio$se - base_final_propose_ratio$se.propose8)^2)),
      sqrt(sum((base_final_propose_ratio$se - base_final_propose_ratio$se.propose9)^2))
    ),
    r2_nag = c(
      r2_nagelkerke(gvf.nule),
      r2_nagelkerke(gvf.propose1),
      r2_nagelkerke(gvf.propose2),
      r2_nagelkerke(gvf.propose3),
      r2_nagelkerke(gvf.propose4),
      r2_nagelkerke(gvf.propose5),
      r2_nagelkerke(gvf.propose6),
      r2_nagelkerke(gvf.propose7),
      r2_nagelkerke(gvf.propose8),
      r2_nagelkerke(gvf.propose9)
    )
  )

# Resultados finales
propose_results_estimacion_ratio <- list(
  base_final_propose_ratio = base_final_propose_ratio,
  models.gvf = list(
    nule = gvf.nule,
    propose1 = gvf.propose1,
    propose2 = gvf.propose2,
    propose3 = gvf.propose3,
    propose4 = gvf.propose4,
    propose5 = gvf.propose5,
    propose6 = gvf.propose6,
    propose7 = gvf.propose7,
    propose8 = gvf.propose8,
    propose9 = gvf.propose9
  ),
  models.diag = diag.models_estimacion
)

propose_results_estimacion_ratio$models.diag$model <- c(
  "gvf.nule", "gvf.propose1", "gvf.propose2", "gvf.propose3",
  "gvf.propose4", "gvf.propose5", "gvf.propose6",
  "gvf.propose7", "gvf.propose8", "gvf.propose9"
)

propose_results_estimacion_ratio$models.diag$param <- rep("ratio", 10)

base_final_propose_total <- base_final %>% filter(n >= 30 & se != 0, des == "total") %>%
  mutate(
    vi_estimacion = se^2 / (estimacion^2),
    log_vi_estimacion = log(vi_estimacion),
    y_est_inv_estimacion = estimacion^-1,
    y_est_inv_sqrt_estimacion = estimacion^(-1/2),
    n_inv_estimacion = 1 / n,
    n_y_estimacion = (n * estimacion)^(1/2)
  )

# Modelos desplegados
gvf.nule <- glm(log_vi_estimacion ~ 1, family = gaussian(link = "identity"), data = base_final_propose_total)

gvf.propose1 <- glm(
  log_vi_estimacion ~ y_est_inv_estimacion + y_est_inv_sqrt_estimacion + atipico,
  family = gaussian(link = "identity"),
  data = base_final_propose_total
)

gvf.propose2 <- glm(
  log_vi_estimacion ~ estimacion + n + estimacion:n + atipico,
  family = gaussian(link = "identity"),
  data = base_final_propose_total
)

gvf.propose3 <- glm(
  log_vi_estimacion ~ n + estimacion + n_y_estimacion + atipico,
  family = gaussian(link = "identity"),
  data = base_final_propose_total
)

gvf.propose4 <- glm(
  log_vi_estimacion ~ periodicidad + y_est_inv_estimacion + n,
  family = gaussian(link = "identity"),
  data = base_final_propose_total
)

gvf.propose5 <- glm(
  log_vi_estimacion ~ y_est_inv_estimacion + periodicidad + n_y_estimacion,
  family = gaussian(link = "identity"),
  data = base_final_propose_total
)

gvf.propose6 <- glm(
  log_vi_estimacion ~ y_est_inv_sqrt_estimacion + periodicidad + n * y_est_inv_estimacion,
  family = gaussian(link = "identity"),
  data = base_final_propose_total
)

gvf.propose7 <- glm(
  log_vi_estimacion ~ atipico + n + estimacion,
  family = gaussian(link = "identity"),
  data = base_final_propose_total
)

gvf.propose8 <- glm(
  log_vi_estimacion ~ atipico + n + estimacion + periodicidad,
  family = gaussian(link = "identity"),
  data = base_final_propose_total
)

gvf.propose9 <- glm(
  log_vi_estimacion ~ atipico + ind + n + estimacion,
  family = gaussian(link = "identity"),
  data = base_final_propose_total
)

v <- gvf.nule$deviance / gvf.nule$df.residual
base_final_propose_total$se.nule <- sqrt(exp(v / 2) * exp(gvf.nule$fitted.values) * base_final_propose_total$estimacion^2)

v <- gvf.propose1$deviance / gvf.propose1$df.residual
base_final_propose_total$se.propose1 <- sqrt(exp(v / 2) * exp(gvf.propose1$fitted.values) * base_final_propose_total$estimacion^2)

v <- gvf.propose2$deviance / gvf.propose2$df.residual
base_final_propose_total$se.propose2 <- sqrt(exp(v / 2) * exp(gvf.propose2$fitted.values) * base_final_propose_total$estimacion^2)

v <- gvf.propose3$deviance / gvf.propose3$df.residual
base_final_propose_total$se.propose3 <- sqrt(exp(v / 2) * exp(gvf.propose3$fitted.values) * base_final_propose_total$estimacion^2)

v <- gvf.propose4$deviance / gvf.propose4$df.residual
base_final_propose_total$se.propose4 <- sqrt(exp(v / 2) * exp(gvf.propose4$fitted.values) * base_final_propose_total$estimacion^2)

v <- gvf.propose5$deviance / gvf.propose5$df.residual
base_final_propose_total$se.propose5 <- sqrt(exp(v / 2) * exp(gvf.propose5$fitted.values) * base_final_propose_total$estimacion^2)

v <- gvf.propose6$deviance / gvf.propose6$df.residual
base_final_propose_total$se.propose6 <- sqrt(exp(v / 2) * exp(gvf.propose6$fitted.values) * base_final_propose_total$estimacion^2)

v <- gvf.propose7$deviance / gvf.propose7$df.residual
base_final_propose_total$se.propose7 <- sqrt(exp(v / 2) * exp(gvf.propose7$fitted.values) * base_final_propose_total$estimacion^2)

v <- gvf.propose8$deviance / gvf.propose8$df.residual
base_final_propose_total$se.propose8 <- sqrt(exp(v / 2) * exp(gvf.propose8$fitted.values) * base_final_propose_total$estimacion^2)

v <- gvf.propose9$deviance / gvf.propose9$df.residual
base_final_propose_total$se.propose9 <- sqrt(exp(v / 2) * exp(gvf.propose9$fitted.values) * base_final_propose_total$estimacion^2)

diag.models <- lapply(
  list(
    gvf.nule, gvf.propose1, gvf.propose2, gvf.propose3, gvf.propose4,
    gvf.propose5, gvf.propose6, gvf.propose7, gvf.propose8, gvf.propose9
  ),
  glance
)

diag.models_estimacion <- diag.models %>%
  bind_rows(.id = "model") %>%
  mutate(
    rmse = c(
      sqrt(sum((base_final_propose_total$se - base_final_propose_total$se.nule)^2)),
      sqrt(sum((base_final_propose_total$se - base_final_propose_total$se.propose1)^2)),
      sqrt(sum((base_final_propose_total$se - base_final_propose_total$se.propose2)^2)),
      sqrt(sum((base_final_propose_total$se - base_final_propose_total$se.propose3)^2)),
      sqrt(sum((base_final_propose_total$se - base_final_propose_total$se.propose4)^2)),
      sqrt(sum((base_final_propose_total$se - base_final_propose_total$se.propose5)^2)),
      sqrt(sum((base_final_propose_total$se - base_final_propose_total$se.propose6)^2)),
      sqrt(sum((base_final_propose_total$se - base_final_propose_total$se.propose7)^2)),
      sqrt(sum((base_final_propose_total$se - base_final_propose_total$se.propose8)^2)),
      sqrt(sum((base_final_propose_total$se - base_final_propose_total$se.propose9)^2))
    ),
    r2_nag = c(
      r2_nagelkerke(gvf.nule),
      r2_nagelkerke(gvf.propose1),
      r2_nagelkerke(gvf.propose2),
      r2_nagelkerke(gvf.propose3),
      r2_nagelkerke(gvf.propose4),
      r2_nagelkerke(gvf.propose5),
      r2_nagelkerke(gvf.propose6),
      r2_nagelkerke(gvf.propose7),
      r2_nagelkerke(gvf.propose8),
      r2_nagelkerke(gvf.propose9)
    )
  )

coef.gvf.models_estimacion <- lapply(
  list(
    gvf.nule, gvf.propose1, gvf.propose2, gvf.propose3,
    gvf.propose4, gvf.propose5, gvf.propose6, gvf.propose7,
    gvf.propose8, gvf.propose9
  ),
  coef
)

models.gvf_estimacion <- list(
  nule = gvf.nule, propose1 = gvf.propose1, propose2 = gvf.propose2,
  propose3 = gvf.propose3, propose4 = gvf.propose4, propose5 = gvf.propose5,
  propose6 = gvf.propose6, propose7 = gvf.propose7,
  propose8 = gvf.propose8, propose9 = gvf.propose9
)

propose_results_estimacion_total <- list(
  base_final_propose_total = base_final_propose_total,
  models.gvf = models.gvf_estimacion,
  models.diag = diag.models_estimacion,
  models.coef = coef.gvf.models_estimacion
)

propose_results_estimacion_total$models.diag$model <- c(
  "gvf.nule", "gvf.propose1", "gvf.propose2", "gvf.propose3",
  "gvf.propose4", "gvf.propose5", "gvf.propose6", "gvf.propose7",
  "gvf.propose8", "gvf.propose9"
)

propose_results_estimacion_total$models.diag$param <- rep("total", 10)

resumen_resultados_ajust <- rbind(
  results_estimacion_ratio$models.diag,
  results_estimacion_total$models.diag
) %>%
  dplyr::select(-c(null.deviance, nobs))

resumen_resultados_propose <- rbind(
  propose_results_estimacion_ratio$models.diag,
  propose_results_estimacion_total$models.diag
) %>%
  dplyr::select(-c(null.deviance, nobs))

resumen_resultados_ajust$type <- "teoric"
resumen_resultados_propose$type <- "propose"

resumen_resultados <- rbind(resumen_resultados_ajust, resumen_resultados_propose) %>%
  filter(model %in% c("gvf.propose7", "gvf.propose8", "gvf.propose9", "gvf.krenzke", "gvf.comun", "gvf.propose"))

View(resumen_resultados)
