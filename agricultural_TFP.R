## This is only code, I have to move on all my data sets stored at other directories,
## once done, I will run the entire code using git datasets:)


library(KFAS)
library(urca)
library(dplyr)
library(readr)
library(ggrepel)
library(ggplot2)
library(lubridate)
library(readxl)
library(tidyr)
library(car)
library(strucchange)  # para efp/sctest (CUSUM)

## =========================================================
## 1. PRODUCCIÓN AGRÍCOLA (datos base y ponderadores)
## =========================================================

prod_ag_est <- read_csv("8d22bea7-25d6-44d2-bea3-53fd4b947024.csv")

df_estado_anio <- prod_ag_est %>%
  group_by(AÑO, CVE_ENT, ENTIDAD) %>%
  summarise(
    SUPERFICIE_SEMBRADA  = sum(SUPERFICIE_SEMBRADA, na.rm = TRUE),
    SUPERFICIE_COSECHADA = sum(SUPERFICIE_COSECHADA, na.rm = TRUE),
    VOLUMEN_PRODUCCION   = sum(VOLUMEN_PRODUCCION, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(RENDIMIENTO = VOLUMEN_PRODUCCION / SUPERFICIE_COSECHADA)

df_pesos <- df_estado_anio %>%
  group_by(AÑO) %>%
  mutate(omega = VOLUMEN_PRODUCCION / sum(VOLUMEN_PRODUCCION, na.rm = TRUE)) %>%
  ungroup()

top10 <- df_estado_anio %>%
  group_by(ENTIDAD) %>%
  summarise(promedio = mean(VOLUMEN_PRODUCCION, na.rm = TRUE)) %>%
  slice_max(promedio, n = 10) %>%
  pull(ENTIDAD)

## =========================================================
## 2. PRECIPITACIÓN Y ÍNDICE CLIMÁTICO NACIONAL
## =========================================================

precip_mens_est <- read_csv("917506fc-a09d-4e81-9d75-5e1f11bf1a14.csv") %>%
  mutate(AÑO = year(as.Date(PERIODO)))

prec_anual <- precip_mens_est %>%
  group_by(CVE_ENT, ENTIDAD, AÑO) %>%
  summarise(PRECIPITACION_ANUAL = sum(PRECIPITACION, na.rm = TRUE), .groups = "drop") %>%
  group_by(CVE_ENT) %>%
  mutate(anom_z = (PRECIPITACION_ANUAL - mean(PRECIPITACION_ANUAL, na.rm = TRUE)) /
           sd(PRECIPITACION_ANUAL, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(CVE_ENT != 0)

df_pesos   <- df_pesos   %>% filter(AÑO >= 1980, AÑO <= 2024)
prec_anual <- prec_anual %>% filter(AÑO >= 1980, AÑO <= 2024)

base_final <- df_pesos %>% left_join(prec_anual, by = c("CVE_ENT", "ENTIDAD", "AÑO"))

indice_climatico <- base_final %>%
  group_by(AÑO) %>%
  summarise(anomalia_nacional = sum(omega * anom_z, na.rm = TRUE))
## =========================================================
## 3. PRODUCTO (Y) — VALOR REAL BASE 2015
## =========================================================

precios_base <- prod_ag_est %>%
  filter(AÑO == 2015) %>%
  group_by(CVE_ENT, CULTIVO) %>%
  summarise(precio_base = sum(VALOR_PRODUCCION, na.rm = TRUE) / sum(VOLUMEN_PRODUCCION, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(precio_base), precio_base > 0)

prod_real <- prod_ag_est %>%
  left_join(precios_base, by = c("CVE_ENT", "CULTIVO")) %>%
  mutate(valor_real = VOLUMEN_PRODUCCION * precio_base)

Y_nacional <- prod_real %>%
  group_by(AÑO) %>%
  summarise(Y = sum(valor_real, na.rm = TRUE), cobertura = sum(!is.na(precio_base)) / n(), .groups = "drop") %>%
  arrange(AÑO)

Y <- Y_nacional %>%
  select(AÑO, Y) %>%
  filter(AÑO >= 1980, AÑO <= 2024) %>%
  rename(Año = AÑO, Valor = Y) %>%
  mutate(serie = "Producto")

## =========================================================
## 4. CAPITAL — PIM / EMPALME FBCF AGRÍCOLA
## =========================================================
tipo_cambio_2015 <- 15.8577

FBCF <- read_csv("FAOSTAT_data_es_5-29-2026.csv") %>%
  mutate(Valor_MXN = Valor * tipo_cambio_2015)

FBCF_Ag <- read_csv("Copia de FAOSTAT_data_es_5-29-2026-2 7.csv") %>%
  mutate(Valor_MXN = Valor * tipo_cambio_2015)

share_ag <- FBCF %>%
  rename(FBCF_total = Valor_MXN) %>%
  inner_join(FBCF_Ag %>% rename(FBCF_ag = Valor_MXN), by = "Año") %>%
  mutate(share = FBCF_ag / FBCF_total)

mod1 <- lm(share ~ Año, data = share_ag)

hist_years <- data.frame(Año = 1980:1994)
hist_years$share_est <- predict(mod1, newdata = hist_years)

FBCF_hist <- FBCF %>%
  filter(Año <= 1994) %>%
  left_join(hist_years, by = "Año") %>%
  mutate(FBCF_ag_est = Valor_MXN * share_est)

FBCF_ag_full <- bind_rows(
  FBCF_hist %>% transmute(Año, Valor_MXN = FBCF_ag_est),
  FBCF_Ag %>% select(Año, Valor_MXN)
) %>%
  arrange(Año)

FBCF_ag_full$serie <- "Capital"

## =========================================================
## 5b. STOCK DE CAPITAL — PERPETUAL INVENTORY METHOD (PIM)
## =========================================================

delta <- 0.06

ventana_g <- FBCF_ag_full %>%
  filter(Año >= min(Año), Año <= min(Año) + 9) %>%
  filter(Valor_MXN > 0)

mod_g <- lm(log(Valor_MXN) ~ Año, data = ventana_g)
g_hat <- coef(mod_g)["Año"]

anio_0 <- min(FBCF_ag_full$Año)
I_0 <- exp(predict(mod_g, newdata = data.frame(Año = anio_0)))
K_0 <- I_0 / (delta + g_hat)

FBCF_ordenado <- FBCF_ag_full %>% arrange(Año) %>% filter(Año >= anio_0)

K_series <- numeric(nrow(FBCF_ordenado))
K_series[1] <- K_0
for (i in 2:nrow(FBCF_ordenado)) {
  K_series[i] <- (1 - delta) * K_series[i - 1] + FBCF_ordenado$Valor_MXN[i]
}

Capital_stock <- FBCF_ordenado %>%
  mutate(Valor = K_series, serie = "Capital") %>%
  select(Año, Valor, serie)

## =========================================================
## 5. TRABAJO, TIERRA, INSUMOS INTERMEDIOS
## =========================================================

labor       <- read_csv("Copia de FAOSTAT_data_es_5-29-2026-2 2.csv")
tierra      <- read_csv("Copia de FAOSTAT_data_es_5-29-2026-2 3.csv")
intermedios <- read_csv("Copia de FAOSTAT_data_es_5-29-2026-2 4.csv")

labor$serie       <- "Trabajo"
tierra$serie      <- "Tierra"
intermedios$serie <- "Intermedios"
Y$serie           <- "Producto"


dap_price_annual <- read_csv("dap_price_annual.csv")

contenido_p2o5_dap <- 0.46  # fracción de P2O5 en el producto DAP físico

precio_p2o5_mxn_2015 <- dap_price_annual %>%
  filter(Año == 2015) %>%
  summarise(p = (DAP_USD_ton / contenido_p2o5_dap) * tipo_cambio_2015) %>%
  pull(p)

intermedios_valor <- intermedios %>%
  mutate(Valor = Valor * precio_p2o5_mxn_2015)  # reemplaza Valor (toneladas) por Valor (pesos 2015)

ggplot(intermedios_valor, aes(x = Año, y = Valor)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Fertilizante (P2O5) valorizado a precios constantes de 2015",
    subtitle = "Precio DAP fijo del año base 2015, aplicado a la cantidad física de cada año",
    x = "Año", y = "Valor (pesos constantes 2015)"
  )

## =========================================================
## 6. CONSOLIDACIÓN E ÍNDICES (base 100)
##    NOTA: se usa intermedios_valor (pesos 2015) en vez de
##    intermedios (toneladas) -- corrección de unidades aplicada.
## =========================================================

AÑO_MIN <- 1991
AÑO_MAX <- 2023

datos <- bind_rows(Capital_stock, labor, tierra, intermedios_valor, Y) %>%
  filter(Año >= AÑO_MIN, Año <= AÑO_MAX) %>%
  group_by(serie) %>%
  arrange(Año) %>%
  mutate(indice = 100 * Valor / first(Valor)) %>%
  ungroup()

## Verificación de cobertura tras el truncado (debe salir 33x5 = 165, sin huecos)
datos %>%
  group_by(serie) %>%
  summarise(min_año = min(Año), max_año = max(Año), n = n())

ggplot(datos, aes(x = Año, y = indice, color = serie)) +
  geom_rect(xmin = 1991.5, xmax = 1992.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = 1993.5, xmax = 1994.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = 2006.5, xmax = 2009.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = 2019.5, xmax = 2020.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3, inherit.aes = FALSE) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(title = "Factores de producción agrícola", subtitle = "Índice base 100 = 1980",
       x = "Año", y = "Índice (base = 100)", color = "")

## =========================================================

## =========================================================

modelo_df <- datos %>%
  select(Año, serie, indice) %>%
  pivot_wider(names_from = serie, values_from = indice) %>%
  arrange(Año) %>%
  left_join(
    indice_climatico %>% rename(Año = AÑO),
    by = "Año"
  ) %>%
  mutate(
    Y = Producto,
    K = Capital,
    T = Tierra,
    L = Trabajo,
    M = Intermedios
  ) %>%
  mutate(
    log_Y = log(Y),
    log_K = log(K),
    log_T = log(T),
    log_L = log(L),
    log_M = log(M)
  ) %>%
  arrange(Año) %>%
  mutate(
    d_log_Y = log_Y - dplyr::lag(log_Y),
    d_log_K = log_K - dplyr::lag(log_K),
    d_log_T = log_T - dplyr::lag(log_T),
    d_log_L = log_L - dplyr::lag(log_L),
    d_log_M = log_M - dplyr::lag(log_M)
  ) %>%
  select(Año, Y, K, T, L, M,
         log_Y, log_K, log_T, log_L, log_M, 
         d_log_Y, d_log_K, d_log_T, d_log_L, d_log_M, 
         anomalia_nacional) %>%
  na.omit()

modelo_df %>%
  select(Año, d_log_Y, d_log_K, d_log_T, d_log_L) %>%
  pivot_longer(
    cols = starts_with("d_log_"),
    names_to = "variable",
    values_to = "crecimiento"
  ) %>%
  ggplot(aes(x = Año, y = crecimiento, color = variable)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(modelo_df$Año), max(modelo_df$Año), by = 2)) +
  labs(title = "Crecimiento logarítmico de las variables", x = "Año", y = "Δ log") +
  theme_minimal() +
  theme(legend.title = element_blank())



# Elasticidades fijadas por convención (growth accounting, enfoque USDA-ERS /
# Fuglie 2010, 2012), ante la ausencia de datos de participación factorial
# específicos para México. Suponen competencia perfecta y rendimientos
# constantes a escala (suman 1). No se estiman por OLS: la regresión simple
# sobre esta serie nacional agregada (T=33) produjo coeficientes no
# significativos y con signo económicamente inconsistente (endogeneidad de
# insumos + error de medición en el stock de capital vía PIM), problema
# documentado en la literatura de estimación de funciones de producción
# (Olley-Pakes 1996; Levinsohn-Petrin 2003; Ackerberg-Caves-Frazer 2015).
s_K <- 0.20
s_L <- 0.35
s_T <- 0.15
s_M <- 0.30


modelo_df <- modelo_df %>%
  mutate(
    A_hat   = log_Y - s_K * log_K - s_L * log_L - s_T * log_T - s_M*M,
    d_A_hat = d_log_Y - s_K * d_log_K - s_L * d_log_L - s_T * d_log_T - s_M*d_log_M
  )

ggplot(modelo_df, aes(x = Año, y = A_hat)) +
  geom_rect(xmin = 1991.5, xmax = 1992.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = 1993.5, xmax = 1994.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = 2006.5, xmax = 2009.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(xmin = 2019.5, xmax = 2020.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3, inherit.aes = FALSE) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  theme_minimal() +
  labs(title = "Residual en niveles (A)", subtitle = "Residuo de contabilidad del crecimiento, elasticidades fijas, sin diferenciar",
       x = "Año", y = "Residual")

A_hat_ts <- ts(modelo_df$A_hat, start = 1991, frequency = 1)

cusum_A_ts <- efp(
  A_hat_ts ~ 1,
  type = "OLS-CUSUM",
  data = modelo_df
)

plot(cusum_A_ts, main = "CUSUM - Estabilidad del residuo (elasticidades fijas)")
sctest(cusum_A_ts)

quiebres <- breakpoints(A_hat_ts ~ 1, data = modelo_df)

quiebres_optimos <- breakpoints(quiebres, breaks = 4)
plot(modelo_df$A_hat, type = "l", main = "Quiebres estructurales en A_hat")
fac_quiebres <- breakfactor(quiebres, breaks = 4)
modelo_segmentado <- lm(A_hat ~ fac_quiebres - 1, data = modelo_df)
lines(fitted(modelo_segmentado), col = "red", lwd = 2)



summary(ur.df(modelo_df$A_hat, type = "trend", selectlags = "AIC"))


ss_model_Ahat <- SSModel(
  A_hat ~
    SSMtrend(degree = 1, Q = list(matrix(NA))) +              # mu_t: permanente (random walk)
    SSMarima(ar = c(NA), Q = matrix(NA), stationary = FALSE), # xi_t: transitorio (AR(1))
  H = matrix(1e-4),
  data = modelo_df
)

var_inicial <- var(modelo_df$A_hat, na.rm = TRUE) / 2

update_fn_Ahat <- function(pars, model) {
  model$Q[1, 1, 1] <- exp(pars[1])
  model$Q[2, 2, 1] <- exp(pars[2])
  model$T[2, 2, 1] <- tanh(pars[3])
  model
}

fit_Ahat <- fitSSM(
  ss_model_Ahat,
  inits = c(log(var_inicial), log(var_inicial), 0),
  updatefn = update_fn_Ahat,
  method = "BFGS"
)
fit_Ahat$optim.out$convergence

kfs_Ahat <- KFS(fit_Ahat$model, smoothing = c("state", "signal"))

modelo_df <- modelo_df %>%
  mutate(
    mu_hat_A = kfs_Ahat$alphahat[, "level"],
    xi_hat_A = kfs_Ahat$alphahat[, "arima1"]
  )

ggplot(modelo_df, aes(x = Año)) +
  geom_line(aes(y = A_hat,    color = "A_hat observado")) +
  geom_line(aes(y = mu_hat_A, color = "Permanente (tendencia)"), linewidth = 0.9) +
  geom_line(aes(y = xi_hat_A, color = "Transitorio (ciclo)"), linewidth = 0.9) +
  theme_minimal() +
  labs(title = "Descomposición de A_hat (niveles): permanente vs. transitorio", x = "Año", y = "A_hat", color = "")

sigma2_nu_A  <- exp(fit_Ahat$optim.out$par[1])
sigma2_eta_A <- exp(fit_Ahat$optim.out$par[2])
cat("sigma^2_nu (permanente):", sigma2_nu_A, " | sigma^2_eta (transitorio):", sigma2_eta_A, "\n")

mu_ts <- ts(modelo_df$mu_hat_A, start = 1991, frequency = 1)

quiebres_mu <- breakpoints(mu_ts ~ 1, data = modelo_df)

cusum_años <- efp(mu_ts ~ 1, type = "OLS-CUSUM")
plot(cusum_años)



modelo_df <- modelo_df %>%
  arrange(Año) %>%
  mutate(d_mu_hat_A = mu_hat_A - dplyr::lag(mu_hat_A))

ggplot(modelo_df, aes(x = Año, y = d_mu_hat_A)) +
  geom_col() +
  geom_vline(xintercept = as.numeric(breakdates(breakpoints(quiebres, breaks = 4))), 
             linetype = "dashed", color = "firebrick") +
  theme_minimal() +
  labs(title = "Incrementos del componente permanente vs. fechas de quiebre en A_hat",
       x = "Año", y = "Δ Permanente")

xi_ts <- ts(modelo_df$xi_hat_A, start = 1991, frequency = 1)
quiebres_xi <- breakpoints(xi_ts ~ 1)

breakdates(breakpoints(quiebres, breaks = 4))     # A_hat original
breakdates(breakpoints(quiebres_mu, breaks = 4))  # componente permanente
breakdates(breakpoints(quiebres_xi, breaks = 4)) 



prod_ag_est %>%
  filter(AÑO >= 1991, AÑO <= 2023) %>%
  left_join(precios_base, by = c("CVE_ENT", "CULTIVO")) %>%
  mutate(valor_real = VOLUMEN_PRODUCCION * precio_base) %>%
  mutate(categoria = case_when(
    grepl("Ma[ií]z|Sorgo|Trigo|Cebada|Avena", CULTIVO, ignore.case = TRUE) ~ "Granos/forrajeros",
    TRUE ~ "Otros"
  )) %>%
  group_by(categoria) %>%
  summarise(valor_total = sum(valor_real, na.rm = TRUE)) %>%
  mutate(participacion = valor_total / sum(valor_total))

prod_ag_est %>%
  filter(AÑO >= 1991, AÑO <= 2023) %>%
  left_join(precios_base, by = c("CVE_ENT", "CULTIVO")) %>%
  mutate(valor_real = VOLUMEN_PRODUCCION * precio_base) %>%
  filter(!grepl("Ma[ií]z|Sorgo|Trigo|Cebada|Avena", CULTIVO, ignore.case = TRUE)) %>%
  group_by(CULTIVO) %>%
  summarise(valor_total = sum(valor_real, na.rm = TRUE)) %>%
  arrange(desc(valor_total)) %>%
  slice_head(n = 20)


prod_ag_est %>%
  filter(AÑO >= 1991, AÑO <= 2023) %>%
  left_join(precios_base, by = c("CVE_ENT", "CULTIVO")) %>%
  mutate(valor_real = VOLUMEN_PRODUCCION * precio_base) %>%
  mutate(categoria = case_when(
    grepl("forraj", CULTIVO, ignore.case = TRUE) ~ "Granos/forrajeros",
    CULTIVO %in% c("Maíz grano", "Sorgo grano", "Trigo grano", "Cebada grano",
                   "Avena grano", "Alfalfa", "Pastos y praderas") ~ "Granos/forrajeros",
    TRUE ~ "Otros"
  )) %>%
  group_by(categoria) %>%
  summarise(valor_total = sum(valor_real, na.rm = TRUE)) %>%
  mutate(participacion = valor_total / sum(valor_total))



