library(rstan)
library(ggplot2)
library(lubridate)
Hora <- c("2026-06-06-16-44-55")
set.seed(ymd_hms(Hora)) 


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# tus datos
t <- readRDS("Datos_Dias_prof.RDS")  # tiempos
y <- readRDS("Datos_obs.RDS")  # observaciones

data_list <- list(
  N_obs = length(t),
  t = t,
  y = y
)

fit <- stan(
  file = "modelo_funcion_curva_media.stan",
  data = data_list,
  chains = 1,
  iter = 100000
)

print(fit)
saveRDS(fit,file = "Results_Clench.RDS")

