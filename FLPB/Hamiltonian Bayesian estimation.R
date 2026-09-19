library(rstan)
library(ggplot2)
library(lubridate)
Hora <- c("2026-06-06-16-52-55")
set.seed(ymd_hms(Hora)) 
start_time <- Sys.time()
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# tus datos
t <- readRDS("Datos_Dias_prof.RDS")  # tiempos
y <- readRDS("Datos_obs.RDS")  # observaciones

data_list <- list(
  N_obs = length(t),
  t = t,
  y = y,
  y_max = max(y)
)

fit <- stan(
  file = "modelo_funcion_curva_media.stan",
  data = data_list,
  chains = 1,
  iter = 100000
)
end_time <- Sys.time()
Time_HB <- end_time - start_time


print(fit)
saveRDS(fit,file = "Results_Frac.RDS")
saveRDS(Time_HB,file = "Time_HB.RDS")