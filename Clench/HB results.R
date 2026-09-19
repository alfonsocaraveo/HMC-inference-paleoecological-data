library(rstan)
library(ggplot2)
library(mnormt)
library(HDInterval)
# datos
t <- readRDS("Datos_Dias_prof.RDS")  # tiempos
y <- readRDS("Datos_obs.RDS")  # observaciones
fit <- readRDS(file = "Results_Clench.RDS")


#Tamaños
N_draws <- extract(fit,inc_warmup = FALSE)$N
b_draws <- extract(fit,inc_warmup = FALSE)$b
sigma_draws <- extract(fit,inc_warmup = FALSE)$sigma
summary(fit)$summary[, c("Rhat", "n_eff")]



################################################################################
######################    TRACEPLOTS             ###############################
################################################################################


traceplot(fit, pars = c("N"), inc_warmup = TRUE)
traceplot(fit, pars = c("N"), inc_warmup = FALSE)

traceplot(fit, pars = c("b"), inc_warmup = TRUE)
traceplot(fit, pars = c("b"), inc_warmup = FALSE)

traceplot(fit, pars = c("sigma"), inc_warmup = TRUE)
traceplot(fit, pars = c("sigma"), inc_warmup = FALSE)



################################################################################
######################    HISTOGRAMS             ###############################
################################################################################
N_draws_df <- data.frame(list(N=N_draws))
plotposte <- ggplot(N_draws_df, aes(x=N)) +
  geom_histogram(bins=50, color="gray")
plotposte


b_draws_df <- data.frame(list(b=b_draws))
plotposte <- ggplot(b_draws_df, aes(x=b)) +
  geom_histogram(bins=20, color="gray")
plotposte


sigma_draws_df <- data.frame(list(sigma=sigma_draws))
plotposte <- ggplot(sigma_draws_df, aes(x=sigma)) +
  geom_histogram(bins=20, color="gray")
plotposte


################################################################################
######################    Intervals              ###############################
################################################################################
# Calculating posterior intervals
quantile(N_draws, probs=c(0.025, 0.975))
quantile(b_draws, probs=c(0.025, 0.975))
quantile(sigma_draws, probs=c(0.025, 0.975))



hdi(N_draws, prob = 0.95)
hdi(b_draws, prob = 0.95)

################################################################################
######################      Median and  MAP            #########################
################################################################################
N_median <- median(N_draws)
b_median <- median(b_draws)
sigma_median <- median(sigma_draws)


N_MAP <- N_draws[which.max(extract(fit,inc_warmup = FALSE)$lp__)]
b_MAP <- b_draws[which.max(extract(fit,inc_warmup = FALSE)$lp__)]
sigma_MAP <- sigma_draws[which.max(extract(fit,inc_warmup = FALSE)$lp__)]


################################################################################
######################            ACF                  #########################
################################################################################


acf(N_draws,main = "N")
acf(b_draws,main = "b")
acf(sigma_draws,main = expression(sigma))


