library(rstan)
library(ggplot2)
library(mnormt)
library(HDInterval)
library(actuar)

# datos
t <- readRDS("Datos_Dias_prof.RDS")  # tiempos
y <- readRDS("Datos_obs.RDS")  # observaciones
fit <- readRDS(file = "Results_Frac.RDS")


#Tamaños
alpha_draws <- extract(fit,inc_warmup = FALSE)$alpha
N_draws <- extract(fit,inc_warmup = FALSE)$N
b_draws <- extract(fit,inc_warmup = FALSE)$b
sigma_draws <- extract(fit,inc_warmup = FALSE)$sigma
length(alpha_draws)
check_hmc_diagnostics(fit)
summary(fit)$summary[, c("Rhat", "n_eff")]

#Tiempo
Time_HB <- readRDS(file = "Time_HB.RDS")

################################################################################
######################    TRACEPLOTS             ###############################
################################################################################

#traceplot(fit, pars = c("alpha"), inc_warmup = TRUE)
traceplot(fit, pars = c("alpha"), inc_warmup = FALSE) +
  ylab(expression(alpha))  + scale_color_manual(values = rep("black", fit@sim$chains)) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),       # tamaño de los títulos de eje
    axis.text = element_text(size = 20),        # tamaño de los números en los ejes
    plot.title = element_text(size = 20, face = "bold")  # tamaño del título si lo agregas
  ) +
  scale_x_continuous(
    breaks = c(0, 50000, 75000 ,100000),
    labels = c("0", "50k","75k" ,"100k")
  )

#traceplot(fit, pars = c("N"), inc_warmup = TRUE)
traceplot(fit, pars = c("N"), inc_warmup = FALSE) +
  ylab("M")  + scale_color_manual(values = rep("black", fit@sim$chains)) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),       # tamaño de los títulos de eje
    axis.text = element_text(size = 20),        # tamaño de los números en los ejes
    plot.title = element_text(size = 20, face = "bold")  # tamaño del título si lo agregas
  ) +
  scale_x_continuous(
    breaks = c(0, 50000, 75000 ,100000),
    labels = c("0", "50k","75k" ,"100k")
  )

#traceplot(fit, pars = c("b"), inc_warmup = TRUE)
traceplot(fit, pars = c("b"), inc_warmup = FALSE) +
  ylab("b")  + scale_color_manual(values = rep("black", fit@sim$chains)) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),       # tamaño de los títulos de eje
    axis.text = element_text(size = 20),        # tamaño de los números en los ejes
    plot.title = element_text(size = 20, face = "bold")  # tamaño del título si lo agregas
  ) +
  scale_x_continuous(
    breaks = c(0, 50000, 75000 ,100000),
    labels = c("0", "50k","75k" ,"100k")
  )

traceplot(fit, pars = c("sigma"), inc_warmup = TRUE)
traceplot(fit, pars = c("sigma"), inc_warmup = FALSE) +
  ylab(expression(sigma))




################################################################################
######################    HISTOGRAMS             ###############################
################################################################################
alpha_draws_df <- data.frame(list(alpha=alpha_draws))
plotposte <- ggplot(alpha_draws_df, aes(x=alpha)) + xlab(expression(alpha)) +
geom_histogram(
  aes(y = after_stat(density)),
  bins=20, color="gray") + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),       # tamaño de los títulos de eje
    axis.text = element_text(size = 18),        # tamaño de los números en los ejes
    plot.title = element_text(size = 20, face = "bold")  # tamaño del título si lo agregas
  ) +
  stat_function(
    fun = function(x) dunif(x, min = 0, max = 1),
    linewidth = 1.4,
    linetype = "longdash"
  ) +
  labs(
    x = expression(alpha),
    y = "Density"
  ) 
plotposte

N_draws_df <- data.frame(list(N=N_draws))
plotposte <- ggplot(N_draws_df, aes(x=N)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins=30, color="gray") + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),       # tamaño de los títulos de eje
    axis.text = element_text(size = 18),        # tamaño de los números en los ejes
    plot.title = element_text(size = 20, face = "bold")  # tamaño del título si lo agregas
  ) +
  coord_cartesian(xlim = c(0, 1500)) +
  stat_function(
    fun = function(x) dgenbeta(
      x,
      shape1 = 2,    # p
      shape2 = 2,    # q
      shape3 = 1,    # a
      scale  = 1500  # b
    ),
    linewidth = 1.4,
    linetype = "longdash",
    xlim = c(88, 1500)
  ) +
  labs(
    x = "M",
    y = "Density"
  ) 
plotposte


b_draws_df <- data.frame(list(b=b_draws))
plotposte <- ggplot(b_draws_df, aes(x=b)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins=20, color="gray") + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),       # tamaño de los títulos de eje
    axis.text = element_text(size = 18),        # tamaño de los números en los ejes
    plot.title = element_text(size = 20, face = "bold")  # tamaño del título si lo agregas
  ) +
  stat_function(
    fun = function(x) dunif(x, min = 0, max = 2),
    linewidth = 1.4,
    linetype = "longdash"
  ) +
  labs(
    x = "b",
    y = "Density"
  ) 
plotposte


sigma_draws_df <- data.frame(list(sigma=sigma_draws))
plotposte <- ggplot(sigma_draws_df, aes(x=sigma)) +
  geom_histogram(bins=20, color="gray")
plotposte



################################################################################
######################    Intervals              ###############################
################################################################################
# Calculating posterior intervals
quantile(alpha_draws, probs=c(0.025, 0.975))
quantile(N_draws, probs=c(0.025, 0.975))
quantile(b_draws, probs=c(0.025, 0.975))
quantile(sigma_draws, probs=c(0.025, 0.975))


hdi(alpha_draws, prob = 0.95)
hdi(N_draws, prob = 0.95)
hdi(b_draws, prob = 0.95)

################################################################################
######################      Median and  MAP            #########################
################################################################################
alpha_median <- median(alpha_draws)
N_median <- median(N_draws)
b_median <- median(b_draws)
sigma_median <- median(sigma_draws)


alpha_MAP <- alpha_draws[which.max(extract(fit,inc_warmup = FALSE)$lp__)]
N_MAP <- N_draws[which.max(extract(fit,inc_warmup = FALSE)$lp__)]
b_MAP <- b_draws[which.max(extract(fit,inc_warmup = FALSE)$lp__)]
sigma_MAP <- sigma_draws[which.max(extract(fit,inc_warmup = FALSE)$lp__)]


################################################################################
######################            ACF                  #########################
################################################################################

acf(alpha_draws,main = expression(alpha))
acf(N_draws,main = "N")
acf(b_draws,main = "b")
acf(sigma_draws,main = expression(sigma))


################################################################################
######################            Graphs                  ######################
################################################################################

ggplot(data.frame(x = c(0, 1500)), aes(x = x)) +
  stat_function(
    fun = function(x) dgenbeta(
      x,
      shape1 = 2,    # p
      shape2 = 2,    # q
      shape3 = 1,    # a
      scale  = 1500  # b
    ),
    linewidth = 1.2
  ) +
  labs(
    x = "M",
    y = "Prior density",
    title = "GB1 prior for N"
  ) +
  theme_bw()


################################################################################
######################    joint posterior distributions             ############
################################################################################

post_df <- as.data.frame(as.matrix(fit, pars = c("alpha", "b", "N")))
post_df_MAP <- data.frame(
  alpha = alpha_MAP,
  N = N_MAP,
  b = b_MAP
)
p1 <- ggplot(post_df, aes(x = alpha, y = b)) +
  geom_point(color = "gray40", alpha = 0.15, size = 0.5) +
  geom_point(
    data = post_df_MAP,
    aes(x = alpha, y = b),
    color = "black",
    shape = 4,
    size = 4,
    stroke = 1.5
  ) +
  labs(x = expression(alpha), y = "b") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15)
  )

p2 <- ggplot(post_df, aes(x = alpha, y = N)) +
  geom_point(color = "gray40", alpha = 0.15, size = 0.5) +
  geom_point(
    data = post_df_MAP,
    aes(x = alpha, y = N),
    color = "black",
    shape = 4,
    size = 4,
    stroke = 1.5
  ) +
  labs(x = expression(alpha), y = "M") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15)
  )

p3 <- ggplot(post_df, aes(x = b, y = N)) +
  geom_point(color = "gray40", alpha = 0.15, size = 0.5) +
  geom_point(
    data = post_df_MAP,
    aes(x = b, y = N),
    color = "black",
    shape = 4,
    size = 4,
    stroke = 1.5
  ) +
  labs(x = "b", y = "M") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15)
  )
p1
p2
p3
