stan_code <- "
functions {
  real curva_media(real alpha, real N, real b, real t) {
    return N * (1 - exp(-b * t^alpha / alpha));
  }
}

data {
  int<lower=1> N_obs;  // Número de observaciones
  vector[N_obs] t;     // Vector de tiempos
  vector[N_obs] y;     // Datos observados
  real<lower=0> y_max; // Max. de datos
}

parameters {
  real<lower=0, upper=1> alpha;     // Parámetro alpha
  real<lower=y_max> N;         // N
  real<lower=0> b;         // Parámetro b
  real<lower=0> sigma;     // Parámetro sigma (desviación estándar)
}


model {

alpha ~ uniform(0, 1);
real a_GB1 = 1;             // the parameters of the GB1 prior
  real b_GB1 = 1500;
  real p_GB1 = 2;
  real q_GB1 = 2;
  // the GB1 prior as an increment of the log-likelihood
  target += log(a_GB1)
        + (a_GB1 * p_GB1 - 1) * log(N)
        + (q_GB1 - 1) * log1m((N / b_GB1)^a_GB1)
        - a_GB1 * p_GB1 * log(b_GB1)
        - lbeta(p_GB1, q_GB1);
b ~ uniform(0, 2);      
sigma ~ uniform(0, 20);
  
  
  vector[N_obs] mu;
  
  // Calcular la media para cada observación
  for (i in 1:N_obs) {
    mu[i] = curva_media(alpha, N, b, t[i]);
  }
    
  // Modelo de distribución normal para los datos observados
  y ~ normal(mu, sigma);
}"

writeLines(stan_code, "modelo_funcion_curva_media.stan")
