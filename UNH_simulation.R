# Importar funções
source("UNH_functions.R")

# Simulação
set.seed(1234)
nrep <- 100
n <- 100
mu_true <- 0.8
sigma_true <- 2.5


mu_hat <- numeric(nrep)
sigma_hat <- numeric(nrep)

for(i in 1:nrep){
  amostra <- r_UNH(n, mu_true, sigma_true)
  est <- estim(amostra)
  mu_hat[i] <- est[1]
  sigma_hat[i] <- est[2]
}

media_mu <- mean(mu_hat)
media_sigma <- mean(sigma_hat)

# Bias
bias_mu <- media_mu - mu_true
bias_sigma <- media_sigma - sigma_true

# EQM
eqm_mu <- mean((mu_hat - mu_true)^2)
eqm_sigma <- mean((sigma_hat - sigma_true)^2)

# Resultados
resultados <- data.frame(
  Parametro = c("mu", "sigma"),
  Verdadeiro = c(mu_true, sigma_true),
  Estimado = round(c(media_mu, media_sigma),4),
  Vies = round(c(bias_mu, bias_sigma), 4),
  EQM = round(c(eqm_mu, eqm_sigma), 4)
)

resultados

