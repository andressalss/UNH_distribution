# Importar funções
source("UNH_functions.R")

# Simulação
set.seed(1234)
nrep <- 100
n <- 100
alpha_true <- 0.8
lambda_true <- 2.5


alpha_hat <- numeric(nrep)
lambda_hat <- numeric(nrep)

for(i in 1:nrep){
  amostra <- r_NHU(n, alpha_true, lambda_true)
  est <- estim(amostra)
  alpha_hat[i] <- est[1]
  lambda_hat[i] <- est[2]
}

media_alpha <- mean(alpha_hat)
media_lambda <- mean(lambda_hat)

# Bias
bias_alpha <- media_alpha - alpha_true
bias_lambda <- media_lambda - lambda_true

# EQM
eqm_alpha <- mean((alpha_hat - alpha_true)^2)
eqm_lambda <- mean((lambda_hat - lambda_true)^2)

# Resultados
resultados <- data.frame(
  Parametro = c("alpha", "lambda"),
  Verdadeiro = c(alpha_true, lambda_true),
  Estimado = round(c(media_alpha, media_lambda),4),
  Vies = round(c(bias_alpha, bias_lambda), 4),
  EQM = round(c(eqm_alpha, eqm_lambda), 4)
)

resultados

