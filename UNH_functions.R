################### Distribuição Nadarajah-Haghighi Unitária #################
# Função de densidade de probabilidade ------------------------------------
d_UNH <- function(y, alpha = 1, lambda= 1) {
  term1 <- (alpha * lambda) / y
  term2 <- (1 - lambda * log(y))^(alpha - 1)
  term3 <- exp(1 - (1 - lambda * log(y))^alpha) 
  return(term1 * term2 * term3)
}

## Exemplo de uso
# d_UNH(2,0.4,0.4)
# integrate(d_UNH, 0, 1)

# Função de distribuição acumulada ----------------------------------------
p_UNH <- function(q, alpha = 1, lambda = 1) {  
  return(exp(1 - (1 - lambda * log(q))^alpha))
}

# Exemplo de uso
# p_UNH(1,1,1)


# Função quantílica -------------------------------------------------------
q_UNH <- function(p, alpha, lambda) {
  
  q <- exp((1/lambda)*((1-(1-log(p))^(1/alpha))))
  
  return(q)
  
}
# u = p_UNH(0.2,2,1)
# q_UNH(u, 2, 1)


# Geração de números aleatórios -------------------------------------------
r_UNH <- function(n, alpha, lambda) {
  u <- runif(n)
  return(q_UNH(u, alpha, lambda))
}

# Exemplo de uso
#q_UNH(0.2, 1, 1)

# Exemplo de uso
# set.seed(123)
# amostra <- r_UNH(n = 1000, alpha = 2, lambda = 1)
# hist(amostra, breaks = 30, main = "Amostra da distribuição UNH", xlab = "y")

## Estimação via máxima verossimilhança

# Função da log-verossimilhança da UNH 
loglik_UNH <- function(par, y) {
  alpha <- par[1]
  lambda <- par[2]
  n <- length(y)
  
  
  ll <- n*log(alpha*lambda) - sum(log(y)) + 
    (alpha - 1) * sum(log(1 - lambda*log(y))) +
    n - sum((1- lambda*log(y))^alpha)
  
  
  return(-ll)
}

# Estimação
estim <- function(x) {
  result <- optim(par = c(1,1),
                  fn = loglik_UNH,
                  y = x,
                  method = "SANN")
  
  return(result$par)
}

# Exemplo de uso

# loglik_UNH(c(0.4,3),x)
# sum(log(d_UNH(x, 0.4,3)))

# x <- r_UNH(100, 0.5,.3)
# estim(x)


# Gerar amostra simulada
# set.seed(123)
# n <- 1000
# alpha_true <- 0.8
# lambda_true <- 2.5
# sample_data <- r_UNH(n, alpha_true, lambda_true)