
################### Distribuição Nadarajah-Haghighi Unitária #################

# Função de densidade de probabilidade ------------------------------------
d_UNH <- function(y, alpha, lambda) {
  term1 <- (alpha * lambda) / y
  term2 <- (1 - lambda * log(y))^(alpha - 1)
  term3 <- exp(1 - (1 - lambda * log(y))^alpha) 
  return(term1 * term2 * term3)
}

## Exemplo de uso
# d_UNH(2,0.4,0.4)
# integrate(d_UNH, 0, 1)

# Função de distribuição acumulada ----------------------------------------
p_UNH <- function(y, alpha, lambda) {  
  return(1 - exp(1 - (1 - lambda * log(y))^alpha))
}

# Exemplo de uso
# p_UNH(2,0.4,0.4)


# Função quantílica -------------------------------------------------------
q_UNH <- function(p, alpha, lambda) {
  return(exp((1 - (-log(1 - p))^(1 / alpha)) / lambda))
}
# Exemplo de uso
# q_UNH(1,0.4,0.4)


# Geração de números aleatórios -------------------------------------------
r_UNH <- function(n, alpha, lambda) {
  u <- runif(n)
  return(q_UNH(u, alpha, lambda))
}

# Exemplo de uso
# set.seed(123)
# amostra <- r_UNH(n = 1000, alpha = 2, lambda = 1)
# hist(amostra, breaks = 30, main = "Amostra da distribuição UNH", xlab = "y")

## Estimação via máxima verossimilhança

# Expressão da log-verossimilhança da UNH 
loglik_UNH_exp <- expression(
  log(alpha) + log(lambda) - log(y) +
  (alpha - 1) * log(1 - lambda * log(y)) +
  (1 - (1 - lambda * log(y))^alpha)
)

# Derivada em relação a alpha
d_alpha <- D(loglik_UNH_exp, "alpha")

# Derivada em relação a lambda
d_lambda <- D(loglik_UNH_exp, "lambda")

# Derivada mista: primeiro alpha, depois lambda
d2_alpha_lambda <- D(d_alpha, "lambda")

# Visualizar
# d_alpha
# d_lambda
# d2_alpha_lambda

# Função da log-verossimilhança da UNH 
loglik_UNH <- function(par, x) {
  alpha <- par[1]
  lambda <- par[2]
  y <- x
  
  # Verificações de validade
  if (any(is.na(y)) || any(y <= 0) || is.na(alpha) || is.na(lambda) ||
      alpha <= 0 || lambda <= 0 || !is.finite(alpha) || !is.finite(lambda)) {
    return(Inf)
  }
  
  inside_log <- 1 - lambda * log(y)
  
  if (any(is.na(inside_log)) || any(!is.finite(inside_log)) || any(inside_log <= 0)) {
    return(Inf)
  }
  
  ll <- sum(
    log(alpha) + log(lambda) - log(y) +
      (alpha - 1) * log(inside_log) +
      (1 - inside_log^alpha)
  )
  
  return(-ll)
}

# Estimação
estim <- function(x) {
                  result <- optim(par = c(1,1),
                  fn = loglik_UNH,
                  x = x,
                  method = "SANN")
  
  return(result$par)
}

# Exemplo de uso
# estim(x)





