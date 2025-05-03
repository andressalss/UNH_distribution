# Importando o pacote GAMLSS
library(gamlss)

# Importando as funções
source("UNH_functions.R")

# Expressão da log-verossimilhança da UNH 
loglik_UNH_exp <- expression(
  log(mu*sigma) - log(y) + 
    (mu - 1)*log(1 - sigma*log(y)) + (1 - (1 - sigma*log(y))^mu)
)

## Teste
# sigma = 3
# mu = 0.5
# y = 0.4
# log(d_UNH(y, mu, sigma))

# Derivada em relação a mu
d_mu <- D(loglik_UNH_exp, "mu")

# Derivada em relação a sigma
d_sigma <- D(loglik_UNH_exp, "sigma")

# Derivada mista: primeiro mu, depois sigma
d2_mu_sigma <- D(d_mu, "sigma")

UNH <-function (mu.link = "logit", sigma.link = "identity"){
  
  mstats <- checklink("mu.link", "UNH", substitute(mu.link),
                      c("logit", "probit", "cloglog", "cauchit", "log", "own"))
  dstats <- checklink("sigma.link", "UNH", substitute(sigma.link),
                      c("inverse", "log", "identity", "own"))
  structure(list(family = c("UNH", "Unit-Nadarajah-Haghighi"),
                 parameters = list(mu = TRUE, sigma = TRUE),
                 nopar = 2,
                 type = "Continuous",
                 mu.link = as.character(substitute(mu.link)),
                 sigma.link = as.character(substitute(sigma.link)),
                 mu.linkfun = mstats$linkfun,
                 sigma.linkfun = dstats$linkfun,
                 mu.linkinv = mstats$linkinv,
                 sigma.linkinv = dstats$linkinv,
                 mu.dr = mstats$mu.eta,
                 sigma.dr = dstats$mu.eta,
                 dldm = function(y, mu, sigma) {
                   dldm <- eval(d_mu)
                   dldm
                 },
                 d2ldm2 = function(y,mu, sigma) {
                   dldm <- eval(d_mu)
                   d2ldm2 <- -dldm * dldm
                   d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15)
                   d2ldm2
                 },
                 dldd = function(y, mu, sigma) {
                   dldd <- eval(d_sigma)
                   dldd
                 },
                 d2ldd2 = function(y,mu, sigma) {
                   dldd <- eval(d_sigma)
                   d2ldd2 = -dldd * dldd
                   d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)
                   d2ldd2
                 },
                 d2ldmdd = function(y,mu, sigma) {
                   dldm <- eval(d_mu)
                   dldd <- eval(d_sigma)
                   d2ldmdd = -(dldm * dldd)
                   d2ldmdd<-ifelse(is.na(d2ldmdd)==TRUE,0,d2ldmdd)
                   d2ldmdd
                 },
                 G.dev.incr = function(y, mu, sigma, w, ...) -2 * log(d_UNH(y=y, alpha=mu, lambda=sigma)),
                 rqres = expression(
                   rqres(pfun = "p_UNH", type = "Continuous", y = y, alpha=mu, lambda=sigma)
                 ),
                 mu.initial = expression(mu <- rep(0.5,length(y))),
                 sigma.initial = expression(sigma<- rep(1, length(y))),
                 mu.valid = function(mu) all(mu > 0 & mu < 1),
                 sigma.valid = function(sigma) all(sigma > 0),
                 y.valid = function(y) all(y > 0 & y < 1)
  ),
  class = c("gamlss.family", "family"))
}
#------------------------------------------------------------------------------------------

