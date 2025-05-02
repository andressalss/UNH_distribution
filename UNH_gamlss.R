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

# Testando os resultados
set.seed(10)
n<-1000

# Caso 1: Sem regressor
mu_true<-0.9
sigma_true<-1
mu_result<-sigma_result<-c()
for (i in 1:100) {
  y<-r_UNH(n,mu_true,sigma_true)
  fit1<-gamlss(y~1, family="UNH", trace = F)
  logit_link<-make.link("logit")
  mu_result[i]<-logit_link$linkinv(fit1$mu.coefficients)
  sigma_result[i]<-fit1$sigma.coefficients
}
result1<- matrix(c(mu_true, mean(mu_result),
                   sigma_true, mean(sigma_result)),2,2)
colnames(result1)<-c("mu","sigma")
rownames(result1)<-c("true value","mean")
print(round(result1,2))



# Caso 2: Com Regressor

X<-runif(n)
logit_link<-make.link("logit")
log_link<-make.link("identity")
b1<-.7
b2<-3
mu_true<-logit_link$linkinv(b1+b2*X)
g1<-.5
g2<-1.5
sigma_true<-log_link$linkinv(g1+g2*X)
R<-100
mu_result<-sigma_result<-matrix(NA,R,2)
for (i in 1:R) {
  y<-r_UNH(n,mu_true,sigma_true)
  fit1<-gamlss(y~X,sigma.formula =~ X, family=UNH(), trace = F)
  mu_result[i,]<-fit1$mu.coefficients
  sigma_result[i,]<-fit1$sigma.coefficients
}

true_values<-c(b1,b2, g1,g2)
mean_values<-c(apply(mu_result,2,mean),
               apply(sigma_result,2,mean))
b_values<-(true_values-mean_values)/true_values*100
eqm_values<-c(apply(mu_result,2,var),
              apply(sigma_result,2,var))+(true_values-mean_values)^2
result1<- cbind(true_values,
                mean_values,
                b_values,
                eqm_values
)
colnames(result1)<-c("true value","mean","bias","eqm")
rownames(result1)<-c("b1","b2","g1","g2")
print(round(result1,2))

