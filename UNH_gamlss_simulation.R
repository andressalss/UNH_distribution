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