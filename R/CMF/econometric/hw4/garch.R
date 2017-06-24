library("memisc")
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
#library("lmtest") # тесты для линейных моделей
#library("glmnet") # LASSO + ridge
library("ggplot2") # графики
#library("car") # 
library("knitr")# rmd files
#library("devtools")
#library("lubridate") # работа с датами
library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("forecast")
library("pander")
library("copula")
library("quantmod") # загрузка с finance.google.com
#library("rusquant") # загрузка с finam.ru
#library("sophisthse") # загрузка с sophist.hse.ru
library("Quandl") # загрузка с Quandl
#library("datasets")
library("nortest")#тесты на нормальность
library("ghyp")#общее-гиперболическое распределение
library("FinTS")
library("fGarch")
library("broom")
library("tseries")

#для того, чтобы временные ряды хорошо скачивались в Росиии
Sys.setlocale("LC_TIME","C")


getSymbols(Symbols="JPM", from="2012-01-01",to="2015-11-01", scr="google")
getSymbols(Symbols="TOYOF", from="2012-01-01",to="2015-11-01", scr="google")
getSymbols(Symbols="PG", from="2012-01-01",to="2015-11-1=01", scr="google")
getSymbols(Symbols="DIS", from="2012-01-01",to="2015-11-01", scr="google")

JP<-JPM$JPM.Open
TOY<-TOYOF$TOYOF.Open
PG<-PG$PG.Open
DIS<-DIS$DIS.Open
glimpse(JP)
glimpse(TOY)
glimpse(PG)
glimpse(DIS)

Y_JP<-(JP-lag(JP,1))/lag(JP,1)
Y_TOY<-(TOY-lag(TOY,1))/lag(TOY,1)
Y_PG<-(PG-lag(PG,1))/lag(PG,1)
Y_DIS<-(DIS-lag(DIS,1))/lag(DIS,1)

Y_all<-cbind(Y_JP, Y_TOY,Y_PG, Y_DIS)
autoplot(Y_all)
autoplot(Y_all, facet=NULL)
Y<-Y_all[700:963]
autoplot(Y, facet=NULL)
autoplot(Y)

JP<-as.vector(na.omit(Y_JP))
TOY<-as.vector(na.omit(Y_TOY))
PG<-as.vector(na.omit(Y_PG))
DIS<-as.vector(na.omit(Y_DIS))

#проведём LM тест на наличие зависимость ARMA 
ArchTest(JP,lags=10)
ArchTest(TOY,lags=10)
ArchTest(PG,lags=10)
ArchTest(DIS,lags=10)
#везде есть арма эффект

tsdisplay(JP)
mod_JP<-auto.arima(JP)
summary(mod_JP)
#получаем AR(1) - процесс


JP_fit_norm<-garchFit(formula=~arma(1,0)+aparch(1,1), data=JP, 
                 cond.dist="norm", include.delta=F, leverage=F, trace=F)
JP_fit_norm
JP_fit_snorm<-garchFit(formula=~arma(1,0)+aparch(1,1), data=JP, 
                  cond.dist="snorm", include.delta=F, leverage=F, trace=F)
JP_fit_snorm
JP_fit_std<-garchFit(formula=~arma(1,0)+aparch(1,1), data=JP, 
                       cond.dist="std", include.delta=F, leverage=F, trace=F)
JP_fit_std

JP_fit_sged<-garchFit(formula=~arma(1,0)+aparch(1,1), data=JP, 
                      cond.dist="sged",shape=1.25,include.shape=T,
                      include.delta=F, leverage=F, trace=F)
JP_fit_sged

JP_fit_norm$Log.likelihood
JP_fit_snorm
JP_fit_sstd
JP_fit_sged

#sNorm хорош по правдоподобию
# можно ещё попробовать другую модель t-Garch

JP_fit_T<-garchFit(formula=~arma(1,0)+aparch(1,1), data=JP, 
                     cond.dist="snorm", delta=2, leverage=T, trace=F)
JP_fit_T
#плохо

#графическая оценка одели
plot(JP_fit_snorm, which=10)
TT<-length(JP)
V_snorm<-rsnorm(100*TT, mean=mean(JP), sd = sd(JP))
qqplot(V_snorm, JP)
abline(0,1)
#всё совсем плохо, хвосты ужасны

#пробуем со стьюдентом
V_std<-rstd(100*TT, mean=mean(JP), sd = sd(JP))
qqplot(V_std, JP)
abline(0,1)
#получше

#просто ged тоже плохо работает
V_ged<-rged(1000*TT, mean=mean(JP), sd = sd(JP), nu=1.3)
qqplot(V_ged, JP)
abline(0,1)

#как результат будем пользоваться стьюдентом а потом ged

#проверяем на стационарность
# ADF-тест
adf.test(JP)
adf.test(TOY)
adf.test(PG)
adf.test(DIS)

# PP-тест
pp.test(JP)

# KPSS-тест
kpss.test(JP, null="Level")
#тоже стац
kpss.test(TOY, null="Level")
kpss.test(PG, null="Level")
kpss.test(DIS, null="Level")
#тест не похходит, так как он асимптотический, а у нас маловато данных


# прогноз среднего и дисперсии на i шагов вперёд
JP_frc <- predict(JP_fit_std,n.ahead=i)
JP_frc[,1] # вектор средних
JP_frc[,3]^2 # вектор дисперсий

alpha <- 0.05
VaR_JP <- JP_frc[1,1]+JP_frc[1,3]*qstd(alpha,mean=0,sd=1)
VaR_JP


#кривая
TT<-length(JP)
VaR_JP<-numeric()
VaR_JP_2<-numeric()
T2<-250
T1<-TT-T2
h<-130
alpha_10=0.1
alpha_5=0.05

for (i in (T1+1):(T1+T2)) {
  h.JP <- JP[(i-h):(i-1)]
  JP.fit <- garchFit(formula=~arma(1,0)+aparch(1,1),data=h.JP,
                       delta=2,include.delta=FALSE,leverage=F,cond.dist="std",
                       include.shape=FALSE,trace=FALSE)
  JP.frc <- predict(JP.fit, n.ahead=1)
  VaR_JP[i-T1] <- JP.frc[1,1]+JP.frc[1,3]*qstd(alpha_5,mean=0,sd=1)
  VaR_JP_2[i-T1] <- JP.frc[1,1]+JP.frc[1,3]*qstd(alpha_10,mean=0,sd=1)
}   

fact_JP<-JP[(T1+1):(T1+T2)]
plot(fact_JP, type="l")
lines(VaR_JP, col="red")
lines(VaR_JP_2, col="blue")
#более менее

K <- sum(fact_JP<VaR_JP)
alpha0 <- K/T2
S <- -2*log((1-alpha_5)^(T2-K)*alpha_5^K)+
  2*log((1-alpha0)^(T2-K)*alpha0^K)
p.value <- 1-pchisq(S,df=1)
alpha_5
alpha0
p.value
#тест не прошли


K <- sum(fact_JP<VaR_JP_2)
alpha0 <- K/T2
Z <- -2*log((1-alpha_10)^(T2-K)*alpha_10^K)+
  2*log((1-alpha0)^(T2-K)*alpha0^K)
p.value <- 1-pchisq(Z,df=1)
alpha_10
alpha0
p.value
# а тут прошли

#все тоже самоё используя GED

TT<-length(JP)
VaR_JP<-numeric()
VaR_JP_2<-numeric()
T2<-250
T1<-TT-T2
h<-150
alpha_10=0.1
alpha_5=0.05

for (i in (T1+1):(T1+T2)) {
  h.JP <- JP[(i-h):(i-1)]
  JP.fit <- garchFit(formula=~arma(1,0)+aparch(1,1),data=h.JP,
                     delta=2,include.delta=FALSE,leverage=T,cond.dist="sged",
                     include.shape=T,shape=1.25,trace=FALSE)
  JP.frc <- predict(JP.fit, n.ahead=1)
  VaR_JP[i-T1] <- JP.frc[1,1]+JP.frc[1,3]*qstd(alpha_5,mean=0,sd=1)
  VaR_JP_2[i-T1] <- JP.frc[1,1]+JP.frc[1,3]*qstd(alpha_10,mean=0,sd=1)
}   

fact_JP<-JP[(T1+1):(T1+T2)]
plot(fact_JP, type="l")
lines(VaR_JP, col="red")
lines(VaR_JP_2, col="blue")          
#хорошо

тест Купика
K <- sum(fact_JP<VaR_JP_2)
alpha0 <- K/T2
Z <- -2*log((1-alpha_10)^(T2-K)*alpha_10^K)+
  2*log((1-alpha0)^(T2-K)*alpha0^K)
p.value <- 1-pchisq(Z,df=1)
alpha_10
alpha0
p.value
#есс

K <- sum(fact_JP<VaR_JP)
alpha0 <- K/T2
Z <- -2*log((1-alpha_5)^(T2-K)*alpha_5^K)+
  2*log((1-alpha0)^(T2-K)*alpha0^K)
p.value <- 1-pchisq(Z,df=1)
alpha_5
alpha0
p.value
#есс

# функции потерь в 2-ух случаях
L.Lo <- sum((fact_JP-VaR_JP)^2*(fact_JP<VaR_JP))/K
L.BI <- sum((fact_JP-VaR_JP)/VaR_JP*(fact_JP<VaR_JP))/K
L.Lo*10^4
L.BI
#итог: ged справился лучше, обе функции потерь меньше


#модель copula-garch
JP.gfit <- garchFit(data=JP,formula=~garch(1,1),
                     shape=1.25,include.shape=F,cond.dist="ged",trace=F)
TOY.gfit <- garchFit(data=TOY,formula=~garch(1,1),
                     shape=1.25,include.shape=F,cond.dist="sged",trace=F)
TT<-length(JP)
VV<-rged(TT*1000,  mean=mean(JP), sd = sd(JP), nu=1.5)

qqplot(VV, JP)
abline(0,1)

ZZ<-rsged(TT*1000,  mean=mean(TOY), sd = sd(TOY), nu=2)
qqplot(ZZ, TOY)
abline(0,1)
# ged совсем не подходит

CC<-rstd(TT*100,  mean=mean(TOY), sd = sd(TOY))
qqplot(CC, TOY)
abline(0,1)

# смотрим на остатки z_t
z <- matrix(nrow=,ncol=2)
z[,1] <- JP.gfit@residuals / JP.gfit@sigma.t
z[,2] <- TOY.gfit@residuals / TOY.gfit@sigma.t

mean_1<-c(0,0)
sd_1<-c(1,1)
nu_1<-c(1.25,1.25 )
xi_1<-c(1, TOY.gfit@fit$par["skew"])

cdf_1<-matrix(nrow=TT, ncol=2)
for(i in 1:2){
  cdf_1[,i]<-psged(z[,i], mean = mean_1[i], sd=sd_1[i], 
                        nu=nu_1[i], xi=xi_1[i])
}

norm.cop <- normalCopula(dim=2,param=0.5,dispstr="un")
stud.cop <- tCopula(dim=2,param=0.5,df=5,df.fixed=TRUE,dispstr="un")
gumb.cop <- gumbelCopula(dim=2,param=2)
clay.cop <- claytonCopula(dim=2,param=2)

norm.fit <- fitCopula(cdf_1,copula=norm.cop)
stud.fit <- fitCopula(cdf_1,copula=stud.cop)
gumb.fit <- fitCopula(cdf_1,copula=gumb.cop)
clay.fit <- fitCopula(cdf_1,copula=clay.cop)

N<-1000
cdf_1.sim <- rCopula(n=N,copula=stud.fit@copula)
z.sim <- matrix(nrow=N,ncol=2)
for (i in 1:2) z.sim[,i] <- qsged(cdf_1.sim[,i],
                                  mean=mean_1[i],sd=sd_1[i],nu=nu_1[i],xi=xi_1[i])
frc1 <- predict(JP.gfit,n.ahead=1)
frc2 <- predict(TOY.gfit,n.ahead=1)
mu <- c(frc1[,1],frc2[,1])
sigma <- c(frc1[,3],frc2[,3])

W<-c(0.5, 0.5)
prt.sim <- w[1]*(mu[1]+sigma[1]*z.sim[,1]) +
  w[2]*(mu[2]+sigma[2]*z.sim[,2])

prt.sim <- sort(prt.sim)
VaR <- prt.sim[alpha*N]
ES <- mean(prt.sim[1:(alpha*N-1)])


#кривая var
TT<-length(JP)
VAR_5<-numeric()
VAR_10<-numeric()
T2<-250
T1<-TT-T2
h<-130
alpha_10=0.1
alpha_5=0.05

W<-c(0.5, 0.5)
N<-1000

for (i in (T1+1):(T1+T2)) {
  h.JP <- JP[(i-h):(i-1)]
  h.TOY <- TOY[(i-h):(i-1)]
  JP.gfit <- garchFit(data=h.JP, shape=1.25,formula=~garch(1,1),include.shape=F,
                      cond.dist="ged",trace=F)
  TOY.gfit <- garchFit(data=h.TOY,formula=~garch(1,1),
                       shape=1.25,include.shape=F,cond.dist="sged",trace=F)
  
  z <- matrix(nrow=h,ncol=2)
  z[,1] <- JP.gfit@residuals / JP.gfit@sigma.t
  z[,2] <- TOY.gfit@residuals / TOY.gfit@sigma.t
  
  mean_1<-c(0,0)
  sd_1<-c(1,1)
  nu_1<-c(1.25,1.25 )
  xi_1<-c(1, TOY.gfit@fit$par["skew"])
  
  cdf_1<-matrix(nrow=h, ncol=2)
  for(j in 1:2){
    cdf_1[,j]<-psged(z[,j], mean = mean_1[j], sd=sd_1[j], 
                     nu=nu_1[j], xi=xi_1[j])
  }
  norm.fit <- fitCopula(cdf_1,copula=norm.cop)
  
  cdf_1.sim <- rCopula(n=N,copula=stud.fit@copula)
  z.sim <- matrix(nrow=N,ncol=2)
  for (j in 1:2) z.sim[,j] <- qsged(cdf_1.sim[,j],
                                    mean=mean_1[j],sd=sd_1[j],nu=nu_1[j],
                                    xi=xi_1[j])
  frc1 <- predict(JP.gfit,n.ahead=1)
  frc2 <- predict(TOY.gfit,n.ahead=1)
  mu <- c(frc1[,1],frc2[,1])
  sigma <- c(frc1[,3],frc2[,3])
  
  prt.sim <- w[1]*(mu[1]+sigma[1]*z.sim[,1]) +
    w[2]*(mu[2]+sigma[2]*z.sim[,2])
  
  prt.sim <- sort(prt.sim)
  VAR_5[i-T1] <- prt.sim[alpha_5*N]
  VAR_10[i-T1] <- prt.sim[alpha_10*N]
  #VaR_JP[i-T1] <- JP.frc[1,1]+JP.frc[1,3]*qstd(alpha_5,mean=0,sd=1)
 # VaR_JP_2[i-T1] <- JP.frc[1,1]+JP.frc[1,3]*qstd(alpha_10,mean=0,sd=1)
}   

fact<-JP[(T1+1):(T1+T2)]*w[1]+TOY[(T1+1):(T1+T2)]*w[2]
plot(fact, type="l")
lines(VAR_5, col="red")
lines(VAR_10, col="blue")

