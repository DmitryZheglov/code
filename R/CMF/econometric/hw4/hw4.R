# исходные данные
library(datasets)
datMTLR= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/ecmetr/hw4/MTLR.csv",header=TRUE,sep=",")
datYNDX= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/ecmetr/hw4/RASP.csv",header=TRUE,sep=",")

dax=unlist((datYNDX[8]-datYNDX[5])/datYNDX[5],use.names = FALSE)
smi=unlist((datMTLR[8]-datMTLR[5])/datMTLR[5],use.names = FALSE)

T <- length(dax)
# LM-тест
library(FinTS)
ArchTest(dax,lags=12)
#большой пивалью значит есть арч эффекты,значит модель будет неточна
library(fGarch)
#общий вид модели
#dax.gfit=garchFit(formula=~arma(m,n)+aparch(p,q),data=dax,cond.dist="norm",include.delta=T/F,leverage=T/F,trace=FALSE)
#выберем одну из них,зафитим параметры и построим прогноз
#garche(1,1)
garchFit(formula=~aparch(1,1),data=dax,delta=2,include.delta=FALSE,leverage=FALSE,trace=FALSE)
#TS-GARCHE(1,1)
garchFit(formula=~aparch(1,1),data=dax,delta=1,include.delta=FALSE,leverage=FALSE,trace=FALSE)
#T-GARCH(1,1)
garchFit(formula=~aparch(1,1),data=dax,delta=2,include.delta=FALSE,leverage=TRUE,trace=FALSE)
#гРАФИЧЕСКИЙ АНАЛИЗ МОДЕЛИ
dax.gfit <- garchFit(formula=~aparch(1,1),data=dax,delta=2,
                     include.delta=FALSE,leverage=TRUE,cond.dist="sged",
                     shape=1.25,include.shape=FALSE,trace=FALSE)

plot(dax.gfit,which=13)
plot(dax.gfit,which=10)
#qqplot не изменяется нет особой разницы между моделями residuals не изменяются




library(tseries)
# ADF-тест(алтернатива=стационарность)
adf.test(dax)
# PP-тест
pp.test(dax)
# KPSS-тест
kpss.test(dax, null="Level")
#видим что тесты показывают большой пивалью значит стационарностьможем принять только на уровне значимости в 90 и меньше процентов




# прогноз среднего и дисперсии на i шагов вперёд
#это все используется дальше в цикле
#dax.frc <- predict(dax.gfit,n.ahead=5)
#dax.frc[,1] # вектор средних
#dax.frc[,3]^2 # вектор дисперсий
# расчёт границы потерь
alpha <- 0.05
#VaR <- dax.frc[1,1]+dax.frc[1,3]*qged(alpha,mean=0,sd=1,
#                                      nu=dax.gfit@fit$par["shape"])
#Кривая VaR — набор последовательных во времени значений VaR
T1 <- 0.8*T; T2 <- T - T1 # обучающая и экзаменующая выборки
# на пространстве экзаменующей выборки построим набор
# последовательных значений VaR
VaR <- numeric()
h <- 0.18*T1

for (i in (T1+1):(T1+T2)) {
  h.dax <- dax[(i-h):(i-1)]
  dax.gfit <- garchFit(formula=~aparch(1,1),data=h.dax,
                       delta=2,include.delta=FALSE,leverage=TRUE,cond.dist="sged",
                       shape=1.5,include.shape=FALSE,trace=FALSE)
  dax.frc <- predict(dax.gfit,n.ahead=1)
  VaR[i-T1] <- dax.frc[1,1]+dax.frc[1,3]*qsged(alpha,mean=0,sd=1,
                                               nu=1.5,xi=dax.gfit@fit$par["skew"])
}



#Кривая VaR
# сравнение оценок риска с фактом
fact <- dax[(T1+1):(T1+T2)]
plot(fact,type="l")
lines(VaR,col="red")
#ylim=c(-5.2,-4.6)


# тест Купика в R:
K <- sum(fact<VaR); alpha0 <- K/T2
S <- -2*log((1-alpha)^(T2-K)*alpha^K)+
  2*log((1-alpha0)^(T2-K)*alpha0^K)
p.value <- 1-pchisq(S,df=1)
#высокий пвелью значит мы скорее всего правильно угодали альфу


############################Рассмотрим многомерный случай(портфель)
#Этапы моделирования:
 # 1. Оценка частных GARCH-моделей;
#2. Расчёт условных стандартизированных остатков 𝑧𝑖,𝑡
#3. Моделирование многомерной величины 𝑧�

#Модель «copula–GARCH» в R
# одномерные GARCH-модели
library(fGarch)
dax.gfit <- garchFit(data=dax,formula=~garch(1,1),
                     shape=1.25,include.shape=F,cond.dist="ged",trace=F)
smi.gfit <- garchFit(data=smi,formula=~garch(1,1),
                     shape=1.3,include.shape=F,cond.dist="sged",trace=F)
# стандартизированные остатки
z <- matrix(nrow=T,ncol=2)
z[,1] <- dax.gfit@residuals / dax.gfit@sigma.t
z[,2] <- smi.gfit@residuals / smi.gfit@sigma.t
# частные распределения остатков
mean <- c(0,0); sd <- c(1,1); nu <- c(1.25,1.3)
xi <- c(1, smi.gfit@fit$par["skew"])
cdf <- matrix(nrow=T,ncol=2)
for (i in 1:2) cdf[,i] <- psged(z[,i],mean=mean[i],
                                sd=sd[i],nu=nu[i],xi=xi[i])

#Модель «copula–GARCH» в R
#Моделирование копулы
library(copula)
# объявление копул
norm.cop <- normalCopula(dim=2,param=0.5,dispstr="un")
stud.cop <- tCopula(dim=2,param=0.5,df=5,
                    df.fixed=TRUE,dispstr="un")
gumb.cop <- gumbelCopula(dim=2,param=2)
clay.cop <- claytonCopula(dim=2,param=2)


# подгонка копул
norm.fit <- fitCopula(cdf,copula=norm.cop)
stud.fit <- fitCopula(cdf,copula=stud.cop)
gumb.fit <- fitCopula(cdf,copula=gumb.cop)
clay.fit <- fitCopula(cdf,copula=clay.cop)
# метод Монте-Карло
N=1000
cdf.sim <- rCopula(n=N,copula=stud.fit@copula)
z.sim <- matrix(nrow=N,ncol=2)
for (i in 1:2) z.sim[,i] <- qsged(cdf.sim[,i],
                                  mean=mean[i],sd=sd[i],nu=nu[i],xi=xi[i])
frc1 <- predict(dax.gfit,n.ahead=1)
frc2 <- predict(smi.gfit,n.ahead=1)
mu <- c(frc1[,1],frc2[,1])
sigma <- c(frc1[,3],frc2[,3])

#Оценка финансового риска
#Двумерный случай
# доходности портфеля из двух активов
prt <- cbind(dax, smi)
# оценка параметров модели
library(ghyp)
prt.fit <- fit.ghypmv(prt,symmetric=FALSE,silent=TRUE)

aic.mv <- stepAIC.ghyp(prt, dist=c("gauss","ghyp"),symmetric=NULL,silent=TRUE)


# выбор оптимальных весов активов в портфеле
opt <- portfolio.optimize(prt.fit,
                          risk.measure="value.at.risk",type="minimum.risk",
                          target.return=NULL,risk.free=NULL,level=0.95,silent=TRUE)
w=opt$opt.weights
# модельные доходности портфеля
prt.sim <- w[1]*(mu[1]+sigma[1]*z.sim[,1]) +
  w[2]*(mu[2]+sigma[2]*z.sim[,2])
# измерители риска
prt.sim <- sort(prt.sim)
VaR <- prt.sim[alpha*N]
ES <- mean(prt.sim[1:(alpha*N-1)])



# расчёт границы потерь

T <- length(dax); alpha <- 0.05

T1 <- 400; T2 <- T - T1 # обучающая и экзаменующая выборки

# на пространстве экзаменующей выборки построим набор

# последовательных значений VaR

x=w[1]*dax+w[2]*smi

VaR <- numeric()

h <- 0.2*T1

for (i in (T1+1):(T1+T2)) {
  
  h.dax <- x[(i-h):(i-1)]
  
  dax.gfit <- garchFit(formula=~aparch(1,1),data=h.dax,
                       
                       delta=2,include.delta=FALSE,leverage=TRUE,cond.dist="sged",
                       
                       shape=1.5,include.shape=FALSE,trace=FALSE)
  
  dax.frc <- predict(dax.gfit,n.ahead=1)
  
  VaR[i-T1] <- dax.frc[1,1]+dax.frc[1,3]*qsged(alpha,mean=0,sd=1,
                                               
                                               nu=1.5,xi=dax.gfit@fit$par["skew"])
  
}

fact <- x[(T1+1):(T1+T2)]

plot(fact,type="l")

lines(VaR,col="red")

#Проведем тест Купика
K <- sum(fact<VaR); alpha0 <- K/T2
S <- -2*log((1-alpha)^(T2-K)*alpha^K)+2*log((1-alpha0)^(T2-K)*alpha0^K)
p.value <- 1-pchisq(S,df=1)
#пивелью большой,значит мы нашли правильную альфа

#Функции потерь
#Величина функции потерь измеряет глубину пробоев кривой VaR
#и интерпретируется как размер понесённых потерь
#Функция потерь Лопеса:
L.Lo <- sum((fact-VaR)^2*(fact<VaR))/K
#Функция потерь Бланко-Ила:
L.BI <- sum((fact-VaR)/VaR*(fact<VaR))/K
L.Lo*10^4
L.BI
#Значения функции потерь в пределах нормы
