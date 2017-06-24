#Zheglov Dmitry home work2
library(lmtest)
# расчёт величины h
library(np)
library(tseries)
# исходные данные
library(datasets)
ozone <- moneydemand[,1]
rad <- moneydemand[,2]
Rs=moneydemand[,3]
rem <- is.na(ozone) | is.na(rad) | is.na(Rs)
ozone <- ozone[!rem]; rad <- rad[!rem];Rs <- Rs[!rem]
# разделим выборку на обучающую и экзаменующую
N <- length(ozone); E <- 20; T <- N-E
train.obs <- (1:T)
eval.obs <- (T+1):N
t.rad <- rad[train.obs]; t.ozone <- ozone[train.obs];t.temp<- Rs[train.obs]
e.rad <- rad[eval.obs]; e.ozone <- ozone[eval.obs];e.temp <- Rs[eval.obs]
source("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw2/SVM_func.R")


#Тест на стационарность
adf.test(t.ozone)
#применим арму для оценки значений
ar1 <- arma(t.ozone, order = c(1,1))
#зафитим параметры в линейной модели от двух неизвестный(фунция выбрана как сумма,но можно другую)
fit.par <- lm(ozone ~ rad + temp,
              data = data.frame(ozone = ar1$residuals[2:T],
                                rad = t.rad[2:T], temp = t.temp[2:T]))

# тесты на нормальностьparam
res <- fit.par$residuals
hist(res)


library(fBasics)
shapiro.test(res)
jarqueberaTest(res)
#pvalue большой знаит остатки нормальны
#tests na geteroksedichost
library(lmtest)
# тест Бреуша–Пагана
bptest(fit.par,varformula=NULL,data=NULL,studentize=FALSE)
# тест Голдфелда–Куандта
gqtest(fit.par,fraction=25,alternative="two.sided")




#тесты имея большие пи валью значит стоит построить модель по взвешенному МНК(в топку)
#e.sq <- fit.par$residuals^2 
#sigma.hat <- lm(e.sq ~ t.rad[2:T])$fitted.values ^ 0.5
#model <- lm(t.ozone[2:T] ~ t.rad[2:T], weights = 1/sigma.hat) 
#summary(model)

#plot(t.rad,t.ozone,pch=16, xlab="logY",ylab="logM")  
#z <- order(t.rad) 
#lines(t.rad[z], model$fitted.values[z], col="blue",lwd=3)


#autocorralation
# тест Дарбина–Ватсона в R
dwtest(fit.par,alternative="two.sided")
#net autocorrelacii







#можно было подгрузить арму,но я пропишу ее заного
###PredictArma(воспользуемся построеной нами армой для построения прогноза по авторегрессии)
predict.arma <- function(model, data, newdata, alpha = 0.05) { 
  alldata <- c(data,newdata) 
  l <- length(alldata) 
  l1 <- length(data) 
  l2 <- length(newdata) 
  p <- length(model$lag$ar) 
  
  X <- matrix(0, ncol = p+1, nrow = l-p) 
  for(i in 1:(l-p)) { 
    X[i,] = c(1,alldata[i:(i+p-1)]) 
  } 
  y <- cbind(alldata[(p+1):l]) 
  
  y.fc <- numeric() 
  upper <- numeric() 
  lower <- numeric() 
  
  for (i in (l1):(l-p)) { 
    X.t <- X[1:(i-1),] 
    y.t <- y[1:(i-1)] 
    beta <- solve(t(X.t)%*%X.t)%*%t(X.t)%*%as.vector(y.t) 
    y.theor <- X.t%*%beta 
    res <- y.t-y.theor 
    sigma2 <- sum(res^2)/(i-p-1) 
    X.next <- X[i,] 
    y.next <- t(X.next)%*%beta 
    
    delta <- sqrt(sigma2 * (1 + t(X.next)%*%solve(t(X.t)%*%X.t)%*%X.next)) 
    dx = delta*qt(alpha/2,df=(i-1)-p) 
    
    y.fc[i-l1+1] <- y.next 
    lower[i-l1+1] = y.fc[i-l1+1] + as.vector(dx) 
    upper[i-l1+1] = y.fc[i-l1+1] - as.vector(dx) 
    print(dx)
  } 

  plot(newdata, type="l",ylim=c(-5.2,-4.6)) 
  lines(y.fc, lwd=3, col="yellow") 
  lines(lower,lty="dashed", lwd=2,col="red") 
  lines(upper,lty="dashed", lwd=2,col="red") 
  
  # list 
  list(fit = y.fc, lower = lower, upper = upper) 
}
#вызову арму
ar1.frc <- predict.arma(model = ar1,
                        data = t.ozone[1:(T)],
                        newdata = e.ozone, alpha = 0.1)

par.frc <- predict(fit.par,
                  newdata=data.frame(rad=e.rad[2:E],temp=e.temp[2:E]),
                  se.fit=TRUE,interval="prediction",level=0.90)
#учтем остатки
frc <- (ar1.frc$fit[2:E] + par.frc$fit[,"fit"])
#построим получивщшийся прогноз  
plot(e.ozone, type="l",ylim=c(-5.2,-4.6)) 
lines(frc, lwd=3, col="yellow") 
lines(ar1.frc$lower[2:E]+ par.frc$fit[,"fit"],lty="dashed", lwd=2,col="red") 
lines(ar1.frc$upper[2:E]- par.frc$fit[,"fit"],lty="dashed", lwd=2,col="red") 




#autocorralation
# тест Дарбина–Ватсона в R
dwtest(fit.par,alternative="two.sided")
#net autocorrelacii


#######################################Neparametrihexkaya
# добавочная объясняющая переменная
#temp=Rs
# регрессионная модель
#kern <- function(x) exp(-(x^2/2))/sqrt(2*pi)
#NW2 <- function(x, x.dat, y.dat, h) {
 # K1 <- K2 <- 0
 # N <- length(y.dat)
 # for (i in 1:N) {
 #   K1 <- K1 + kern((x-x.dat[i])/h)*y.dat[i]
 #   K2 <- K2 + kern((x-x.dat[i])/h)
 # }
  #K1 / K2
#}

# регрессионная модель (другой способ) многомерный случай от двух переменных
#make kernal and use Nadra-Uotson
bw2 <- npregbw(ozone ~ rad + temp, 
               ckertype="gaussian", bwtype="fixed", 
               data=data.frame(ozone=t.ozone,rad=t.rad,temp=t.temp)) 

#фитим параметры
ozone.reg <- npreg(bw2) 
ozone.hat <- predict(ozone.reg) 
fit.npar <- npreg(bw2) 
#график оценки
zt <- order(t.rad) 
plot(t.ozone[zt],pch=16) 
lines(ozone.hat[zt],col="blue",lwd=3) 
#вычислим ошибку для построения прогноза
e <- t.ozone - ozone.hat 
s2 <- (e-mean(e))^2 
h.res <- npregbw(s2 ~ rad + temp, ckertype="gaussian", bwtype="fixed", data=data.frame(rad=t.rad,temp=t.temp)) 
res.npar <- npreg(h.res) 
# метод бутстрапа для моделирования прогноза и доверитеьного интервала при условной диспресии остатков
b <- 10^4; alpha <- 0.1 
top <- bottom <- numeric(E) 
y <- predict(fit.npar,newdata=data.frame(rad=e.rad,temp=e.temp)) 
for (i in 1:E) { 
  s2.hat <- predict(res.npar,newdata=data.frame(rad=e.rad[i],temp=e.temp[i])) 
  g <- (4/(3*T))^(1/5)*sqrt(s2.hat) 
  e.star <- e[sample(1:T,size=b,replace=TRUE)]+g*rnorm(b) 
  e.star <- sort(e.star) 
  bottom[i] <- y[i] + e.star[alpha/2*b] 
  top[i] <- y[i] + e.star[(1-alpha/2)*b] 
} 
#изобразим прогноз и Доверительный интервал(мы получили менее точный прогноз нежели в методе авторересии)
z <- order(e.rad) 
plot(e.rad,e.ozone,ylim=c(-5.5,-4.5))
lines(e.rad,y,col="blue",lwd=2) 
lines(e.rad[z],top[z],type="l",col="red",lty="dashed") 
lines(e.rad[z],bottom[z],type="l",col="red",lty="dashed")


