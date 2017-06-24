library(np)
library(datasets)
ozone <- airquality$Ozone
rad <- airquality$Solar.R
rem <- is.na(ozone) | is.na(rad)
ozone <- ozone[!rem]; rad <- rad[!rem]
# разделим выборку на обучающую и экзаменующую
N <- length(ozone); E <- 20; T <- N-E
train.obs <- (1:T)
eval.obs <- (T+1):N
t.rad <- rad[train.obs]; t.ozone <- ozone[train.obs]
e.rad <- rad[eval.obs]; e.ozone <- ozone[eval.obs]

bw <- npregbw(ozone ~ rad, ckertype="gaussian",
              bwtype="fixed", data=data.frame(ozone=t.ozone,rad=t.rad))
h <- bw$bw

# ядро и функция Надарая – Уотсона
kern <- function(x) exp(-(x^2/2))/sqrt(2*pi)
NW <- function(x, x.dat, y.dat, h) {
  K1 <- K2 <- 0
  N <- length(y.dat)
  for (i in 1:N) {
    K1 <- K1 + kern((x-x.dat[i])/h)*y.dat[i]
    K2 <- K2 + kern((x-x.dat[i])/h)
  }
  K1 / K2
}
plot(t.rad,t.ozone,pch=16)
z <- order(t.rad)
lines(t.rad[z],NW(t.rad,t.rad,t.ozone,h)[z],col="blue",lw=3)

plot(t.rad,t.ozone,pch=16)
fit.npar <- npreg(bw)
ozone.hat <- predict(fit.npar)
lines(t.rad[z],ozone.hat[z],col="blue",lw=3)

e <- t.ozone - ozone.hat
s2 <- var(e)
g <- (4/(3*T))^(1/5)*sqrt(s2)
# симулированные значения ошибок
b <- 10^4
e.star <- e[sample(1:T,size=b,replace=TRUE)]+g*rnorm(b)
e.star <- sort(e.star)
# прогноз и доверительные границы
alpha <- 0.1
y <- predict(fit.npar,newdata=data.frame(rad=e.rad))
bottom <- y + e.star[alpha/2*b]
top <- y + e.star[(1-alpha/2)*b]

z <- order(e.rad)
plot(e.rad,e.ozone,ylim=range(c(top,bottom)))
lines(e.rad[z],y[z],col="blue",lwd=2)
lines(e.rad[z],top[z],col="red",lty="dashed")
lines(e.rad[z],bottom[z],col="red",lty="dashed")

# моделирование условной дисперсии
s2 <- (e-mean(e))^2
h.res <- npregbw(s2 ~ rad, ckertype="gaussian",
                 bwtype="fixed", data=data.frame(rad=t.rad))
res.npar <- npreg(h.res)
# метод бутстрапа
b <- 10^4; alpha <- 0.1
top <- bottom <- numeric(E)
y <- predict(fit.npar,newdata=data.frame(rad=e.rad))
for (i in 1:E) {
  s2.hat <- predict(res.npar,newdata=data.frame(rad=e.rad[i]))
  g <- (4/(3*T))^(1/5)*sqrt(s2.hat)
  e.star <- e[sample(1:T,size=b,replace=TRUE)]+g*rnorm(b)
  e.star <- sort(e.star)
  bottom[i] <- y[i] + e.star[alpha/2*b]
  top[i] <- y[i] + e.star[(1-alpha/2)*b]
}

z <- order(e.rad)
plot(e.rad,e.ozone,ylim=range(c(top,bottom)))
lines(e.rad[z],y[z],col="blue",lwd=2)
lines(e.rad[z],top[z],col="red",lty="dashed")
lines(e.rad[z],bottom[z],col="red",lty="dashed")
