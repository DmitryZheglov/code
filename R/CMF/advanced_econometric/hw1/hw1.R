library(datasets)
ozone <- airquality$Ozone
rad <- airquality$Solar.R
rem <- is.na(ozone) | is.na(rad)
ozone <- ozone[!rem]; rad <- rad[!rem]
d=2

N <- length(ozone); E <- 20; T <- N-E
train.obs <- (1:T)
eval.obs <- (T+1):N
t.rad <- rad[train.obs]; t.ozone <- ozone[train.obs]
e.rad <- rad[eval.obs]; e.ozone <- ozone[eval.obs]

fit.par <- lm(ozone ~ radiation,data=data.frame(radiation=t.rad,ozone=t.ozone),weights=NULL)

summary(fit.par)

fit.par$coefficients
fit.par$residuals
fit.par$fitted.values
plot(t.rad,t.ozone,pch=16,
     xlab="radiation",ylab="ozone")
z <- order(t.rad)
lines(t.rad[z],
      fit.par$fitted.values[z],
      col="blue",lwd=3)




res <- fit.par$residuals
hist(res)
plot(res,type="l")

#regress model
library(fBasics)
print(shapiro.test(res))
print(jarqueberaTest(res))


fit.par <- lm(log(ozone) ~ rad + rad2,
              data=data.frame(rad=t.rad,rad2=t.rad^2,ozone=t.ozone))
plot(t.rad,t.ozone,pch=16,
     xlab="radiation",ylab="ozone")
z <- order(t.rad)
lines(t.rad[z],
      exp(fit.par$fitted.values[z]),
      col="blue",lwd=3)

res <- fit.par$residuals
hist(res)
plot(res,type="l")
print(shapiro.test(res))
print(jarqueberaTest(res))

library(lmtest)
# ???????? ?????????????????????????
print(bptest(fit.par,varformula=NULL,data=NULL,studentize=FALSE))

# ???????? ?????????????????????????????????
print(gqtest(fit.par,fraction=25,alternative="two.sided"))

#other model
oz <- lm(t.ozone ~ t.rad)
bptest(oz,varformula=NULL,data=NULL,studentize=FALSE)
print(bptest(oz,varformula=NULL,data=NULL,studentize=FALSE))
#???????????????????? ???????????? ?????????????????????? ???????????????????? ????
e.sq <- oz$residuals^2
sigma.hat <- lm(e.sq ~ t.rad)$fitted.values ^ 0.5
#print(sigma.hat <- lm(e.sq ~ t.rad)$fitted.values ^ 0.5)
#???????????????????? ???????????????????? ??????
oz.wgt <- lm(t.ozone ~ t.rad, weights = 1/sigma.hat)
print(oz.wgt)

# ???????? ????????????????????????????? ?? R(???????????????????? ???????? ?????????? ???? ?????????? ???? ???? ??????????????????????)
dw=dwtest(fit.par,alternative="two.sided")

###Select autoregrission component
library(tseries) 

plot(log(t.ozone), type = "l", lwd=3, col="grey", xlab="Time") 

adf.test(log(t.ozone)) 

ar1 <- arma(log(t.ozone), order=c(1,0)) 
lines(ar1$fitted.values, lwd=3, col="blue") 

adf.test(ar1$residuals[2:T]) 
##############


library(tseries)
adf.test(log(t.ozone))

ar1 <- arma(log(t.ozone), order = c(1,0))

adf.test(ar1$residuals[2:T])


fit.par <- lm(ozone ~ rad + rad2,
              data = data.frame(ozone = ar1$residuals[2:T],
                                rad = t.rad[2:T], rad2 = t.rad[2:T]^2))

#z <- sort(t.rad)
#plot(t.rad[z],ar1$residuals[z],xlab="Radiation",ylab="Ozone",col="grey",pch=20)
#x=t.rad
#lines(x[z],fit.par$coefficients[1]+x[z]*fit.par$coefficients[2]+x[z]*x[z]*fit.par$coefficients[3],col="blue",type='p')
#x=t.rad
#lines(t.rad[z], fit.par$coefficients[1]+t.rad[z]*fit.par$coefficients[2]+t.rad[z]^2*fit.par$coefficients[3], col="blue", lwd=3, type="p") 
#Graphic!!!!!!!!!autoregress


z <- sort(t.rad) 
plot(t.rad,ar1$residuals,pch=16) 


lines(z, 
      fit.par$coefficients[1]+z*fit.par$coefficients[2]+z^2*fit.par$coefficients[3], 
      col="blue",lwd=3,type='l')

