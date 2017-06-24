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

#Enter p and q
p=1
q=1





library(tseries)
adf.test(log(t.ozone))
ar1 <- arma(log(t.ozone), order = c(p,q))

###PredictArma(Zheglov_DA)
predict.arma <- function(model, data, newdata, alpha){
  ar1 <- model
  
  x <- model$call 
  x <- as.character(x) 
  x <- x[length(x)]
  y <- charToRaw(x)
  p <- rawToChar(y[length(y)-4]); p <- as.numeric(p) 
  q <- rawToChar(y[length(y)-1]); q <- as.numeric(q) 
  
  y <- ar1$fitted.values
  e <- vector()
  y[1:p] = 0
  e[1:p] = 0
  
  e[p]=y[p]-ar1$coef[p+q+1]
  T <- length(data)
  
  for(t in (p+1):T) {
    e[t] <- y[t]-ar1$coef[p+q+1]
    i=1; j=1
    while ((t-i>0)&(i<p+1)) {
      e[t]=e[t]-ar1$coef[i]*y[t-i]
      i=i+1
    }
    while ((t-j>0)&(j<q+1)) {
      e[t]=e[t]-ar1$coef[p+j]*e[t-j]
      j=j+1
    }
  }
  v <- vector()
  residuals <- ar1$residuals
  residuals[1:p] = 0
  s=sum(residuals^2)/90
  
  
  v=y
  
  
  for (i in 1:20){
    
    e[T+i]=rnorm(1,0,sum(residuals^2)/90)
    w =  ar1$coef[p+q+1] + e[T+i]
    for (i in 1:p) w <- w+ar1$coef[i]*v[T+1-i]
    for (j in 1:q) w <- w+ar1$coef[j]*e[T+1-j]
    v=c(v,w)
  }
  
  n=length(c(data,newdata))
  w=v^2
  delta=sqrt(s)*sqrt(1+sum(w[(T+1):n])*sum(w[1:T])^-1)
  answer=list(fit=v, lower = v + delta*qt(alpha/2,df=19),upper = v - delta*qt(alpha/2,df=19))
  plot(exp(newdata), type="l",ylim=c(0,400))
  lines(exp(v[90:110]), lwd=3, col="yellow")
  lines(exp(answer$lower[91:111]),lty="dashed", lwd=2,col="red")
  lines(exp(answer$upper[91:111]),lty="dashed", lwd=2,col="red")
  
}
