library(datasets)
#TaskOne:download dataframe from finam
datYNDX= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/ecmetr/hw3/RASP.csv",header=TRUE,sep=",")
datMTLR= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/ecmetr/hw3/MTLR.csv",header=TRUE,sep=",")
datMTLRTEST= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/ecmetr/hw3/MTLRTEST.csv",header=TRUE,sep=",")
ozone <- datMTLR$X.OPEN.
smi=unlist((datMTLR[8]-datMTLR[5])/datMTLR[5],use.names = FALSE)
rad <- smi
#ozone <- datYNDX$X.OPEN.
#dax=unlist((datYNDX[8]-datYNDX[5])/datYNDX[5],use.names = FALSE)
#rad <- dax
rem <- is.na(ozone) | is.na(rad)
ozone <- ozone[!rem]; rad <- rad[!rem]
d=2
#N <- length(ozone); E <- 20; T <- N-E


#e.ozone <- datMTLRTEST$X.OPEN.
e.ozone=datMTLRTEST$X.OPEN.
t.rad <- rad; t.ozone <- ozone
#e.rad <- rad[eval.obs];
E=length(e.ozone)
N=length(t.ozone)+length(e.ozone)
T <- N-E
e.rad=unlist((datMTLRTEST[8]-datMTLRTEST[5])/datMTLRTEST[5],use.names = FALSE)


#Enter p and q





library(tseries)
adf.test(log(t.ozone))
ar1 <- arma(log(t.ozone), order = c(1,0))



fit.par <- lm(ozone ~ rad + rad2,
              data = data.frame(ozone = ar1$residuals[2:T],
                                rad = t.rad[2:T], rad2 = t.rad[2:T]^2))



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
                   newdata=data.frame(rad=e.rad[2:E],rad2=e.rad[2:E]^2),
                   se.fit=TRUE,interval="prediction",level=0.90)
frc <- exp(ar1.frc$fit[2:E] + par.frc$fit[,"fit"])
plot(e.ozone, type="l",ylim=c(-100,200)) 
lines(frc, lwd=3, col="yellow") 

