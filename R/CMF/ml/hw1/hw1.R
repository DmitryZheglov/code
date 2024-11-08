

datX= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw1/mtrain.csv",header=TRUE,sep=",")
datY= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw1/ytrain.csv",header=TRUE,sep=",")

a <- unlist(cbind(datY))
A <- as.matrix(datX)
A <- cbind(1,A)
n <- ncol(A) - 1
m <- nrow(A)
ntrain=2944
X=A[1:ntrain,1:(n+1)]
Xtest=A[(ntrain+1):m,1:(n+1)]
y=a[1:ntrain]
ytest=a[(ntrain+1):m]

#Bez regyliarizacii
g <- function(z){
    1/(1+exp(-z)) 
} 

J <- function(theta) {
  m <- nrow(X)
  h.theta <- g(X%*%theta)
  -t(y)%*%log(h.theta)/m - t(1-y)%*%log(1-h.theta)/m
}
gradJ <- function(theta) {
  m <- nrow(X)
  t(X)%*%(g(X%*%theta)-y)/m
}

theta0 <- cbind(rep(0,times=n+1))

opt <- optim(fn=J, gr=gradJ, par=theta0, method="BFGS")
theta <- opt$par; Jval <- opt$value
l=list(theta=as.vector(theta),J=Jval)



w=rep(0,times=nrow(Xtest))
K=function(x,theta) {
  yv=rep(0,times=nrow(Xtest))
  for( i in 1:nrow(x)) {
   s= x[i,1:ncol(x)]
   if((t(s)%*%theta) > 0) {
     yv[i]=1
   }
  }
  return(yv)
}
w=K(Xtest,theta)


u=abs(w-ytest)
ERROR=mean(u)
print(ERROR)

#regylacionnii methode
lambda=10

J.reg <- function(theta) {
  m <- nrow(X)
  h.theta <- g(X%*%theta)
  # ???????????????????????????????????? ??????????????
  J <- -t(y)%*%log(h.theta)/m - t(1-y)%*%log(1-h.theta)/m
  # ?????????????????????????????? ????????????????????????
  reg <- lambda*sum(theta^2)/(2*m)
  J + reg
}

gradJ.reg <- function(theta) {
  m <- nrow(X)
  # ???????????????????????????????????? ????????????????
  grad <- t(X)%*%(g(X%*%theta)-y)/m
  # ?????????????????????????????? ????????????????????????
  reg <- lambda/m * theta; reg[1] <- 0
  grad + reg
}

optReg <- optim(fn=J.reg, gr=gradJ.reg, par=theta0, method="BFGS")
thetaReg <- optReg$par; JvalReg <- optReg$value
lReg=list(thetaReg=as.vector(thetaReg),JReg=JvalReg)



wReg=rep(0,times=nrow(Xtest))
K=function(x,theta) {
  yv=rep(0,times=nrow(Xtest))
  for( i in 1:nrow(x)) {
    s= x[i,1:ncol(x)]
    if((t(s)%*%theta) > 0) {
      yv[i]=1
    }
  }
  return(yv)
}
wReg=K(Xtest,thetaReg)


uReg=abs(wReg-ytest)
ERRORReg=mean(uReg)
print(ERRORReg)

