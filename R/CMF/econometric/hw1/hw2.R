#HomeWorkOneCMF

#TaskOne:download dataframe from finam
datYNDX= read.csv("/home/dz/kaggle/proga/R/cmf/ecmetr/hw1/YNDX.csv",header=TRUE,sep=",")

#TaskTwo:transfer price to rateOfReturn
dax=unlist((datYNDX[8]-datYNDX[5])/datYNDX[5],use.names = FALSE)
x=unique(dax)

#Test on Normalnost(criterii rabotaut tolko na viborkah bez povtoreniq ispolzuen function unique)
a=mean(x)
s=sd(x)
hist(dax,breaks =40)
test1=shapiro.test(dax)
test2=ks.test(x, "pnorm", mean = a, sd = s)
print(test1)
print(test2)
#dohodnosti raspredeleni ne normalno becouse y oboih criteriev p-value<0.05 nash yroven znachimosti

#graphic quantil quantil
qqplot(rnorm(n=10^3,mean=a,sd=s),dax,pch=16)
abline (0,1,lwd=5)
#tazhelii hvosti simvoliziryut o tom shto dohodnosti raspredeleni ne po normalnomy zakony,vozmozhno eto vibrosi i esli ot nih izbavitsia to polychim normalnoe raspredelenie

#
library(ghyp)

#provedem Ozenky parametrov modeli OGR
dax.ghyp <- fit.ghypuv(dax,symmetric=FALSE,silent=TRUE)
#histogramma modeli
hist(dax.ghyp)
#graphic qq quantil
qqghyp(dax.ghyp)
#Fittim ozenky vtorym methodom
dax.t <- fit.tuv(dax,symmetric=FALSE,silent=TRUE)
#proverim po testy LR
lik.ratio.test(dax.ghyp,dax.t,conf.level=0.95)
#mi ne otvergli hipotiz H0,znachit OGR obladaet toi zhe obiyasnitelnoi sposobnosti
#viberem model s naimenshim AIC
aic.uv <- stepAIC.ghyp(dax,dist=c("gauss","t","ghyp"),
                       symmetric=NULL,silent=TRUE)
summary(aic.uv$best.model)

#sozdadim viborky po modeli s lychimi zafichinimi parametrami:BestModel
alpha <- 0.1; N <- 10^6
dax.sim <- rghyp(n=N,object=aic.uv$best.model)
dax.sim <- sort(dax.sim)
#find value-at-risk and expected-shortfall
VaR <- dax.sim[alpha*N]
ES <- mean(dax.sim[1:(alpha*N-1)])

#razdelim viborky iz RateOfReturn na train and test
T=length(dax)-1
T1 <- 0.8*T; T2 <- T - T1
#zadadim voctor VaR
VaR <- numeric()
#obychaushaya
h <- 0.18*T1 
for (i in (T1+1):(T1+T2)) {
  h.dax <- dax[(i-h):(i-1)]
  dax.fit <- stepAIC.ghyp(h.dax,dist=c("gauss","t","ghyp"),
                          symmetric=NULL,silent=TRUE)
  VaR[i-T1] <- qghyp(alpha,object=dax.fit$best.model)
}
#stroim 2 graphica dlya test viborki and VaR and smotrim chto bi Var snizy kasalsya krivoi dohodnostei
fact <- dax[(T1+1):(T1+T2)]
plot(fact,type="l")
lines(VaR,col="red")

#sdelaem test cupica
K <- sum(fact<VaR)
alpha0 <- K/T2
S <- -2*log((1-alpha)^(T2-K)*alpha^K)+2*log((1-alpha0)^(T2-K)*alpha0^K)
p.value <- 1-pchisq(S,df=1)
print(p.value)
#vidim chto p.value bolshoi znachit y nas vse horosho i model kachestvennaya dlya ozenki riska
# rassmotrim eche odin pokazatel kachestva and podschitaem 2 functions of loss
L.Lo <- sum((fact-VaR)^2*(fact<VaR))/K
L.BI <- sum((fact-VaR)/VaR*(fact<VaR))/K
#oba criteriya pokazivaut chto mi adekvatno ozenivaem function of loss
print(L.Lo)
print(L.BI)


library(datasets)
#dawnload dataframe from finam
datYNDX= read.csv("/home/dz/kaggle/proga/R/cmf/ecmetr/hw3/RASP.csv",header=TRUE,sep=",")
datMTLR= read.csv("/home/dz/kaggle/proga/R/cmf/ecmetr/hw3/MTLR.csv",header=TRUE,sep=",")

#TaskTwo:transfer price to rateOfReturn
dax=unlist((datYNDX[8]-datYNDX[5])/datYNDX[5],use.names = FALSE)
smi=unlist((datMTLR[8]-datMTLR[5])/datMTLR[5],use.names = FALSE)
x=unique(dax)
y=unique(smi)
prt <- cbind(dax, smi)

library(ghyp)
prt.fit <- fit.ghypmv(prt,symmetric=FALSE,silent=TRUE)

aic.mv <- stepAIC.ghyp(prt, dist=c("gauss","ghyp"),symmetric=NULL,silent=TRUE)
summary(aic.mv$best.model)
w <- c(0.5,0.5) # vesa
N <- 10^6; alpha <- 0.1
sim <- rghyp(n=N,object=prt.fit)
prt.sim <- w[1]*sim[,1]+w[2]*sim[,2]
prt.sim <- sort(prt.sim)
VaR <- prt.sim[alpha*N]
ES <- mean(prt.sim[1:(alpha*N-1)]) 
print(VaR)
print(ES)
#opt <- portfolio.optimize(prt.fit,risk.measure="value.at.risk",type="minimum.risk",
#                          target.return=NULL,risk.free=NULL,level=0.95,silent=TRUE)
opt <- portfolio.optimize(prt.fit,risk.measure="value.at.risk", 
                          type="minimum.risk",target.return=NULL,risk.free=NULL,level=0.95, 
                          method="BFGS",silent=TRUE)
w=opt$opt.weights

ob=w[1]*dax+w[2]*smi

#razdelim viborky iz RateOfReturn na train and test
T=length(ob)-1
T1 <- 400; T2 <- T - T1
#zadadim voctor VaR
VaR <- numeric()
#obychaushaya
h <- 100 
for (i in (T1+1):(T1+T2)) {
  h.ob <- ob[(i-h):(i-1)]
  ob.fit <- stepAIC.ghyp(h.ob,dist=c("gauss","ghyp"),
                         symmetric=NULL,silent=TRUE)
  VaR[i-T1] <- qghyp(alpha,object=ob.fit$best.model)
}


#stroim 2 graphica dlya test viborki and VaR and smotrim chto bi Var snizy kasalsya krivoi dohodnostei



fact <- ob[(T1+1):(T1+T2)]
plot(fact,type="l")
lines(VaR,col="red")
#sdelaem test cupica
K <- sum(fact<VaR)
alpha0 <- K/T2
S <- -2*log((1-alpha)^(T2-K)*alpha^K)+2*log((1-alpha0)^(T2-K)*alpha0^K)
p.valueOptim <- 1-pchisq(S,df=1)
print(p.valueOptim)
#vidim chto p.value bolshoi znachit y nas vse horosho i model kachestvennaya dlya ozenki riska
# rassmotrim eche odin pokazatel kachestva and podschitaem 2 functions of loss
L.Lo <- sum((fact-VaR)^2*(fact<VaR))/K
L.BI <- sum((fact-VaR)/VaR*(fact<VaR))/K
print(L.Lo)
print(L.BI)
# viberem best model
dax.fit <- stepAIC.ghyp(dax,dist=c("gauss","t","ghyp"),
                        symmetric=NULL,silent=TRUE)$best.model
smi.fit <- stepAIC.ghyp(smi,dist=c("gauss","t","ghyp"),
                        symmetric=NULL,silent=TRUE)$best.model
#naidem raspredelenia
dax.cdf <- pghyp(dax,object=dax.fit)
smi.cdf <- pghyp(smi,object=smi.fit)
cdf <- cbind(dax.cdf, smi.cdf)

library(copula)
# zadadim copeli
norm.cop <- normalCopula(dim=2,param=0.5,dispstr="un")
stud.cop <- tCopula(dim=2,param=0.5,df=5,
                    df.fixed=TRUE,dispstr="un")
gumb.cop <- gumbelCopula(dim=2,param=2)
clay.cop <- claytonCopula(dim=2,param=2)
# zafitim copuli
norm.fit <- fitCopula(cdf,copula=norm.cop)
stud.fit <- fitCopula(cdf,copula=stud.cop)
gumb.fit <- fitCopula(cdf,copula=gumb.cop)
clay.fit <- fitCopula(cdf,copula=clay.cop)

# postroim elementi dlia copula stud
N <- 1000
stud.sim <- rCopula(n=N,copula=stud.fit@copula)
# slozhim quantili
dax.sim <- qghyp(stud.sim[,1],object=dax.fit)
smi.sim <- qghyp(stud.sim[,2],object=smi.fit)
w <- c(0.5,0.5)
prt.sim <- w[1]*dax.sim + w[2]*smi.sim
# find Var AND ES
alpha <- 0.1
prt.sim <- sort(prt.sim)
VaRCopSt <- prt.sim[alpha*N]
ESCopSt <- mean(prt.sim[1:(alpha*N-1)])
print(VaRCopSt)
print(ESCopSt)

#dlia gymbelya(analogichno)

N <- 1000
gumb.sim <- rCopula(n=N,copula=gumb.fit@copula)
dax.sim <- qghyp(gumb.sim[,1],object=dax.fit)
smi.sim <- qghyp(gumb.sim[,2],object=smi.fit)
w <- c(0.5,0.5)
prt.sim <- w[1]*dax.sim + w[2]*smi.sim
alpha <- 0.1
prt.sim <- sort(prt.sim)
VaRCopGumb <- prt.sim[alpha*N]
ESCopGumb <- mean(prt.sim[1:(alpha*N-1)])

#Sravnim
print(VaR)
print(ES)
print(VaRCopSt)
print(ESCopSt)
print(VaRCopGumb)
print(ESCopGumb)
#y opt yhe best
opt <- portfolio.optimize(prt.fit,risk.measure="value.at.risk", 
                          type="minimum.risk",target.return=NULL,risk.free=NULL,level=0.95, 
                          method="BFGS",silent=TRUE)
w=opt$opt.weights

ob=w[1]*dax+w[2]*smi

#razdelim viborky iz RateOfReturn na train and test
T=length(ob)-1
T1 <- 400; T2 <- T - T1
#zadadim voctor VaR
VaR <- numeric()
#obychaushaya
h <- 100 
for (i in (T1+1):(T1+T2)) {
  h.ob <- ob[(i-h):(i-1)]
  ob.fit <- stepAIC.ghyp(h.ob,dist=c("gauss","ghyp"),
                         symmetric=NULL,silent=TRUE)
  VaR[i-T1] <- qghyp(alpha,object=ob.fit$best.model)
}


#stroim 2 graphica dlya test viborki and VaR and smotrim chto bi Var snizy kasalsya krivoi dohodnostei



fact <- ob[(T1+1):(T1+T2)]
plot(fact,type="l")
lines(VaR,col="red")
#sdelaem test cupica
K <- sum(fact<VaR)
alpha0 <- K/T2
S <- -2*log((1-alpha)^(T2-K)*alpha^K)+2*log((1-alpha0)^(T2-K)*alpha0^K)
p.valueOptim <- 1-pchisq(S,df=1)
print(p.valueOptim)
#vidim chto p.value bolshoi znachit y nas vse horosho i model kachestvennaya dlya ozenki riska
# rassmotrim eche odin pokazatel kachestva and podschitaem 2 functions of loss
L.Lo <- sum((fact-VaR)^2*(fact<VaR))/K
L.BI <- sum((fact-VaR)/VaR*(fact<VaR))/K
print(L.Lo)
print(L.BI)