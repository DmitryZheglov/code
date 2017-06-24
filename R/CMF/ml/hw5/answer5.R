library(metafor)
library(kernlab)
library(fBasics)
source("C:/Users/user/Desktop/proga/R/cmf/learn/hw5/SVM_func.R") 

#подключаем выборку
datX= read.csv("C:/Users/user/Desktop/proga/R/cmf/learn/hw5/AD_comp_train.csv",header=TRUE,sep=",")
datXT= read.csv("C:/Users/user/Desktop/proga/R/cmf/learn/hw5/AD_comp_test.csv",header=TRUE,sep=",")

X=datX[1:2]
XT=datXT[1:2]
y=datX[,3]

#Разделение выборки
m <- nrow(X)
# номера «аномальных» наблюдений
anom.obs <- (1:m)[y==1]; la <- length(anom.obs)
m.cv <- round(0.2*(m-la)) + la; m.train <- m - m.cv
# номера экзаменующей и обучающей выборок
cv.obs <- c( sample((1:m)[-anom.obs],size=m.cv-la,replace=FALSE),
             anom.obs )
train.obs <- (1:m)[-cv.obs]
# разделение выборки


y.train <- y
X.trainl=log(X+1)

# тесты на нормальность
#shapiro.test(X.trainl)
jarqueberaTest(X.trainl)
X.trainlc=(X.trainl[,2]/X.trainl[,1])^(50)
cor.test(X.trainl[,1],X.trainlc)
cor.test(X.trainlc,X.trainl[,2])
X=cbind(X.trainl,X.trainlc)


X.train <- X; X.cv <- XT
#Подбор параметров и расчёт «вероятности нормальности»
# оценки параметров
mu <- apply(X.train,2,mean)
sigma <- apply(X.train,2,sd)
# функция «вероятности»
p <- function(X,mu,sigma) {
  m <- nrow(X); n <- ncol(X)
  prob <- matrix(nrow=m,ncol=n)
  for (j in 1:n) prob[,j] <- dnorm(X[,j],mu[j],sigma[j])
  apply(prob,1,prod)
}





#Определение параметра 𝜀
prob.cv <- p(X.cv,mu,sigma)
pr <- range(prob.cv) 

# границы возможных значений 𝜀
res <- NULL # в неё будут сохраняться результаты моделирования
# для каждого наблюдения экзаменующей выборки рассчитываем
# прогноз при определённом значении 𝜀 и сравниваем его с фактом
for (eps in seq(pr[1], pr[2], length = 1000)) {
  y.pred <- 1*(prob.cv<=eps)
  res <- rbind(res, c(eps,fitStats(y.pred,y.cv)) )
}
dimnames(res)[[2]][1] <- "epsilon" # заголовки
# выбор наиболее подходящего 𝜀
j <- which.max(res[,"f1.score"])
eps <- res[j,"epsilon"]
# окончательный прогноз
y.pred <- 1*(prob.cv<eps)
# определение «вероятностей»
prob.train <- p(X.train,mu,sigma)

print(y.pred)



