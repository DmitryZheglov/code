#Viboru
library(kernlab)
source("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw2/SVM_func.R") 
# файл с пользовательскими функциями

datX= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw2/mtrain.csv",header=TRUE,stringsAsFactors = FALSE,sep=",")
datY= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw2/mtest.csv",header=TRUE,stringsAsFactors = FALSE,sep=",")



ClinLR <- datX$ClinLR
head(ClinLR)

polit <- c(extLib = 1, Lib = 2, sliLib = 3, Mod = 4, sliCon = 5, Con = 6, extCon = 7)
polit

for (i in 1:length(polit)) {
  repl <- ClinLR == names(polit[i])
  ClinLR[repl] <- replace(ClinLR, repl, polit[i])[repl]
}
ClinLR <- as.numeric(ClinLR)

DoleLR <- datX$DoleLR
head(DoleLR)
polit <- c(extLib = 1, Lib = 2, sliLib = 3, Mod = 4, sliCon = 5, Con = 6, extCon = 7)
polit

for (i in 1:length(polit)) {
  repl <- DoleLR == names(polit[i])
  DoleLR[repl] <- replace(DoleLR, repl, polit[i])[repl]
}
DoleLR <- as.numeric(DoleLR)

educ <- datX$educ
head(educ)
polit <- c(MS = 1, HSdrop = 2, HS = 3, Coll = 4, CCdeg = 5, BAdeg = 6, MAdeg = 7)
polit

for (i in 1:length(polit)) {
  repl <- educ == names(polit[i])
  educ[repl] <- replace(educ, repl, polit[i])[repl]
}
educ <- as.numeric(educ)

income=datX$income

o=numeric()
for(i in 1:length(income)){
if(income[i]=="$105Kplus"){
  o[i]=105
}else if(income[i]=="$3Kminus"){
  o[i]=3
  }else{
  v=unlist(strsplit(income[i],"-")) 
  v1=unlist(strsplit(v[1],"K")) 
  v2=unlist(strsplit(v[2],"K")) 
  n1=nchar(v1) 
  n2=nchar(v2)
  b1=substring(v1,2,n1)
  b2=substring(v2,2,n2)
  b1=as.numeric(b1)
  b2=as.numeric(b2)
  o[i]=(b1+b2)/2
}
}
income=o

vote=datX$vote
pi=numeric()
for(i in 1:length(vote)){
  if(vote[i]=="Dole"){
    pi[i]=0
  }else{
    pi[i]=1
  }
}
vote=pi
y=vote
X=cbind(1,popul=datX$popul,TWnews=datX$TVnews,ClinLR,DoleLR,age=datX$age,educ,income)
m <- nrow(X)
m.train <- round(0.8*m); m.cv <- m - m.train
train.obs <- sample(1:m,size=m.train,replace=FALSE)
cv.obs <- (1:m)[-train.obs]
X.train <- X[train.obs,]; X.cv <- X[cv.obs,]
y.train <- y[train.obs]; y.cv <- y[cv.obs]

par <- c(0.01,0.05,0.1,0.5,1,5,10,50,
         100,500,1000)
par <- expand.grid(par,par)
# заголовки столбцов
dimnames(par)[[2]] <- c("C","sigma")

res <- NULL # в неё будут записаны результаты моделирования


for (i in 1:nrow(par)) { # для каждой комбинации экз. параметров
  # подбор параметров 𝜃 на обучающей выборке
  model <- ksvm(X.train, y.train, type="C-svc",
                C = par$C[i], kern = "rbfdot",
                kpar = list(sigma=par$sigma[i]))
  # прогнозная классификация на экзаменующей выборке
  y.pred <- predict(model, newdata = X.cv, type = "response")
  # запись комбинации экзогенных параметров и статистик
  # прогноза, возвращаемых пользовательской функцией fitStats
  res <- rbind(res, c(par$C[i],par$sigma[i],fitStats(y.cv,y.pred)) )
}
dimnames(res)[[2]][1:2] <- c("C","sigma") # заголовки столбцов
b=res[,6]
which.max(b[28:121])
  
 