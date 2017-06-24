#Viboru
library(kernlab)
source("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw2/SVM_func.R") 
# файл с пользовательскими функциями

datX= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw2/mtrain.csv",header=TRUE,stringsAsFactors = FALSE,sep=",")
datY= read.csv("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw2/mtest.csv",header=TRUE,stringsAsFactors = FALSE,sep=",")


######################Obrabotka traina
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
Xtrain=cbind(1,popul=datX$popul,TWnews=datX$TVnews,ClinLR,DoleLR,age=datX$age,educ,income)
################################Cancle obrabotki traina

#Obnylenie peremennix
polit=NULL
DoleLR=NULL
ClinLR=NULL
educ=NULL
income=NULL
o=NULL
vote=NULL
pi=NULL



################################Start obrabotki testa


ClinLR <- datY$ClinLR
head(ClinLR)

polit <- c(extLib = 1, Lib = 2, sliLib = 3, Mod = 4, sliCon = 5, Con = 6, extCon = 7)
polit

for (i in 1:length(polit)) {
  repl <- ClinLR == names(polit[i])
  ClinLR[repl] <- replace(ClinLR, repl, polit[i])[repl]
}
ClinLR <- as.numeric(ClinLR)

DoleLR <- datY$DoleLR
head(DoleLR)
polit <- c(extLib = 1, Lib = 2, sliLib = 3, Mod = 4, sliCon = 5, Con = 6, extCon = 7)
polit

for (i in 1:length(polit)) {
  repl <- DoleLR == names(polit[i])
  DoleLR[repl] <- replace(DoleLR, repl, polit[i])[repl]
}
DoleLR <- as.numeric(DoleLR)

educ <- datY$educ
head(educ)
polit <- c(MS = 1, HSdrop = 2, HS = 3, Coll = 4, CCdeg = 5, BAdeg = 6, MAdeg = 7)
polit

for (i in 1:length(polit)) {
  repl <- educ == names(polit[i])
  educ[repl] <- replace(educ, repl, polit[i])[repl]
}
educ <- as.numeric(educ)

income=datY$income

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

vote=datY$vote
pi=numeric()
for(i in 1:length(vote)){
  if(vote[i]=="Dole"){
    pi[i]=0
  }else{
    pi[i]=1
  }
}
vote=pi

Xtest=cbind(1,popul=datY$popul,TWnews=datY$TVnews,ClinLR,DoleLR,age=datY$age,educ,income)


######################################cancle obrabotki testa



X.train <- Xtrain
y.train <- y



model <- ksvm(X.train, y.train, type="C-svc",
              C = 0.5, kern = "rbfdot",
              kpar = list(sigma=0.01))
# прогнозная классификация на экзаменующей выборке
y.pred <- predict(model,Xtest, type = "response")
# запись комбинации экзогенных параметров и статистик
# прогноза, возвращаемых пользовательской функцией fitStats
library(xlsx)
write.xlsx(y.pred,"C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw2/ypred.xlsx",sheetName="ypred",
           col.names=TRUE,row.names=FALSE,append=FALSE)