#Нейронные сети в R
# структура сети (количество и размер слоёв)
source("C:/Users/user/Desktop/proga/R/cmf/learn/hw6/NN_functions.R")
model <- list(layer = c(2, 30, 30, 3), activation = c("sigmoid",
                                                      "sigmoid", "sigmoid")); L <- unlist(lapply(model, length))
# начальные значения параметров Θ
#𝑙выбираются случайным
# образом, чтобы избежать появления одинаковых нейронов
Theta <- InitPar(model, eps = 1)
# разделение выборки




DATX = read.csv("C:/Users/user/Desktop/proga/R/cmf/learn/hw6/train.csv",header=TRUE,stringsAsFactors = FALSE,sep=",")
X=cbind(DATX[,1],DATX[,2])

z=max(y)
y=NULL

for(i in 1:m){
  if(DATX[i,3]==1){
    y=rbind(y,c(1,0,0))
  }
  else if (DATX[i,3]==2) {
    y=rbind(y,c(0,1,0))
  }
  else{
    y=rbind(y,c(0,0,1))
  }
}

m=5000
m.train <- trunc(0.8*m); m.test <- m - m.train
X.train <- X[1:m.train,]; X.test <- X[(m.train+1):m,]
y.train <- y[1:m.train,]; y.test <- y[(m.train+1):m,]

#Структура исходных данных
#Пусть стоит задача разделения наблюдений на три класса, для
#чего имеются две объясняющие переменные, тогда матрицы
#исходных данных должны иметь следующий вид
head(X)
head(y)
#Единицы в строках матрицы y обозначают принадлежность
#наблюдения к одному из трёх классов, каждому из которых
#соответствует столбец этой матрицы

#Forward propagation
predictNN <- function(X, Theta, model, min.val = 10^-7) {
  
  L <- unlist(lapply(model, length))
  if (length(L) != 2) stop("In predictNN: invalid model")
  if (L[1] != L[2] + 1) stop("In predictNN: number of layers and number
                             of activations don't match")
  if (L[2] < 2) stop("network must have at least 2 layers")
  if (!all(model$activation %in% c("sigmoid", "relu", "softmax")))
    stop("In predictNN: activation must be in ('sigmoid', 'relu',
         'softmax')")
  act <- vector("list", L[2])
  for (i in 1:L[2]) {
    if (model$activation[i] == "relu") {
      act[[i]] <- relu
    } else if (model$activation[i] == "sigmoid") {
      act[[i]] <- sigmoid
    } else if (model$activation[i] == "softmax") {
      act[[i]] <- softmax
    }
  }
  
  # your code here
  list(a = a, z = z)
}


#Функция потерь
J <- function(X, y, theta, model, lambda = 0, min.val = 10^-7) {
  L <- unlist(lapply(model, length))
  if (length(L) != 2) stop("In J: invalid model")
  if (L[1] != L[2] + 1) stop("In J: number of layers and number of
                             activations don't match")
  if (L[2] < 2) stop("network must have at least 2 layers")
  m <- nrow(X)
  Theta <- unvec(theta, model)
  aL <- predictNN(X, Theta, model)$a[[L[1]]]
  aL[aL < min.val] <- min.val
  aL[aL > 1 - min.val] <- 1 - min.val
  err <- -sum(apply(y * log(aL), 1, sum) + apply((1 - y) * log(1 -
                                                                 aL), 1, sum)) / m
  reg <- lambda / (2*m) * sum(theta^2)
  err + reg
}

#Back propagation (градиент)
gradJ <- function(X, y, theta, model, lambda = 0) {
  m <- nrow(X); L <- unlist(lapply(model, length))
  # you may check if model is valid
  if (!all(model$activation %in% c("sigmoid", "relu", "softmax")))
    stop("In gradJ: activation must be in ('sigmoid', 'relu', 'softmax')")
  act.grad <- vector("list", L[2])
  for (i in 1:L[2]) {
    if (model$activation[i] == "relu") {
      act.grad[[i]] <- relu.grad
    } else if (model$activation[i] == "sigmoid") {
      act.grad[[i]] <- sigmoid.grad
    } else if (model$activation[i] == "softmax") {
      act.grad[[i]] <- softmax.grad
    }
  }
  Theta <- unvec(theta, model)
  pred <- predictNN(X, Theta, model)
  delta <- vector("list", L[1])
  Delta <- reg <- vector("list", L[2])
  # your code here
  
  vec(Delta)
  
}
#Подбор параметров нейронной сети
lambda <- 0.1 # например
# развёртка матриц Θ
# в векторный аргумент
theta <- vec(Theta, model)
# функции ошибки и градиента, зависящие только от theta
err <- function(x) J(X.train, y.train, x, model, lambda)
gr <- function(x) gradJ(X.train, y.train, x, model, lambda)
# минимизация ошибки (на обучающей выборке)
opt <- optim(fn=errFunc, gr=gr, par=theta, method="BFGS")
# свёртка матриц Θ
Theta <- unvec(opt$par, model)
#Построение прогноза
# кл ассификация (прогноз) на экзаменующей выборке
y.pred <- predictNN(X.test, Theta, model)$a[[L[1]]]
#Прогнозные значение обычно имеют следующий вид:
head(y.pred)