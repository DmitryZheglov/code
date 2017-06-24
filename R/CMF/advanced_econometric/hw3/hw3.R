#Методы выбора регрессоров,регуляризации и снижения размерности
#Исходные данные
statedata <- data.frame(state.x77, row.names = state.abb, check.names = T)
head(statedata)
y=statedata[4]
#Пошаговое исключение регрессоров (1/4)
# исходная модель с полным списком регрессоров
ols <- lm(Life.Exp ~ ., data = statedata)
summary(ols); extractAIC(ols)
#Пошаговое исключение регрессоров (2/4)
# исключение площади штата
ols <- update(ols, . ~ . - Area)
summary(ols); extractAIC(ols)
#Пошаговое исключение регрессоров (3/4)
# финальная версия модели
ols <- update(ols, . ~ . - Area-Illiteracy-Income)
summary(ols); extractAIC(ols)
#Пошаговое исключение регрессоров (4/4)
# автоматизированная процедура на основе критерия AIC
ols <- lm(Life.Exp ~ ., data = statedata)
step.back <- step(ols, direction = "backward", trace = 0)
summary(step.back); extractAIC(step.back)

#Пошаговое добавление регрессоров
# начинаем с самой простой модели (только константа)
ols <- lm(Life.Exp ~ 1, data = statedata)
step.fwd <- step(ols, scope = list(lower = . ~ ., upper = . ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area), direction = "forward", trace = 0)
summary(step.fwd); extractAIC(step.fwd)
library(penalized)
#PCR в R
X <- as.matrix(statedata[,-4]); y <- cbind(statedata[,4])
e <- eigen(t(X)%*%X)
Z.pc <- X %*% e$vectors
# проверим, насколько хорошо каждая из главных компонент
# объясняет дисперсию y
d <- ncol(X)
tss <- sum((y - mean(y))^2)
beta <- vector("list", d); r.sq <- numeric(d)
for (i in 1:d) {
  Z <- cbind(1, Z.pc[,i])
  beta[[i]] <- solve(t(Z)%*%Z)%*%t(Z)%*%y
  y.hat <- Z %*% cbind(beta[[i]])
  rss <- sum((y - y.hat)^2)
  r.sq[i] <- 1 - rss/tss
}
lasso <- penalized(y, X, unpenalized = ~1, lambda1 = 10,
                   lambda2 = 0, model = "linear", trace = FALSE)
beta.lasso <- c(lasso@unpenalized, lasso@penalized)
r.sq
#Переставим столбцы 𝑍𝑝𝑐 в соответствии со значеrrrrниrrями 𝑅2rr
z <- order(r.sq, decreasing = TRUE)
Z.pc <- Z.pc[,z]
beta <- vector("list", d)
r.sq <- numeric(d); delta.r <- numeric(d)
for (p in 1:d) {
  Z <- cbind(1, Z.pc[,1:p])
  beta[[p]] <- solve(t(Z)%*%Z)%*%t(Z)%*%y
  y.hat <- Z %*% cbind(beta[[p]])
  rss <- sum((y - y.hat)^2)
  r.sq[p] <- 1 - rss/tss
  delta.r[p] <- ifelse(p == 1, r.sq[p], r.sq[p] - r.sq[p-1])
}
p <- 4
B <- e$vectors[,z][,1:p]
Z.test <- cbind(1, (X %*% B))
y.hat <- Z.test %*% cbind(beta[[p]])
beta.pcr <- c(beta[[p]][1], B %*% cbind(beta[[p]][-1]))
y.hat <- cbind(1, X) %*% cbind(beta.pcr)
library(pls)
pls <- plsr(Life.Exp ~ ., data = statedata, ncomp = 7)
summary(pls)
b <- coef(pls)
b0 <- pls$fitted[1] - sum(pls$model[1,-1] * b)
beta.pls <- c(b0, b)