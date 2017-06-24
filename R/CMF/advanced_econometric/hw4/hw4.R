# исходные данные
library(plm)
#data(Gasoline)
#head(Gasoline)

data(EmplUK)
head(EmplUK)

#Пошаговое исключение регрессоров (1/4)
# исходная модель с полным списком регрессоров
ols <- lm(emp ~ ., data = EmplUK)
summary(ols); extractAIC(ols)

#Объединённая регрессия
model.formula <- emp ~ wage + capital + output
gas.ls <- plm(model.formula,data=EmplUK,index=c("firm","year"),
              model="pooling",effect="individual")
summary(gas.ls)

#Модель с фиксированными эффектами
gas.fe <- plm(model.formula,data=EmplUK,index=c("firm","year"),
              model="within",effect="individual")
summary(gas.fe)

#Извлечение индивидуальных эффектов
summary(fixef(gas.fe))

#Модель со случайными эффектами
gas.fee <- plm(model.formula,data=EmplUK,index=c("firm","year"),
              model="random",effect="individual")
summary(gas.fee)

# объединённая регрессия против фиксированных эффектов
pooltest(model.formula,data=EmplUK,index=c("firm","year"),
         model="within",effect="individual")

# объединённая регрессия против случайных эффектов
plmtest(model.formula,data=EmplUK,index=c("firm","year"),
        effect="individual",type="bp")

# случайные эффекты против фиксированных
phtest(model.formula,data=EmplUK,index=c("firm","year"),
       model=c("within","random"),effect="individual")
#из тестов становится понятно,что модель с фиксированными эффектами является наиболее хорошо описывающий нашу занятость


#Пример авторегрессионной модели
ar.model <- dynformula(log(emp)~log(wage)+log(capital)+
                         log(output), lag.form=list(1,0,0,0))
cigar.argmm <- pgmm(ar.model,data=EmplUK,index=c("firm","year"),
                    gmm.inst=~log(emp)+log(wage)+log(capital)+
                      log(output),
                    lag.gmm=c(2,10))
summary(cigar.argmm)
EmplUK$emp
print(sum((gas.fe$residuals)^2))
cigar.argmm$fitted.values
z=unlist(cigar.argmm$residuals)
z=exp(z)
sum(z^2)
c=cigar.argmm$coefficients
c
#Среди статистических моделей лучше всего себя продемонстрировала в сравнении с другими через критерии,
#модель с фиксированными эффектами.Но  сумма квадратов ошибок меньше у динамической модели больше чем в 2 раза.
#Поэтому следует выбрать ее.
#коэффициенты в нашей модели показывают,что наибольшее влияние на занятость влияет капитал и заработная плата,остальные регрессоры
#вляют не так сильно.