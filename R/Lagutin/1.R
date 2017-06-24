t1=c(24,43,40,49,35,44,22,28,42,47)
t2=c(24,54,53,30,71,35,32,43,27,61)
t1=t1/100
t2=t2/100
xi=c()
for (i in 1:10){
  xi[i]=-1-1/10+i/5
}
t=c(t1,t2)
x=c(xi,xi)
plot(x,t)
g=mean(t)
s=sqrt(var(t))
f=function(x,theta){
  sum(theta*x)
}
loss=function(x,theta,y){
  sum((y-f(x,theta))^2)
}
fit.par <- lm(t ~ x)
print(fit.par$coefficients)
sum((t-0.402-x*0.020 )^2)
fit.par1 <- lm(t ~ sin(pi*x))
sum(fit.par1$residuals)
print(fit.par1$coefficients)

print(c(g-2*s,g+2*s))

