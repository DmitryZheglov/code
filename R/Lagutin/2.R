t1=c(24,43,40,49,35,44,22,28,42,47)
t2=c(24,54,53,30,71,35,32,43,27,61)
y1=t1/100
y2=t2/100







x=seq(-0.9,1,0.2)
#y1=c(1.14,0.98,0.96,0.78,0.56,0.57,0.46,0.35,0.17,-0.19)
#y2=c(1.30,1.08,0.95,0.72,0.71,0.71,0.23,0.25,0.37,-0.16)

plot(x,y1,"l")
lines(x,y2)
X=cbind(rep(1,10),x)
theta1=solve(t(X)%*%X)%*%t(X)%*%matrix(y1)
curve(theta1[1]+x*theta1[2],col="red",add=TRUE)
theta2=solve(t(X)%*%X)%*%t(X)%*%matrix(y2)
curve(theta2[1]+x*theta2[2],col="blue",add=TRUE)
#одно тета по всем точкам 
Xnew=cbind(rep(1,20),rep(x,each=2))
Ynew=c(y1[1],y2[1],y1[2],y2[2],y1[3],y2[3],y1[4],y2[4],y1[5],y2[5],y1[6],y2[6],y1[7],y2[7],y1[8],y2[8],y1[9],y2[9],y1[10],y2[10])
theta=solve(t(Xnew)%*%Xnew)%*%t(Xnew)%*%matrix(Ynew)
plot(x,y1,"l")
lines(x,y2)
curve(theta1[1]+x*theta[2],col="blue",add=TRUE)
#строим доверительные интервалы
D0=sum((Ynew-rep(eta_ch,each=2))^2)
D1=sum((Ynew-Xnew%*%theta)^2)
R=((D1-D0)/8)/(D0/(10))
qf(0.95,8,10)
p.value=1-pf(R,8,10)


sigma=D1/(18)
gr_upper=D1/qchisq(0.025,18)
gr_lower=D1/qchisq(0.975,18)

#ищем выброс
l=c(1,0.7)

granica_f=t(l)%*%theta+qt(0.975,18)*sqrt(D1/18)*sqrt(t(l)%*%solve(t(Xnew)%*%Xnew)%*%l    )


l=c(1,0)

granica_upper=t(l)%*%theta+qt(0.975,18)*sqrt(D1/18)*sqrt(t(l)%*%solve(t(Xnew)%*%Xnew)%*%l    )
granica_lower=t(l)%*%theta-qt(0.975,18)*sqrt(D1/18)*sqrt(t(l)%*%solve(t(Xnew)%*%Xnew)%*%l    )





#хотим отвергнуть адекватность альтернативы

Eta=cbind(y1,y2)
eta=c(y1,y2)
eta_ch=(y1+y2)/2

wrongX=cbind(rep(1,20),rep(sin(pi*x),each=2))
thetaWrong=solve(t(wrongX)%*%wrongX)%*%t(wrongX)%*%matrix(Ynew)
D0=sum((Ynew-rep(eta_ch,each=2))^2)
D1=sum((Ynew-wrongX%*%thetaWrong)^2)

R=((D1-D0)/8)/(D0/(10))
qf(0.95,8,10)
p.value=1-pf(R,8,10)

#ищем выброс dimonu
l=c(1,0.7)

granica_f=t(l)%*%thetaWrong+qt(0.975,18)*sqrt(D1/18)*sqrt(t(l)%*%solve(t(wrongX)%*%wrongX)%*%l    )


l=c(1,0)

granica_upper=t(l)%*%thetaWrong+qt(0.975,18)*sqrt(D1/18)*sqrt(t(l)%*%solve(t(wrongX)%*%wrongX)%*%l    )
granica_lower=t(l)%*%thetaWrong-qt(0.975,18)*sqrt(D1/18)*sqrt(t(l)%*%solve(t(wrongX)%*%wrongX)%*%l    )




