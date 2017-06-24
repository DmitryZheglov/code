x=read.table("C:/Users/user/Desktop/proga/R/Lagutin/9/1.txt")
x35=read.table("C:/Users/user/Desktop/proga/R/Lagutin/9/35.txt")
x=unlist(x)
x35=unlist(x35)
for( i in 1:100){
  if (x[i]<=0.2){
    x[i]=0
  }
}
for( i in 1:100){
  if (x35[i]<=0){
    x35[i]=0
  }
}

i1=-log(1-x35)
i2=-(log(1-x)-0.2)
ros.test=function(x,y) {
  n=length(x)
  m=length(y)
  r=sort(rank(c(x,y))[1:n])
  rr=seq(1,n,1)
  s=sort(rank(c(y,x))[1:m])
  ss=seq(1,m,1)
  S=n*m/(n+m)*(1/(n*m)*(1/6+1/m*sum((r-rr)^2)+1/n*sum((s-ss)^2))-2/3)
  p.value=1-pCvM(S)
  return(S)
}


y=t(read.table("sasha.txt"))
x=sqrt(y)
alt=t(read.table("35.txt"))


plot(seq(0.01,1,0.01),y,"l")
#mann uitni test
((sum(rank(c(alt,x))[1:100])-50*101)-50*100)/sqrt(100*100*201/12)

#student test
sqrt(50)*(mean(x)-mean(alt))/sqrt((99*sd(alt)^2+99*sd(x)^2)/198)
qt(0.975,198)

2-2*pnorm(0.7892146)

ks.test(alt,x,alternative = "g")
plot(Vectorize(ecdf(x)))
curve(ecdf(alt)(x),0,1,add=TRUE)
KriteriySmirnov(alt,x)
################################11.4
y=t(read.table("my.txt"))
x=log(y/(1-y))

mean(x)
median(x)
vec=vector("numeric")
for(i in 1:100) {
  for(j in i:100) {
    vec=c(vec,(x[i]+x[j])/2)
  }
}
median(vec)


################################12.5
y=t(read.table("my.txt"))

delta=seq(0,1,1/7)
num=vector("numeric",7)
for(j in 1:7) {
  num[j]=sum((y>delta[j])*(y<=delta[j+1]))
}

chisq.test(num,p=rep(1/7,7))

###############################11.5
y=t(read.table("sasha.txt"))
x=log(1/(1-y))
x=x-median(x)
f=function(z) return(ecdf(x)(z))
R=sum((1-f(x)-f(-x))^2)
curve(f(x))
