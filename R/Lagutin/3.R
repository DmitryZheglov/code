x=read.table("C:/Users/user/Desktop/proga/R/Lagutin/9/1.txt",header = FALSE)
x=unlist(x)
K<- function(x){
n<-length(x)
N=7
q<-qchisq(0.95,N-1)
#Krit Hi 2
s=c(0,1/7,2/7,3/7,4/7,5/7,6/7,1)
yash=vector("integer",N)
for(i in 1:(n)) {
  if((x[i]>=s[1])&(x[i]<=s[2])) {
    yash[1]=yash[1]+1
  }
  if((x[i]>=s[2])&(x[i]<=s[3])) {
    yash[2]=yash[2]+1
  }
  if((x[i]>=s[3])&(x[i]<=s[4])) {
    yash[3]=yash[3]+1
  }
  if((x[i]>=s[4])&(x[i]<=s[5])) {
    yash[4]=yash[4]+1
  }
  if((x[i]>=s[5])&(x[i]<=s[6])) {
    yash[5]=yash[5]+1
  }
  if((x[i]>=s[6])&(x[i]<=s[7])) {
    yash[6]=yash[6]+1
  }
  if((x[i]>=s[7])&(x[i]<=s[8])) {
    yash[7]=yash[7]+1
  }
}
p0=c(1/7,1/7,1/7,1/7,1/7,1/7,1/7)
T<-sum((((yash-(n*p0))^2)/(n*p0)))
print(T)
print(q)
if(T>q){
  print("False")
}else{
  print("TRUE")
}
}

K(x)
