#HomeWorkOneCMF

#TaskOne:download dataframe from finam
datMTLR= read.csv("C:/Users/Dmitriy/Desktop/R/cmf/MTLR.csv",header=TRUE,sep=",")
datYNDX= read.csv("C:/Users/Dmitriy/Desktop/R/cmf/YNDX.csv",header=TRUE,sep=",")
datRASP= read.csv("C:/Users/Dmitriy/Desktop/R/cmf/RASP.csv",header=TRUE,sep=",")

#TaskTwo:transfer to rate
RateOfReturnMTLR=unlist((datMTLR[8]-datMTLR[5])/datMTLR[5])
RateOfReturnYNDX=unlist((datYNDX[8]-datYNDX[5])/datYNDX[5])
RateOfReturnRASP=unlist((datRASP[8]-datRASP[5])/datRASP[5])

#transfer price xlose from dataframe
PriceCloseMTLR=unlist(datMTLR[8])
PriceCloseYNDX=unlist(datYNDX[8])
PriceCloseRASP=unlist(datRASP[8])

#build grafic(click Zoom)

plot(PriceCloseMTLR,type = "l",col = "red")
plot(RateOfReturnMTLR,type = "l",col = "blue")
plot(PriceCloseYNDX,type = "l",col = "red")
plot(RateOfReturnYNDX,type = "l",col = "blue")
plot(PriceCloseRASP,type = "l",col = "red")
plot(RateOfReturnRASP,type = "l",col = "blue")


#Make function for mean and variation by vector
f= function(x){
print(mean(x))
print(sd(x))
}

#Calculating function
f(RateOfReturnMTLR)
f(RateOfReturnYNDX)
f(RateOfReturnRASP)
