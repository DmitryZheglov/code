library(jpeg)
#Метод K-средних в R Пусть X — матрица наблюдений  
# не связано с предыдущими рисунками 
source("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw4/func.R") 

# загрузка рисунка  
img <- readJPEG("C:/Users/Dmitriy/Desktop/proga/R/cmf/learn/hw4/IT_fellow.jpg")  
# создание цветовой матрицы 
X <- NULL 
for (i in 1:3) X <- cbind(X, as.vector(img[,,i])) 
head(X,4)  
km <- kmeans(X, 2, iter.max = 100, nstart = 10) 

X_new =NULL
X_new=rbind(km$centers[km$cluster,])
  # your code here  
# сохранение нового изображения 
img_new <- array(0, dim = dim(img)) 
for (i in 1:3) img_new[,,i] <- matrix(X_new[,i], nrow = dim(img)[1],ncol = dim(img)[2]) 
writeJPEG(img_new, "landscape_small_16col.jpeg", 1) 

