sort.vertex <- function(simplex,fn) {
  # список из двух элементов: vertex и vertex.value 
  # vertex - матрица вершин симплекса, отсортированных по 
  # возрастанию значений функции на них 
  # vertex.value - вектор значений функций на вершинах, отсортированный 
  #по возрастанию 
  vertex=simplex
  vertex.val=numeric()
  for(i in 1:(length(simplex[,1]))){
    vertex.val[i]=fn(simplex[i,])
  }
  vertex.value=sort(vertex.val)
  for(i in 1:length(simplex[,1])){
    w=which.min(vertex.val)
    vertex[i,]=simplex[w,]
    vertex.val[w]=max(vertex.val)*10
  }
  list(vertex=vertex,value=vertex.value)
}  
z <- sort.vertex(init.simplex(x0),f) 
s <- z$vertex
vert.value <- z$value 
# параметры оптимизации 
d <- length(x0) 
nm.par <- c(1,(d+2)/d,(3*d-2)/(4*d),(d-1)/d) 
names(nm.par) <- c("alpha","beta","gamma","delta") 

val <- mean(vert.value) 
delta.val <- 10^30
delta <- 10^-12 


q=0
#Оптимизационный цикл 
while (delta.val>=delta){
  
  tmp <- val  
  # сохраняем предыдущее значение  
  # центроид наилучших вершин   
  x.bar <- apply(s[1:d,],2,mean)   
  # отражённая точка   
  x.r <- x.bar + nm.par["alpha"]*(x.bar-s[d+1,])   
  f.r <- f(x.r) 
  shrinked <- FALSE  
  # индикатор, было ли сокращение симплекса  
  # проверка условий 2. и 3.   
  if ((f.r>=vert.value[1]) & (f.r<vert.value[d])) {
    s[d+1,] <- x.r; f.new.vert <- f.r   
  }else{if (f.r<vert.value[1]){
    # продвинутая точка     
    x.e <- x.bar + nm.par["beta"]*(x.r-x.bar)     
    f.e <- f(x.e)     
    if (f.e<vert.value[1]) { 
      s[d+1,] <- x.e
      f.new.vert <- f.e     
    } else {       
      s[d+1,] <- x.r 
      f.new.vert <- f.r     
    } 
  }
  }
  # проверка условия 4. 
  if ((f.r>=vert.value[d]) & (f.r<vert.value[d+1])) { 
    # внешнее сжатие 
    x.oc <- x.bar + nm.par["gamma"]*(x.r-x.bar) 
    f.oc <- f(x.oc) 
    if (f.oc<=f.r) { 
      s[d+1,] <- x.oc; f.new.vert <- f.oc 
    } else { 
      # сокращение симплекса 
      for (i in 2:(d+1)) 
        s[i,] <- s[1,]+nm.par["delta"]*(s[i,]-s[1,]) 
      shrinked <- TRUE 
    } 
  } 
  # проверка условия 5. 
  if (f.r>=vert.value[d+1]) { 
    # внутреннее сжатие 
    x.ic <- x.bar - nm.par["gamma"]*(x.r-x.bar) 
    f.ic <- f(x.ic) 
    if (f.ic<vert.value[d+1]) { 
      s[d+1,] <- x.ic; f.new.vert <- f.ic 
    } else { 
      # сокращение симплекса 
      for (i in 2:(d+1)) 
        s[i,] <- s[1,]+nm.par["delta"]*(s[i,]-s[1,]) 
      shrinked <- TRUE 
    } 
  } 
  # переоценка значений функции на вершинах, пересортировка симплекса 
  if (shrinked) { 
    z <- sort.vertex(s,f) 
    s <- z$simplex; vert.value <- z$value 
  } else { 
    vert.value[d+1] <- f.new.vert 
    z <- order(vert.value) 
    s <- s[z,]; vert.value <- vert.value[z] 
  } 
  q=q+1
  if(q>1100){
    break()
  }
} 