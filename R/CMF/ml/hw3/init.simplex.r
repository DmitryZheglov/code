init.simplex <- function(x0) { 
  m=matrix(0,nrow=length(x0)+1,ncol=length(x0)) 
  for (i in 1:(length(x0)+1)) m[i,]=x0 
  
  for (i in 2:(length(x0)+1)) { 
    if (x0[i-1]==0) m[i,i-1]=2.5*10^(-4) 
    else m[i,i-1]=m[i,i-1]+5*10^(-2) 
  } 
  m 
}