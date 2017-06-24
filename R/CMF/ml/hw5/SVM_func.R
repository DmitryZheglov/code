fitStats <- function(y,y.pred) {
  true.pos=0
  false.pos=0
  false.neg=0
  true.neg=0
  l=length(y)
  for(j in 1:l){
    if(y[j]==1&y.pred[j]==1){
      true.pos=true.pos+1
    }else if(y[j]==1&y.pred[j]==0){
      false.neg=false.neg+1
    }else if(y[j]==0&y.pred[j]==0){
      true.neg=true.neg+1
    }else{
      false.pos =false.pos+1
    }
  }
  accuracy=(true.neg+true.pos)/l
 
  precision=true.pos/(true.pos+false.pos)
  recall=true.pos/(true.pos+false.neg)
  if (precision + recall == 0){
    f1.score <- 0
  } else{
    f1.score=2*(precision*recall)/(precision+recall)
  }
  stat <- c(accuracy,precision,recall,f1.score)
  names(stat) <- c("accuracy","precision","recall","f1.score")
  stat
  
}
