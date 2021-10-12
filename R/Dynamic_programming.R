#' Implements Dynamic programming for knapsack problem
#' @param x data.frame, contains variables weight(w) and value(v)
#' @param W Knapsack capacity
#' @return Total and Maximum value within the max capacity
#' @export

knapsack_dynamic<-function(x,W){
  if (is.data.frame(x) == FALSE & is.numeric(W) == FALSE){
    stop()
  }
  if(W<=0){
    stop()
  }

  w<-x$w
  v<-x$v
  n<-nrow(x)
  m<- matrix(nrow=nrow(x)+1,ncol=W+1)
  m[1,]<-0
  for (i in 1:n){
    for (j in 0 :W){
      if (w[i]>j ){
        m[i+1,j+1]<-m[i,j+1]
      }else{
        m[i+1,j+1]<-max(m[i,j+1],m[i,j+1-w[i]]+v[i])
      }
    }
  }
  i <- (n+1)
  value <- max(m[i,])
  element <- c()
  j <- 1
  while(i > 1) {
    if(!value %in% m[i-1,]){ 
      element[j] <- i-1
      j <- j + 1
      i <- i - 1
      value <- value - x$v[i]
    } else{ 
      i <- i - 1
    }

  }
  
  result<-list(value=round(m[n+1,W+1]),
               elements=as.numeric(sort(element)))
  return(result)
}
