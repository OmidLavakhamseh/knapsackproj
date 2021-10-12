#' Implements Greedy ALgorithm for knapsack problem
#' @param x data.frame, contains variables weight(w) and value(v)
#' @param W weight threshold for the knapsack
#' @return Total v and position of w's for that value
#' @seealso
#' \code{\link{brute_force_knapsack}}
#' \code{\link{knapsack_dynamic}}



greedy_knapsack <- function(x, W){
  c1=c()
  c=c()
  if (inherits(x,"data.frame")==FALSE){
    stop()
  }
  if(W<=0){
    stop()
  }
  v=v1 = 0
  vrat = x$v/x$w
  numrow<- nrow(x) 
  x$OI= 1:numrow
  vd=vd1 = 0
  x=x[order(vrat, decreasing=TRUE),]
  i=1
  while((v1<W)|(v1==W)){
    v=v1
    vd=vd1
    c=c1
    v1=v1+x$w[i]
    vd1=vd1+x$v[i]
    c1=c(c1,i)
    i=i+1
  }

  greedylist=list(value=round(vd),elements=x$OI[c])


  return(greedylist)

}


