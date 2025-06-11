bmat <- function(rwn,rw1,cw2,p1,p2) {
 # browser()
  t1 <- matrix(rbinom(rwn^2,1,p1),nrow=rwn,ncol=rwn)
  t1[rw1,cw2] <- rbinom(length(rw1)*length(cw2),1,p2)
  return(t1)
}