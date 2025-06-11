nmat <- function(rwn,mn1,sd1,rw1,cw2,mn2,sd2) {
  #browser()
  t1 <- matrix(rnorm(rwn^2,mn1,sd1),nrow=rwn,ncol=rwn)
  t1[rw1,cw2] <- rnorm(length(rw1)*length(cw2),mn2,sd2)
  return(t1)
}