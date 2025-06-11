rcdel <- function(mtrx,rw=1) {
  #browser()
  tmp2 <- mtrx^2
  resx <- apply(tmp2,rw,mean,na.rm=TRUE)
  resx[is.na(resx)] <- -1
  resi <- which.max(resx)
  resmx <- resx[resi]
  return(list(resi,resmx))
}