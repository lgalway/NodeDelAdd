rij <- function(mtrx,ndel) {
  tmean <- function(x) {
    if (length(x[!is.na(x)] ) == 0) NA else mean(x,na.rm=TRUE)}
 # browser()
  M <- dim(mtrx)[1]; N <- dim(mtrx)[2]
  mu <- mean(mtrx,na.rm=TRUE)
  aij <- matrix(apply(mtrx,1,tmean),byrow=FALSE,nrow=M,ncol=N)-mu
  bij <- matrix(apply(mtrx,2,tmean),byrow=TRUE,nrow=M,ncol=N)-mu
  # if (ndel > 83) browser()
  return(mtrx-aij-bij-mu)
}