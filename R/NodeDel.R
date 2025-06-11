NodeDel <- function(mtrx,dlta) {
  tmean <- function(x) {
    if (length(x[!is.na(x)] ) == 0) NA else mean(x,na.rm=TRUE)}
  M <- dim(mtrx)[1]; N <- dim(mtrx)[2]
  mtrxT <- mtrx
  #browser()
  ndel <- 0
  resT <- rij(mtrxT,ndel)
  cohr <- mean(resT^2,na.rm=TRUE)
  while (cohr >= dlta) {
   # browser()
    rdel <- rcdel(resT,1);cdel <-rcdel(resT,2)
    if (rdel[[2]] >= cdel[[2]]) mtrxT[rdel[[1]],] <- NA else mtrxT[,cdel[[1]]] <- NA
    resT <- rij(mtrxT,ndel)
    cohr <- mean(resT^2,na.rm=TRUE)    
    ndel <- ndel + 1
  # if(ndel > 83) browser()
    # print(sprintf("%3d %5.2f",ndel,cohr))
  }
  return(mtrxT)
}