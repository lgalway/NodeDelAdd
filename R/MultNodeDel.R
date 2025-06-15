MultNodeDel <- function(mtrx,dlta,tau) {
  # Deletes multiple nodes in a single pass, Reduces recalculation of parameters. 
  # For larger data sets (n>100)
  # L Galway June 14, 2025

  tmean <- function(x) {
    # Utility function to deal witcols/rows that are are all NA (deleted)
    if (length(x[!is.na(x)] ) == 0) NA else mean(x,na.rm=TRUE)}
  
  M <- dim(mtrx)[1]; N <- dim(mtrx)[2]
  mtrxT <- mtrx #Working matrix
  resT <- rij(mtrxT)
  cohr <- mean(resT^2,na.rm=TRUE)
  while (cohr >= dlta) {
    rdel <- mrcdel(resT,1); mtrxT[!(is.na(rdel)) & rdel > (tau * cohr),] <- NA
    cohr <- mean(resT^2,na.rm=TRUE)
    resT <- rij(mtrxT,ndel)
    cdel <- mrcdel(resT,2); mtrxT[,!(is.na(cdel)) & cdel > (tau * cohr)] <- NA
    resT <- rij(mtrxT,ndel)
    cohr <- mean(resT^2,na.rm=TRUE)    
  }
  return(mtrxT)
}

mrcdel <- function(mtrx,rw=1,tc) {
  tmp2 <- mtrx^2
  resx <- apply(tmp2,rw,mean,na.rm=TRUE)
  return(resx)
}