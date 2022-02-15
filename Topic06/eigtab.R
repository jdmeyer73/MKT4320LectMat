eigtab <- function(pcaobj) {
   eigtable <- data.frame(Component=integer(),
                          Eigenvalue=double(),
                          Difference=double(),
                          Proporation=double(),
                          Cumulative=double())
   len <- length(pcaobj$sdev)
   for (i in 1:len) {
      eigtable[i,1] <- i
      eigtable[i,2] <- pcaobj$sdev[i]^2
      if(i!=len) {
         eigtable[i,3] <-  pcaobj$sdev[i]^2 - pcaobj$sdev[i+1]^2
         } else {
            eigtable[i,3] <- NA
      }
      eigtable[i,4] <- (pcaobj$sdev[i]^2)/len
      if(i==1) {
         eigtable[i,5] <- (pcaobj$sdev[i]^2)/len
         } else {
            eigtable[i,5] <- (pcaobj$sdev[i]^2)/len + eigtable[i-1,5]
      }
   }
   eigtable <- cbind("Component"=eigtable[,1], round(eigtable[,2:5],4))
   eigtable
}

