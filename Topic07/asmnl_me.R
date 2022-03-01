asmnl_me <- function(mod) {
   mlen <- length(mod$model) - 3
   mlen2 <- mlen + 1
   mdata <- mod$model[,1:mlen2]
   zt <- 0
   zp <- 0
   enames <- NULL
   mnames <- names(mod$model)[2:mlen]
   for (i in 2:mlen) {
      if (class(mod$model[[i]])[3]=="factor") {
         if (length(levels(mod$model[[i]]))==2) {
            x <- data.frame(tapply(as.numeric(mod$model[[i]]), idx(mod,2), mean))
         } else {
            err1 <- paste("Variable", names(mod$model)[i], "has more than two levels.")
            err2 <- paste("Please use dummy variable coding instead of factor coding")
            err3 <- paste("for marginal effects.")
            err <- paste0(err1,err2,err3,sep="\n")
            return(err)
         }
      } else {
         x <- data.frame(tapply(mod$model[[i]], idx(mod,2), mean))
         zp <- data.frame(zp,x)
         enames <- cbind(enames,names(mod$model)[i])
      }
      zt <- data.frame(zt,x)
   }
   zt <- zt[,-1]
   zp <- zp[,-1]
   colnames(zt) <- mnames
   colnames(zp) <- enames
   
   for (i in 1:dim(zp)[1]) {
      for (j in 1:dim(zp)[2]) {
         mdata[idx(mdata,2)==rownames(zp)[i], colnames(zp)[j]] <- zp[i,j]
      }
   }
   pprob <- round(apply(predict(mod, newdata=mdata), 2, mean),4)
   outtitle <- paste0("\n",
                        "--------------------------------","\n",
                        "Predicted Probabilities at Means","\n",
                        "--------------------------------","\n")
   cat(outtitle)
   print(pprob)
   
   for (var in enames) {
      dash <- paste(rep("-",nchar(var)), collapse="")
      sep <- paste0("---------------------",dash)
      outme <- paste0("Marginal effects for ", var)
      cat("\n",sep,"\n",outme,"\n",sep,"\n")
      print(round(effects(mod, covariate=var, type="aa", data=zt),5))
   }
}