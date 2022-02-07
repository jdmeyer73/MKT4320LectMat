cldescrold <- function(data, var, vtype=c("F", "C"), cvar) {
   if(vtype=="F") {
      var <- eval(substitute(var), data)
      cvar <- eval(substitute(cvar), data)
      fdata <- data.frame(model.matrix(~var-1, data=data))
      len <- length(fdata)
      names(fdata) <- substring(names(fdata), 4)
      cnames <- colnames(fdata)
      fdata <- cbind(fdata,Cluster=with(data, {cvar}))
      if(len==2) {
         means <- aggregate(.~Cluster, fdata, 
                            FUN=function(x) round(mean(x), digits=4))[,1:2]
         fdata <- fdata[,-2]
         aov <- aov(fdata[[1]]~Cluster, data=fdata)
         p <- summary(aov)[[1]][["Pr(>F)"]][1]
         aovp <- data.frame(Variable=colnames(fdata)[1], p.value=round(p,4))
         if(p<.1) {
            tukey <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
         } else {
            tukey <- NULL
         }
      } else {
         tukey <- list()
         means <- aggregate(.~Cluster, fdata, 
                            FUN=function(x) round(mean(x), digits=4))
         for (i in 1:len) {
            name <- cnames[i]
            aov <- aov(fdata[[i]]~Cluster, data=fdata)
            p <- summary(aov)[[1]][["Pr(>F)"]][1]
            aovp[i,] <- data.frame(Variable=colnames(fdata)[[i]],
                                     p.value=round(p,4))
            if(p<.1) {
               tukey[[name]] <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
            } else {
               tukey[[name]] <- NULL
            }
         }
      }
   }
   if(vtype=="C") {
      var <- deparse(substitute(var))
      cvar <- deparse(substitute(cvar))
      cnames <- cbind(var,"Cluster")
      fdata <- data.frame(data[[var]], data[[cvar]])
      names(fdata) <- cnames
      means <- aggregate(.~Cluster, fdata, 
                         FUN=function(x) round(mean(x), digits=4))
      aov <- aov(fdata[[1]]~Cluster, data=fdata)
      p <- summary(aov)[[1]][["Pr(>F)"]][1]
      aovp <- data.frame(Variable=colnames(fdata)[1], p.value=round(p,4))
      if(p<.1) {
         tukey <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
      } else {
         tukey <- NULL
      }
   }
   return=list("means"=means, "aovp"=aovp, "tukey"=tukey)
}



