mydend <- function(data, 
                   dist=c("euc", "euc2", "max", "abs", "bin"), 
                   method=c("ward", "single", "complete", "average"),
                   cut) {
   #
   # Create lookup for distance method and distance title
   #
   distdf <- data.frame(inp=c("euc", "euc2", "max", "abs", "bin"), 
                        outp=c("euclidean", "euclidean", "maximum", 
                               "manhattan", "binary"), 
                        dtitle=c("Euclidean", "Euclidean^2", "Maximum", 
                                 "Absolute", "Binary"))
   outdist <- distdf[,2:3]
   rownames(outdist) <- distdf[,1]
   dtype <- outdist[dist,1]
   dtitle <- outdist[dist,2]
   #
   # Create lookup for linkage method and linkage title
   #
   linkdf <- data.frame(inp=c("ward", "single", "complete", "average"), 
                        outp=c("ward.D", "single", "complete", "average"), 
                        ltitle=c("Ward's D", "Single", "Complete", "Average"))
   outlink <- linkdf[,2:3]
   rownames(outlink) <- linkdf[,1]
   ltype <- outlink[method,1]
   ltitle <- outlink[method,2]
   #
   # Create power for distance (needed for euc2)
   #
   if(dist=="euc2") {
      pw <- 2
   } else {
      pw <- 1
   }
   #
   # Create base hclust
   #
   hc <- hclust(dist(data, method=dtype)^pw, method=ltype)
   #
   # Create plot based on cut or not
   #
   if(missing(cut)) {
      dend <- as.dendrogram(hc)
      sub <- "All Branches"
      plot(dend, ylab="Similarity Measure",
           main=paste(dtitle,"Distance /",ltitle,"Linkage"),
           sub=sub)
   } else {
      memb <- cutree(hc, k=cut)
      cent <- NULL
      for(k in 1:cut) {
         cent <- rbind(cent, colMeans(data[memb==k, , drop=FALSE]))
      }
      hc <- hclust(dist(cent, method=dtype)^pw, method=ltype,
                   members=table(memb))
      dend <- as.dendrogram(hc)
      xdf <- data.frame(memb=hc$order)
      ydf <- data.frame(table(memb))
      zdf <- merge(xdf,ydf, by="memb", sort=FALSE)
      zdf$label <- paste("(n=",zdf$Freq,")")
      sub <- paste("Top",cut,"Branches")
      plot(dend, ylab="Similarity Measure",
           main=paste(dtitle,"Distance /",ltitle,"Linkage"),
           sub=sub)
      axis(1, at=hc$order, labels=zdf$label, las=2, lty=0)
   }
}