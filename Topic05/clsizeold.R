clsizeold <- function(data, 
                   dist=c("euc", "euc2", "max", "abs", "bin"), 
                   method=c("ward", "single", "complete", "average"),
                   cuts=1) {
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
   # Create table
   #
   if(length(cuts)>1) {
      x <- max(cuts)
      df <- data.frame(table(cutree(hc,x)))
      df$p <- round(df$Freq/nrow(data),4)
      names(df) <- c("Cluster",paste0("C.",x,".N"), paste0("C.",x,".Pr"))
      cuts <- sort(cuts[cuts<x], decreasing=TRUE)
      for(value in cuts) {
         temp <- data.frame(table(cutree(hc,value)))
         temp$p <- round(temp$Freq/nrow(data),4)
         names(temp) <- c("Cluster",paste0("C.",value,".N"), paste0("C.",value,".Pr"))
         df <- merge(df,temp, by="Cluster", all=TRUE)
         }
   } else {
      df <- data.frame(table(cutree(hc,cuts)))
      df$p <- round(df$Freq/nrow(data),4)
      names(df) <- c("Cluster",paste0("C.",cuts,".N"), paste0("C.",cuts,".Pr"))
      }
   df
}