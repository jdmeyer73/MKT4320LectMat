clsize <- function(data, 
                   dist=c("euc", "euc2", "max", "abs", "bin"), 
                   method=c("ward", "single", "complete", "average"),
                   cuts=2) {
   require(dendextend)
   require(dplyr)
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
   # Create base hclust and dendrogram object
   #
   hc <- hclust(dist(data, method=dtype)^pw, method=ltype)
   hcd <- as.dendrogram(hc)
   #
   # Create tables
   #
   kc_table <- lapply(cuts, 
                      function(i) data.frame(table(cutree(hcd, k=i))))
   k_count <- Reduce(function(d1, d2) merge(d1, d2, 
                                            by="Var1", all=TRUE), 
                     kc_table)
   if (length(cuts)>1) {
      cnc <- sapply(cuts, function(i) paste0("k_", cuts, "_Count"))[,1]
   } else {
      cnc <- paste0("k_", cuts, "_Count")
   }
   colnames(k_count) <- c("Num_Clusters", cnc)
   k_count
   
   kp_table <- lapply(cuts, 
                      function(i) data.frame(round(100*prop.table(table(cutree(hcd, k=i))),2)))
   k_perc <- Reduce(function(d1, d2) merge(d1, d2, 
                                           by="Var1", all=TRUE), 
                    kp_table)
   if (length(cuts)>1) {
      cnp <- sapply(cuts, function(i) paste0("k_", cuts, "_Percent"))[,1]
   } else {
      cnp <- paste0("k_", cuts, "_Percent")
   }
   colnames(k_perc) <- c("Num_Clusters", cnp)
   k_perc
   #
   # Create dendrogram with bars
   #
   cuts_m <- max(cuts)
   the_bars <- sapply(cuts, 
                      function(i) cutree(hcd, k=i, 
                                         order_clusters_as_data = FALSE))
   if (length(cuts)>1) {
      cn <- sapply(cuts, function(i) paste0("k_",cuts))[,1]
   } else {
      cn <- paste0("k_", cuts)
   }
   colnames(the_bars) <- cn
   hcd %>% 
      set("branches_k_color", k=cuts_m) %>%
      set("branches_lwd", 4) %>%
      set("labels_colors","white") %>%
      plot(ylim=c(hcd_h[cuts_m], hcd_h[1]))
   colored_bars(colors=the_bars, dend=hcd, sort_by_labels_order = FALSE)
   results <- list("kcount"=k_count, "kperc"=k_perc)
   return(results)
 }