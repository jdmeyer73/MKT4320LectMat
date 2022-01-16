or_table <- function(MOD, DIGITS=4, LEVEL=95) {
   vals <- round(data.frame(exp(cbind(OR=coef(MOD),
                        confint(profile(MOD), 
                                level=LEVEL/100)))), DIGITS)
   vals$parameter <- rownames(vals)
   vals <- vals[, c(4,1,2,3)]
   lwr <- (100-LEVEL)/2
   upr <- (100+LEVEL)/2
   clnames <- c("Parameter", "OR Est", paste0(lwr,"%"),paste0(upr,"%"))
   colnames(vals) <- clnames
   return(vals)
}
