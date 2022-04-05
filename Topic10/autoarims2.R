autoarima2 <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h, arima, sarima) {
   require(fpp3)
   require(ggfortify)
   require(stringr)
   require(cowplot)
   p <- arima[1]
   d <- arima[2]
   q <- arima[3]
   P <- sarima[1]
   D <- sarima[2]
   Q <- sarima[3]
   fc <- h-1
   SelfDF <- p+q+P+Q
   if (datetype=="ym") {
      slag <- 24
      tsdata <- data %>% 
         mutate(Date=yearmonth(.data[[tvar]]),
                Measure=.data[[obs]]) %>% 
         as_tsibble(index=Date)
   } else if (datetype=="yq") {
      slag <- 12
      tsdata <- data %>%
         mutate(Date=yearquarter(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else if (datetype=="yw") {
      slag <- 26
      tsdata <- data %>%
         mutate(Date=yearweek(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else {
      stop("Time variable not in correct format")
   }
   train <- tsdata[1:(nrow(tsdata)-h), ]
   forecast <- tsdata[(nrow(tsdata)-fc):nrow(tsdata), ]
   train_fit <- train %>% 
      fabletools::model(Self=ARIMA(Measure~pdq(p,d,q)+PDQ(P,D,Q)),
                        Auto=ARIMA(Measure, stepwise=FALSE, approx=TRUE))
   train_fc <- train_fit %>% fabletools::forecast(h=h)
   modnames <- train_fit %>% 
      pivot_longer(everything(), names_to = ".model",
                   values_to="Order") %>% 
      mutate(ARIMA=format(Order)) %>%
      data.frame() %>%
      select(.model,ARIMA) %>%
      mutate(ARIMA=str_replace(ARIMA," .*", ""),
             ARIMA=str_replace(ARIMA,"<",""),
             ARIMA=str_replace(ARIMA,"ARIMA","A"),
             ARIMA=str_replace(ARIMA,"\\)\\(", "\\)S\\("),
             ARIMA=str_replace(ARIMA,">",""))
   acc <- fabletools::accuracy(train_fc, forecast) %>%
      inner_join(glance(train_fit), by=".model") %>%
      inner_join(modnames, by=".model") %>%
      mutate(".model"=paste0(.model, ": ", ARIMA)) %>%
      select(.model, RMSE, MAE, MAPE, AIC) %>%
      rename(Model=1) %>% 
      mutate_at(2:5, round, 3) %>% 
      data.frame()
   AM <- modnames[2,2]
   if (grepl("S",AM)) {
      AutoDF <- as.integer(substr(AM,3,3)) + as.integer(substr(AM,7,7)) +
         as.integer(substr(AM,11,11)) + as.integer(substr(AM,15,15))
   } else {
      AutoDF <- as.integer(substr(AM,3,3)) + as.integer(substr(AM,7,7))
   }
   Self.Resid <- train_fit[1] %>% residuals()
   Auto.Resid <- train_fit[2] %>% residuals()
   Self.bt <- Box.test(Self.Resid$.resid, lag=slag, type="Ljung-Box", fitdf=SelfDF)
   Auto.bt <- Box.test(Auto.Resid$.resid, lag=slag, type="Ljung-Box", fitdf=AutoDF)
   Self.cp <- ggcpgram(Self.Resid$.resid)+
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.caption=element_text(hjust=0, size=14)) +
      labs(caption=paste0("Ljung-Box test: p=",round(Self.bt$p.value,4)),
           title=paste0(modnames[1,1],": ",modnames[1,2]))
   Auto.cp <- ggcpgram(Auto.Resid$.resid)+
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.caption=element_text(hjust=0, size=14)) +
      labs(caption=paste0("Ljung-Box test: p=",round(Auto.bt$p.value,4)),
           title=paste0(modnames[2,1],": ",modnames[2,2]))
   wn <- plot_grid(Self.cp, Auto.cp, ncol=2)
   fcresid <- train_fc %>% 
      mutate(actual=rep(forecast$Measure,2),
             resid=actual-.mean) %>% 
      inner_join(modnames, by=".model") %>%
      mutate(.model=paste0(.model, ": ", ARIMA)) %>%
      select(-ARIMA)
   fcresplot <- fcresid %>% ggplot(aes(x=Date, y=resid, color=.model)) +
      geom_line(size=1) +
      geom_hline(yintercept = 0, color="black", linetype="dashed", size=2) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   plot <- train_fc %>% autoplot(train, level=NULL, size=1) + 
      autolayer(forecast, .data[[obs]]) +
      labs(y=obs) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   results <- list("plot"=plot, "acc"=acc, 
                   "fcresid"=fcresid, "fcresplot"=fcresplot,
                   "wn"=wn)
   return(results)
}