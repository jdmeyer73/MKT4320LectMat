library(ggplot2)       # Advanced graphing capabilities
library(tidyr)         # Easier programming   
library(scales)        # Control appearance of axes and legend labels
library(htmlTable)     # Better HTML Tables
library(reshape2)      # Easily convert wide data to long data
library(GGally)        # ggplot extension; for scatterplot matrix
library(summarytools)  # Summary statistics
library(effects)       # Help with linear predictions
library(cowplot)       # Arrange separate plots in a grid
library(ggtext)        # Annotate ggplots
library(lubridate)     # Easily work with dates
library(jtools)        # Concise regression results
library(dplyr)         # Easier programming
load("Topic02/advtsales.rdata")
set.seed(123)
temp <- advtsales %>% sample_n(30)
onepoint <- subset(temp, id==195) %>% mutate(labvar="$$x_i$$")
label =  "x<sub>i</sub>,y<sub>i<sub>"
ggplot(temp, aes(x=ad_tv, y=sales)) + 
   geom_point(size=3, color="brown") +
   geom_smooth(method="lm", se=FALSE, color="midnightblue") +
   geom_point(data=onepoint, color="orange", size=3) +
   geom_richtext(data=onepoint, mapping=aes(x=ad_tv, y=sales, label=labvar, vjust=-.1), fill=NA, label.color=NA)+
   annotate("text", x=150, y=20, label=paste("list(x[i] ==", 0.151, ", p==.226)"), parse=TRUE)

set.seed(123)
advtsales %>% 
   sample_n(30) %>%
   ggplot(aes(x=ad_tv, y=sales)) +
   # Requests scatter plot
   geom_point(size=3, color="brown") +
   # Requests linear regression fitted line without confidence interval
   geom_smooth(method="lm", se=FALSE, color="midnightblue") +
   # Changes text size to be larger
   theme(text=element_text(size=15)) +
   # Adds axis labels
   labs(x="Television Advertising (ad_tv)",
        y="Sales (sales)")

data.frame(X=(0:20)) %>% 
   mutate(Y=-(X^2)+20*X) %>% 
   ggplot(aes(x=X, y=Y)) +
   geom_point(size=3, color="brown") +
   geom_smooth(method="lm", se=FALSE, color="midnightblue") +
   theme(text=element_text(size=15))

spacktable <- data.frame((Package=c("R", "Stata GUI", "Stata Command", "SPSS GUI", "SPSS Syntax", "SAS", "Minitab"), Method=c("lm(dv ~ iv1 + iv2 + ... + ivk", "Statistics > Linear models and related > Linear Regression", "regress dv iv1 iv2 ... ivk", "Analyze > Regression > Linear", "regression/dependent dv/enter iv1 iv2 ... ivk", "proc reg; model dv = iv1 iv2 ... ivk;","Stat > Regression > Regression")))
Package <- c("R", "Stata GUI", "Stata Command", "SPSS GUI", "SPSS Syntax", "SAS", "Minitab")
Method <- c("lm(dv ~ iv1 + iv2 + ... + ivk", "Statistics > Linear models and related > Linear Regression", "regress dv iv1 iv2 ... ivk", "Analyze > Regression > Linear", "regression/dependent dv/enter iv1 iv2 ... ivk", "proc reg; model dv = iv1 iv2 ... ivk;","Stat > Regression > Regression")
spacktable <- data.frame(Package,Method)
htmlTable(spacktable, rowlabel=NULL)
mat1data <- c("R", "Stata GUI", "Stata Command", "SPSS GUI", "SPSS Syntax", "SAS", "Minitab","lm(dv ~ iv1 + iv2 + ... + ivk", "Statistics > Linear models and related > Linear Regression", "regress dv iv1 iv2 ... ivk", "Analyze > Regression > Linear", "regression/dependent dv/enter iv1 iv2 ... ivk", "proc reg; model dv = iv1 iv2 ... ivk;","Stat > Regression > Regression")
mat1 <- matrix(mat1data, nrow=7, ncol=2, byrow=FALSE)
mat1 %>% addHtmlTableStyle(align="ll") %>% htmlTable()
mat2data <- c("font-family: Arial",
              "font-family: Arial",
              "font-family: Arial",
              "font-family: Arial",
              "font-family: Arial",
              "font-family: Arial",
              "font-family: Arial",
              "font-family: Courier",
              "font-family: Arial",
              "font-family: Courier",
              "font-family: Arial",
              "font-family: Courier",
              "font-family: Courier",
              "font-family: Arial")
mat2 <- matrix(mat2data, nrow=7, ncol=2, byrow=FALSE)
mat1 %>% addHtmlTableStyle(align="ll", css.cell=mat2) %>% htmlTable()

load("Topic02/advtsales.rdata")
lrreg1 <- advtsales %>% select(-id)
htmlTable(txtRound(favstats(lrreg1$ad_tv),2), 
          caption="Table 1.3A: Measures of Centrality and Dispersion for 'Age'")
stats <- c("n.valid", "mean", "sd", "min", "max")
# Use package summarytools to easily create summary statistics table
# Note: summarytools::descr not available in virtual environment
# Request htmlTable for summary statistics with rounding two 2 digits
htmlTable(txtRound(descr(lrreg1, stats=stats, transpose=TRUE),2))
lrreg1 %>% lm(sales~ad_tv+ad_radio+ad_paper)


library(reshape2)
meltdata <- melt(lrreg1)

melt(lrreg1) %>% 
   ggplot(aes(factor(variable), value)) +
   geom_boxplot() +
   # Adds the whiskers to the boxplot
   stat_boxplot(geom='errorbar') +
   # Changes text size to be larger
   facet_wrap(~variable, scale="free") +
   labs(x="", y="")

pairs(lrreg1, pch=19, upper.panel=NULL)

results <- lm(sales~ad_tv+ad_radio+ad_paper, data=advtsales)
glance(summary(results))

setHtmlTableTheme("Google", align="lr", css.table = "width: 50%;")
htmlTable(txtRound(
      t(glance(summary(results))),
      digits=4, 
      scientific=FALSE),
      caption="Table 2.2: Tidy Summary Table")

setHtmlTableTheme("Google", align="lrrrr")
htmlTable(txtRound(
   tidy(summary(results)),
   digits=4, 
   scientific=FALSE),
   rnames=FALSE,
   caption="Table 2.3: Tidy Coefficients Table")

results_std <- lm(scale(sales)~scale(ad_tv)+scale(ad_radio)+scale(ad_paper) -1, data=advtsales)
stdbeta <- results_std$coefficients
htmlTable(t(stdbeta))

stdbeta <- function (x) 
{
   b <- summary(x)$coef[-1, 1]
   sx <- sapply(x$model[-1], sd)
   sy <- sapply(x$model[1], sd)
   beta <- b * sx/sy
   return(beta)
}

htmlTable(txtRound(lm.beta(results), 4, scientific=FALSE),
          caption="Table 2.4: Standardized Beta Coefficients")

dat2 %>%
   ggplot(aes(x=ad_tv, y=fit)) + geom_line(color="red") + geom_ribbon(aes(ymin=lower, ymax=upper), fill="red", alpha=0.1)

# Create dataframe for ad_tv prediction using 'effects' package
lp.adtv <- as.data.frame(predictorEffects(results)$ad_tv)
# Create plot and save as object 'p1'
p1 <- 
   # Begins plot with 'ad_tv' on x axis and 'fit' on y axis
   ggplot(aes(x=ad_tv, y=fit), data=lp.adtv) +
      # Draws line based on predicted points
      geom_line(color="darkred") +
      # Adds confidence interaval bands around line
      geom_ribbon(aes(ymin=lower, ymax=upper), fill="red", alpha=0.2) +
      # Next two commands scale x and y axes
      scale_x_continuous(limits=c(0,300), expand=c(.025,.025), 
                         breaks=seq(0,300,50), minor_breaks=NULL) +
      scale_y_continuous(limits=c(5,30), expand=c(.025,.025), 
                         breaks=seq(5,30,5), minor_breaks=NULL) +
      # Labels axes
      labs(x="TV Advertising (ad_tv)", y="Linear Prediction")

# Repeat for other two variables
lp.adrad <- as.data.frame(predictorEffects(results)$ad_radio)
p2 <- ggplot(aes(x=ad_radio, y=fit), data=lp.adrad) +
         geom_line(color="darkgreen", size=1) +
         geom_ribbon(aes(ymin=lower, ymax=upper), fill="green", alpha=0.2) +
         scale_x_continuous(limits=c(0,50), expand=c(.025,.025), 
                            breaks=seq(0,50,10), minor_breaks=NULL) +
         scale_y_continuous(limits=c(5,30), expand=c(.025,.025), 
                            breaks=seq(5,30,5), minor_breaks=NULL) +
         labs(x="Radio Advertising (ad_radio)", y="Linear Prediction")

lp.adpap <- as.data.frame(predictorEffects(results)$ad_paper)
p3 <- ggplot(aes(x=ad_paper, y=fit), data=lp.adpap) +
         geom_line(color="darkorange", size=1) +
         geom_ribbon(aes(ymin=lower, ymax=upper), fill="orange", alpha=0.2) +
         scale_x_continuous(limits=c(0,115), expand=c(.025,.025), 
                            breaks=seq(0,115,20), minor_breaks=NULL) +
         scale_y_continuous(limits=c(5,30), expand=c(.025,.025), 
                            breaks=seq(5,30,5), minor_breaks=NULL) +
         labs(x="Newspaper Advertising (ad_paper)", y="Linear Prediction")

# Arrange three plots in a grid using package 'cowplot'
plot_grid(p1,p2,p3)

# Create new data for prediction with 'ad_tv' as focus
ad.tv.pred <- crossing(ad_tv=seq(0,300,30), # 11 levels
                       ad_radio=seq(0,50,10)) # 6 levels
# Append linear prediction and prediction intervals to new data
ad.tv.pred$pred <- as.data.frame(
                     predict.lm(resultssig,   # Model to use for prediction
                                ad.tv.pred,   # Data set to predict on
                                interval="confidence"))  # Confidence intervals
# Create plot and save as object 'p1'
p1 <- 
   # Begins plot 
   ggplot(aes(x=ad_tv,  # levels of 'ad_tv' for x-axis
              y=pred$fit,  # linear prediction for y-axis
              group=as.factor(ad_radio),  # different geoms for each level of 'ad_radio'
              color=as.factor(ad_radio)),  # different colors for each level of 'ad_radio'
          data=ad.tv.pred) +   
   # Draws lines and points based on predicted values
   geom_line() + geom_point() +
   # Adds confidence interaval bands around line
   geom_errorbar(aes(ymin=pred$lwr, ymax=pred$upr), width=5) +
   # Next two commands scale x and y axes
   scale_x_continuous(breaks=seq(0,300,50), minor_breaks=NULL) +
   scale_y_continuous(limits=c(0,30), expand=c(.025,.025), 
                      breaks=seq(0,30,5), minor_breaks=NULL) +
   # Position legend at bottom with title over legend
   theme(legend.position="bottom") +
   guides(color=guide_legend(title.position="top")) +
   # Labels axes and legend
   labs(x="TV Advertising (ad_tv)", 
        y="Linear Prediction", 
        color="Radio Advertising (ad_radio)")

# Repeat for other variable
ad.rad.pred <- crossing(ad_tv=seq(0,300,100), # 4 levels
                       ad_radio=seq(0,50,5)) # 11 levels

ad.rad.pred$pred <- as.data.frame(
   predict.lm(resultssig, ad.rad.pred, interval="confidence"))

p2 <- ggplot(aes(x=ad_radio, y=pred$fit, 
                 group=as.factor(ad_tv), color=as.factor(ad_tv)),
             data=ad.rad.pred) +   
         geom_line() + geom_point() +
         geom_errorbar(aes(ymin=pred$lwr, ymax=pred$upr), width=.083) +
         scale_x_continuous(breaks=seq(0,50,10), minor_breaks=NULL) +
         scale_y_continuous(limits=c(0,30), expand=c(.025,.025), 
                            breaks=seq(0,30,5), minor_breaks=NULL) +
         theme(legend.position="bottom") +
         guides(color=guide_legend(title.position="top")) +
         labs(x="Radio Advertising (ad_radio)", 
         y="Linear Prediction", 
         color="TV Advertising (ad_tv)")

# Arrange three plots in a grid using package 'cowplot'
plot_grid(p1,p2)

x <- 0:5
f <- seq(5,55,10)
t <- seq(20,70,10)
is <- melt(data.frame(x,f,t), id.vars="x", variable.name="dummy")
is$dummy <- factor(is$dummy, levels=c("f","t"), labels=c("<i>D</i> = 0", "<i>D</i> = 1"))
ggplot(aes(x=x, y=value, color=dummy), data=is) +
   geom_line(size=1.5) +
   scale_color_manual(values=c("darkred", "darkgreen")) +
   geom_richtext(data=is[is$x==4,],aes(label=dummy), size=9, vjust=1.45, family="serif", label.color=NA, fill=NA) +
   theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         panel.background=element_blank(), axis.line=element_line(color="black"),
         axis.text=element_blank(), axis.ticks=element_blank(), 
         axis.title=element_blank(), legend.position="none", 
         text=element_text(size=15)) +
   geom_segment(aes(x=0, y=5, xend=0, yend=20), 
                color="midnightblue", size=1, arrow=arrow(ends="both")) +
   geom_richtext(x=0.2, y=12.1, label="<i>&beta;</i><sub>2</sub>", color="midnightblue",label.color=NA, size=9, fill=NA, family="serif") +
   geom_textbox(x=2.5, y=10, label="Slope of both lines", box.color=NA, color="black", size=12, fill=NA, width=unit(0.4, "npc"), halign=1) +
   geom_textbox(x=4.4, y=8, label="= <i>&beta;</i><sub>1</sub>", box.color=NA, color="black", size=12, fill=NA, family="serif")+
   coord_fixed(.05)

x <- 0:5
f <- seq(5,55,10)
t <- seq(5,70,13)
ss <- melt(data.frame(x,f,t), id.vars="x", variable.name="dummy")
ss$dummy <- factor(ss$dummy, levels=c("f","t"), labels=c(
   "<i>D</i> = 0<br><span style='font-family: sans;'>Slope</span> = <i>&beta;</i><sub>1</sub>", 
   "<i>D</i> = 1<br><span style='font-family: sans;'>Slope</span> = <i>&beta;</i><sub>1</sub> + <i>&beta;</i><sub>2</sub>"))
ggplot(aes(x=x, y=value, color=dummy), data=ss) +
   geom_line(size=1.5) +
   scale_color_manual(values=c("darkred", "darkgreen")) +
   geom_richtext(data=ss[ss$x==2,],aes(label=dummy), size=9, vjust=c(1.2,-.75), family="serif", label.color=NA, fill=NA) +
   theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         panel.background=element_blank(), axis.line=element_line(color="black"),
         axis.text=element_blank(), axis.ticks=element_blank(), 
         axis.title=element_blank(), legend.position="none", 
         text=element_text(size=15)) +
   coord_fixed(.05)

x <- 0:5
f <- seq(5,45,8)
t <- seq(20,100,16)
inss <- melt(data.frame(x,f,t), id.vars="x", variable.name="dummy")
inss$dummy <- factor(inss$dummy, levels=c("f","t"), labels=c(
   "<i>D</i> = 0<br><span style='font-family: sans;'>Slope</span> = <i>&beta;</i><sub>1</sub>", 
   "<i>D</i> = 1<br><span style='font-family: sans;'>Slope</span> = <i>&beta;</i><sub>1</sub> + <i>&beta;</i><sub>3</sub>"))
ggplot(aes(x=x, y=value, color=dummy), data=inss) +
   geom_line(size=1.5) +
   scale_color_manual(values=c("darkred", "darkgreen")) +
   geom_richtext(data=inss[inss$x==3,],aes(label=dummy), size=9, vjust=c(1.1,-.6), family="serif", label.color=NA, fill=NA) +
   theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         panel.background=element_blank(), axis.line=element_line(color="black"),
         axis.text=element_blank(), axis.ticks=element_blank(), 
         axis.title=element_blank(), legend.position="none", 
         text=element_text(size=15)) +
   geom_segment(aes(x=0, y=5, xend=0, yend=20), 
                color="midnightblue", size=1, arrow=arrow(ends="both")) +
   geom_richtext(x=0.28, y=13.1, label="<i>&beta;</i><sub>2</sub>", color="midnightblue",label.color=NA, size=9, fill=NA, family="serif") +
   coord_fixed(.0342)

x <- 0:5
L4 <- seq(5,55,10)
L3 <- seq(20,70,10)
L2 <- seq(35,85,10)
L1 <- seq(50,100,10)
mis <- melt(data.frame(x,L4,L3,L2,L1), id.vars="x", variable.name="dummy")
mis$dummy <- factor(mis$dummy, levels=c("L4","L3","L2","L1"), labels=c("<i>L</i><sub>4</sub>", "<i>L</i><sub>3</sub>", "<i>L</i><sub>2</sub>", "<i>L</i><sub>1</sub>"))
ggplot(aes(x=x, y=value, color=dummy), data=mis) +
   geom_line(size=1.5) +
   scale_color_manual(values=c("darkred", "darkgreen", "darkblue", "darkorange")) +
   geom_richtext(data=mis[mis$x==5,],aes(label=dummy), size=9, hjust=0, family="serif", label.color=NA) +
   theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         panel.background=element_blank(), axis.line=element_line(color="black"),
         axis.text=element_blank(), axis.ticks=element_blank(), 
         axis.title=element_blank(), legend.position="none", 
         text=element_text(size=15)) +
   scale_x_continuous(limits=c(-1,6)) +
   geom_richtext(x=-.2, y=5, label="<i>&alpha;</i>", color="darkred",label.color=NA, size=9, family="serif") +
   geom_richtext(x=-.55, y=20, label="<i>&alpha;</i>+ <i>&beta;</i><sub>4</sub>", color="darkgreen",label.color=NA, size=9, family="serif") +
   geom_richtext(x=-.55, y=35, label="<i>&alpha;</i>+ <i>&beta;</i><sub>3</sub>", color="darkblue",label.color=NA, size=9, family="serif") +
   geom_richtext(x=-.55, y=50, label="<i>&alpha;</i>+ <i>&beta;</i><sub>3</sub>", color="darkorange",label.color=NA, size=9, family="serif") +
   geom_textbox(x=3, y=12, label="Slope of<br>all lines", box.color=NA, color="black", size=12, fill=NA, width=unit(0.4, "npc"), halign=1) +
   geom_textbox(x=5.6, y=9, label="= <i>&beta;</i><sub>1</sub>", box.color=NA, color="black", size=12, fill=NA, family="serif")+
   coord_fixed(.048)


lm_beta <- function (MOD) {
   b <- summary(MOD)$coef[-1, 1]
   sx <- sapply(MOD$model[-1], sd)
   sy <- sapply(MOD$model[1], sd)
   beta <- b * sx/sy
   return(beta)
}

lm_beta(results)
lm.beta(results)   
