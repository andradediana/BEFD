---
  title: "BEFD: Final Project"
output:
  pdf_document: default
html_document:
  df_print: paged
date: "2025-11-21"
---
  
###########################################
############### SET UP ####################
###########################################

#First, let's import all the necessary libraries and install packages 
install.packages("plotrix")
install.packages("RColorBrewer")

library(DIMORA)
library(forecast)
library(zoo)
library(lmtest)
library(readxl)
library(plotrix)
library(RColorBrewer)
library(gam)
library(prophet)
library(knitr)
library(ggplot2)
library(knitr)

###########################################
########### DATA PREPROCESSING ############
###########################################

#There are 65 data points depicting the revenue in billion dollars.

#Importing our main dataset 
starbucks <- read.csv("starbucks.csv", header = FALSE)

#Additional datasets to support our conclusions
survey_2016_opinion_italy <- read.csv("statistic_id550719_italy_-consumers-opinion-on-starbucks-positive-aspects-2016.csv")
survey_2023_japan <- read.csv("statistic_id1471037_most-common-reasons-to-love-starbucks-coffee-in-japan-2023.csv")

#Assigning tittle to each column 
colnames(starbucks)[1] <- "Quarter" 
colnames(starbucks)[2] <- "Revenue"


#PLOT OF QUARTERLY DATA 
#Let's start by plotting the data, in order to understand its 
#behavior and possible strategies to fit the model.
#-------------------------------------------------------------
x <- starbucks$Quarter[1:65]
x_q <- as.yearqtr(x, format = "%Y - Q%q")
y <- starbucks$Revenue[1:65]
y_ts <- ts(y, start = c(2009, 1), frequency = 4)
plot(x_q,y, type = "b", pch=20, xlab="Quarter", ylab = "Revenue in billion (USD)",  main="Starbucks Quarterly Revenue (2009 - 2025)", xaxt = "n", lty=3, col="Blue")
#ticks allows us to add a title per year on the x-axis
ticks <- x_q[seq(1, length(x_q), by = 4)] #by = 4 allows one title per year 
axis(1, at=ticks, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)
#-----------------------------------------------------------
#It's evident that the data shows an increasing trend, with some small 
#seasonality and the beginning of each year. Additionally, there is an 
#important dip around the 2020 year. It's necessary to consider all 
#these details when modeling the data.


#PLOT OF CUMULATIVE SUM
#----------------------------------------------------------
plot(cumsum(y), type = "b", pch = 20, xlab="Quarter", ylab = "Cumulative sum in billion (USD)", main="Starbucks Cumulative Quarterly Revenue (2009 - 2025)", xaxt= "n", col="Blue")
#T_pos does not take the labels directly from x_q, takes the position
t_pos <- seq(1, length(x_q), by = 4)
axis(1, at=t_pos, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = t_pos,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(x_q[t_pos]),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)
#----------------------------------------------------------


#SEASONAL PLOT
#----------------------------------------------------------
seasonplot(y_ts,
           col = 1:20,
           pch = 19,
           year.labels = TRUE,
           year.labels.left = TRUE,
           ylab = "Revenue (billion USD)",
           xlab = "Quarter",
           main = "Seasonal Plot of Starbucks Quarterly Revenue (2009-2025)")
#---------------------------------------------------------

#AUTOCORRELATION FUNCTION PLOT 
#It shows many significant auto correlations at different lags, 
#which means past values influence future values. They are 
#decaying slowly. That indicates the presence of an increasing trend.

acf(y)

#PARTIAL AUTOCORRELATION FUNCTION PLOT
#The Partial Autocorrelation Function (PACF) measures the partial 
#correlation between a time series and its lagged values. Here PACF 
#shows a significant spike at lag 1.

pacf(y)


###########################################
########## ADDITIONAL DATASETS ############
###########################################

#There are many reasons why Starbucks remains highly popular and 
#continues to grow in global appeal. A 2023 survey conducted in Japan 
#provides insight into consumers' perceptions of the brand and the 
#factors contributing to its sustained popularity.

#REASONS TO LOVE STARBUCKS PIE CHART
#---------------------------------------------------------
reasons <- survey_2023_japan$Most.common.reasons.to.love.Starbucks.Coffee.in.Japan.2023
percent <- survey_2023_japan$X

#Labels in two lines to improve readability
labels <- paste(reasons, paste0(percent, "%"), sep = "\n")
colors <- brewer.pal(n = length(percent), "Set3")

angles <- pie3D(
  percent,
  labels = NA,          # remove labels from pie
  explode = 0.05,
  col = colors,
  main = "Why People Love Starbucks",
  theta = 0.9,  #inclination of the chart
)

pie3D.labels(
  angles,               # returned from pie3D()
  labels = labels,      # what to display
  radius = 1.55,        # push labels outward
  labelcex = 0.9
)

#-------------------------------------------------------


#Moreover, an earlier survey conducted in Italy explored consumers' 
#perceptions of Starbucks and provided additional insights into how 
#the brand is viewed in different cultural contexts.

#CUSTOMERS PERCEPTION BAR CHART
#-------------------------------------------------------
reasons1 <-survey_2016_opinion_italy$Italy..consumers..opinion.on.Starbucks..positive.aspects.2016
percent1 <- survey_2016_opinion_italy$X
labels1 <- paste(reasons1, percent1, "%")

par(mar = c(5, 12, 4, 2)) #to fix the problem of the margins

bp <- barplot(percent1,
              names.arg = reasons1,
              horiz = TRUE,
              col = "lightskyblue",
              main = "Survey Results: Reasons for Liking Starbucks",
              xlab = "Percentage (%)",
              las = 1,
              cex.names = 0.8 
)

text(x = percent1 + 1, 
     y = bp,
     labels = paste0(percent, "%"),
     cex = 0.8)
#--------------------------------------------------------


###########################################
############# MODELS FITTING ##############
###########################################

#Our goal is to find the best mathematical model to represent the 
#dataset, Starbucks' revenues from 2009-2025. We will start from 
#the simplest models and we will adjust the complexity of these models
#based on their performance. In this case, the simplest model to 
#implement will be a linear model. However, we will include some 
#trend and seasonality to improve its performance. 

##1. Linear Model + trend and seasonality
#--------------------------------------------------------
# Fit linear model with trend and seasonality
TSLM_starbucks <- tslm(y_ts ~ trend + season)
summary(TSLM_starbucks)

#From the R squared value we could say that the models' performance 
#is pretty good, from these results, we can confirm the existence of 
#seasonality. However, it's still important to evaluate the residual 
#plot and ACF of residuals.

#Plotting of the model
plot(y, type = "b", pch = 16, lty = 3, xaxt = "n", ylab = "Revenue (billion USD)", xlab = "Quarter", main="Linear model with trend and seasonality")
lines(1:length(y), fitted(TSLM_starbucks), lwd = 2, col = "red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#T_pos does not take the labels directly from x_q, takes the position
t_pos <- seq(1, length(x_q), by = 4)
axis(1, at=t_pos, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = t_pos,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(x_q[t_pos]),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#From this graph is evident that the model is not able to predict an 
#important behavior of the model, which is the drop in revenues around 
#the second and third quarter in 2020. 

#DW TEST
dwtest(TSLM_starbucks)

#DW < 2 indicates a strong positive auto correlation, and the p-value 
#is extremely small, which is a clear indicator that the autocorrelation
#is definitely present.

#AIC VALUE
TSLM_AIC <- AIC(TSLM_starbucks)
TSLM_AIC

#FORECASTING 
forecast_TSLM <- forecast(TSLM_starbucks)
forecast_TSLM
plot(forecast_TSLM, xlab="Period", ylab="Revenues (billion USD)")

#RESIDUALS LM
res<- residuals(TSLM_starbucks)
plot(res, xlab="Period", main="Residuals from linear model")

#Autocorrelation function
acf(res, main="Autocorrelation from linear model")
#Partial autocorrelation function
Acf(res, main="Partial autocorrelation from linear model")

#MSE
mse_TSLM <- mean(residuals(TSLM_starbucks)^2)
mse_TSLM

#From these graphs we can confirm what we previously stated: there's 
#still some autocorrelation in the model. This can be observed in the 
#first graph, since the residuals are still showing some evident trend, 
#and not the expected white noise behavior. In the same way, both the 
#autocorrelation and partial autocorrelation plots are showing multiple 
#significant lags.
#-----------------------------------------------------------


##2. Diffusion models
#-----------------------------------------------------------
##Fitting Models A product has 4 phases in its life cycle : 1- Introduction 2-Growth 3-Maturity 4-Decline. Starbucks' revenue is a quantitative measure enabling us to model it through economic models such as Bass model or Generalized Bass Model (GBM).

#Bass Model
BM_starbucks <- BM(y, display = T)
summary(BM_starbucks)

#p, q and m estimations are significant and the R\^2 = 0.99. The 
#estimations of p, q and m will now be used to fit the GBM.

###prediction (out-of-sample)
pred_BM_starbucks<- predict(BM_starbucks, newx=c(1:65)) 
pred_BM_starbucks.inst<- make.instantaneous(pred_BM_starbucks)

plot(y, type= "b", xlab = "Quarter", ylab="Revenues (billion USD)", main="Bass Model", pch=16, lty=3, xaxt="n", cex=0.6)
lines(pred_BM_starbucks.inst, lwd=2, col="red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#T_pos does not take the labels directly from x_q, takes the position
t_pos <- seq(1, length(x_q), by = 4)
axis(1, at=t_pos, labels=FALSE) #Drawing the ticks without the labels 

# Add rotated labels
text(x = t_pos,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(x_q[t_pos]),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#Although the cumulative plot shows a reasonably good fit, the 
#instantaneous predictions are less satisfactory due to the sharp 
#decline in revenue beginning in early 2020 and reaching its trough in 
#the third quarter of that year. This decline can be attributed to the 
#impact of the COVID-19 pandemic, which peaked in 2020 and led to 
#widespread lock-downs and operational disruptions.

plot(cumsum(y), type= "b",xlab="Quarter", ylab="Cumulative sum (billion USD)", main="Cumulative revenues fitted with a Bass Model",  pch=16, lty=3, xaxt="n", cex=0.6)
lines(pred_BM_starbucks, lwd=2, col="red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#T_pos does not take the labels directly from x_q, takes the position
t_pos <- seq(1, length(x_q), by = 4)
axis(1, at=t_pos, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = t_pos,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(x_q[t_pos]),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#Bass models residuals
res_BM<- residuals(BM_starbucks)
#Autocorrelation function
acf<- acf(res_BM, main="Autocorrelation from Bass model")
plot(res_BM, pch = 16, main="Residuals from Bass model")

#MSE
mse_BM <- mean((res_BM)^2)
mse_BM

#It is evident that the decline in 2020 represents a shock that must be
#accounted for. To model this effect, we can employ the Generalized Bass 
#Model (GBM), which allows for the incorporation of shocks into the 
#diffusion process.First, we consider a rectangular shock, and for 
#the initial values of p, q and m we use the parameter estimates 
#obtained from the standard Bass Model.

#Generalized Bass Model
GBM_starbucks_rec <- GBM(y,shock = "rett",nshock = 1,prelimestimates = c(1.194006e+03,1.940997e-03,3.000163e-02, 47,51,-2))
summary(GBM_starbucks_rec)

#Residuals
res_GBMr<- residuals(GBM_starbucks_rec)

#MSE
mse_GBMr <- mean((res_GBMr)^2)
mse_GBMr

#All parameter estimates are statistically significant; however, 
#the model can still be improved. Consequently, we fit a Generalized 
#Bass Model with an exponential shock to better capture the observed 
#dynamics in the revenue data.
GBM_starbucks_exp<- GBM(y,shock = "exp",nshock = 1,prelimestimates = c(1.194006e+03,1.940997e-03,3.000163e-02, 47,-0.1,-0.1))
summary(GBM_starbucks_exp)

pred_GBM_starbucks_exp<- predict(GBM_starbucks_exp, newx=c(1:65))
pred_GBM_starbucks_exp.inst<- make.instantaneous(pred_GBM_starbucks_exp)

plot(y, type= "b",xlab="Quarter", ylab="Revenues (billion USD)", main="GBM with exponential shock", pch=16, lty=3, cex=0.6, xaxt="n") #, xlim=c(1,65) 
lines(pred_GBM_starbucks_exp.inst, lwd=2, col="red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#T_pos does not take the labels directly from x_q, takes the position
t_pos <- seq(1, length(x_q), by = 4)
axis(1, at=t_pos, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = t_pos,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(x_q[t_pos]),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#GBM with exponential shock plots
plot(cumsum(y), type= "b",xlab="Quarter", ylab="Cumulative sum (billion USD)", main="Cumulative revenues by GBM with exp. shock", pch=16, lty=3, xaxt="n", cex=0.6)
lines(pred_GBM_starbucks_exp, lwd=2, col="red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#T_pos does not take the labels directly from x_q, takes the position
t_pos <- seq(1, length(x_q), by = 4)
axis(1, at=t_pos, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = t_pos,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(x_q[t_pos]),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#Residuals
res_GBMe<- residuals(GBM_starbucks_exp)
acf<- acf(res_GBMe, main="Autocorrelation from GBM with exp shock")
plot(res_GBMe, pch = 16, main="Residuals from GBM with exponential shock")

#MSE
mse_GBMe <- mean((res_GBMe)^2)
mse_GBMe

#To compare the performances of BM and GBMexp tilde R-squared is 
#calculated:
  
(0.999989- 0.99993)/(1- 0.99993^2)

#since it is greater than 0.3, means that the more complex model, 
#in this case GBMexp, is significant.


#GGM model
GGM_starbucks<- GGM(y, prelimestimates=c(1.194006e+03,  0.05, 0.4, 1.940997e-03, 3.000163e-02))
summary(GGM_starbucks)
pred_GGM_starbucks<- predict(GGM_starbucks, newx=c(1:65))
pred_GGM_starbucks.inst<- make.instantaneous(pred_GGM_starbucks)

res_GGM <- residuals(GGM_starbucks)
plot(res_GGM, pch = 16, main="Residuals from GGM")

mse_GGM <- mean((res_GGM)^2)
mse_GGM

################################################
####Comparison between BM and GBMexp and GGM####
################################################


plot(y, type= "b",xlab="Quarter", ylab="Revenues (billion USD)", main="Comparison between BM, GBMexp and GGM", pch=16, lty=3, cex=0.6, xaxt="n")
lines(pred_BM_starbucks.inst, lwd=2, col="red")
lines(pred_GBM_starbucks_exp.inst, lwd=2, col="deepskyblue")
lines(pred_GGM_starbucks.inst, lwd=2, col="limegreen")
lines(1:length(y), fitted(TSLM_starbucks), lwd = 2, col = "navy" )
legend("topleft", legend = c("Actual", "BM", "GBMexp", "GGM", "LM"), col = c("black", "red", "deepskyblue", "limegreen", "navy"), lty = c(3, 1, 1, 1, 1))
#T_pos does not take the labels directly from x_q, takes the position
t_pos <- seq(1, length(x_q), by = 4)
axis(1, at=t_pos, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = t_pos,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(x_q[t_pos]),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)
#--------------------------------------------------------------

##3. ARIMA models
#--------------------------------------------------------------
#We'll start implementing ARIMA models, in this case we'll consider 
#different combinations of the parameters, from the basic ones to more 
#complex approaches.

#ARIMA (0,0,1)
arima1 <- Arima(y_ts, order = c(0,0,1))
summary(arima1)
resid1<- residuals(arima1)
tsdisplay(resid1)

mse_arima1 <- mean((resid1)^2)
mse_arima1

#We can observe these results are very poor from the ACF and residuals 
#plot. Possibly due to the differentiating term.

plot(y_ts, xlab="Quarter", ylab="Revenues (billion USD)", main="ARIMA (0,0,1)", pch=16, lty=3, cex=0.6, xaxt="n")
lines(fitted(arima1), col="red") #fitted values of the model
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#ticks allows us to add a title per year on the x-axis
ticks <- x_q[seq(1, length(x_q), by = 4)] #by = 4 allows one title per year 
axis(1, at=ticks, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#ARIMA (1,1,1)
arima2 <- Arima(y_ts, order = c(1,1,1))
summary(arima2)
resid2<- residuals(arima2)
tsdisplay(resid2)

#MSE
mse_arima2 <- mean((resid2)^2)
mse_arima2

plot(y_ts, xlab="Quarter", ylab="Revenues (billion USD)", main="ARIMA (1,1,1)", pch=16, lty=3, cex=0.6, xaxt="n")
lines(fitted(arima2), col="red") #fitted values of the model
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#ticks allows us to add a title per year on the x-axis
ticks <- x_q[seq(1, length(x_q), by = 4)] #by = 4 allows one title per year  
axis(1, at=ticks, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#It's evident the improvement of the results by adding the differentiating and AR terms.
#forecast:
for2<- forecast(arima2)
plot(for2) #follows a horizontal line which indicates that maybe the model needs improvement
for2

#ARIMA (3,1,1)
arima3 <-Arima(y_ts, order = c(3,1,1))
summary(arima3)
resid3<- residuals(arima3)
tsdisplay(resid3)

#MSE
mse_arima3 <- mean((resid3)^2)
mse_arima3

plot(y_ts, xlab="Quarter", ylab="Revenues (billion USD)", main="ARIMA (3,1,1)", pch=16, lty=3, cex=0.6, xaxt="n")
lines(fitted(arima3), col="red") #fitted values of the model
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#ticks allows us to add a title per year on the x-axis
ticks <- x_q[seq(1, length(x_q), by = 4)] #by = 4 allows one title per year 
axis(1, at=ticks, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#Forecasting
for3<- forecast(arima3)
plot(for3)
for3

#ARIMA (3,2,2)
arima4 <- Arima(y_ts, order = c(3,2,2))
summary(arima4)
resid4<- residuals(arima4)
tsdisplay(resid4)

#MSE
mse_arima4 <- mean((resid4)^2)
mse_arima4

#Model's plot
plot(y_ts, xlab="Quarter", ylab="Revenues (billion USD)", main="ARIMA (3,2,2)", pch=16, lty=3, cex=0.6, xaxt="n")
lines(fitted(arima4), col="red") #fitted values of the model
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#ticks allows us to add a title per year on the x-axis
ticks <- x_q[seq(1, length(x_q), by = 4)] #by = 4 allows one title per year 
#Drawing the ticks without the labels 
axis(1, at=ticks, labels=FALSE)
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#Forecasting
for4<- forecast(arima4)
plot(for4)
for4

#SARIMA (0,1,1)(0,0,1)
sarima1 <- Arima(y_ts, order=c(0,1,1), seasonal=c(0,0,1))
fit1<- fitted(sarima1)
summary(sarima1)
r1<- residuals(sarima1)
tsdisplay(r1)

#MSE
mse_sarima1 <- mean((r1)^2)
mse_sarima1

#Model's plot
plot(y_ts, xlab="Quarter", ylab="Revenues (billion USD)", main="SARIMA (0,1,1)(0,0,1)", pch=16, lty=3, cex=0.6, xaxt="n")
lines(fit1, col="red") #fitted values of the model
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#ticks allows us to add a title per year on the x-axis
ticks <- x_q[seq(1, length(x_q), by = 4)] #by = 4 allows one title per year 
axis(1, at=ticks, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#Forecasting 
f1<- forecast(sarima1)
plot(f1)


#SARIMA (1,1,1)(1,1,1)
SARIMA2 <- Arima(y_ts, order=c(1,1,1), seasonal=c(1,1,1))
summary(SARIMA2)
r2<- residuals(SARIMA2)
tsdisplay(r2) 

#MSE
mse_sarima2 <- mean((r2)^2)
mse_sarima2

#Model's plot
plot(y_ts, xlab="Quarter", ylab="Revenues (billion USD)", main="SARIMA (1,1,1)(1,1,1)", pch=16, lty=3, cex=0.6, xaxt="n")
lines(fitted(SARIMA2), col="red") #fitted values of the model
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#ticks allows us to add a title per year on the x-axis
ticks <- x_q[seq(1, length(x_q), by = 4)] #by = 4 allows one title per year 
axis(1, at=ticks, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#Forecasting
f2<- forecast(SARIMA2)
plot(f2)


#So far, model parameters were chosen through trial and error but, it 
#could also be done through a function called auto.arima which selects 
#best values for parameters p,q and d. 

#AUTO ARIMA 
auto_arima_starbucks <- auto.arima(y_ts)
auto_arima_starbucks

summary(auto_arima_starbucks)

r.auto<- residuals(auto_arima_starbucks)
tsdisplay(r.auto) 

#MSE
mse_aarima <- mean((r.auto)^2)
mse_aarima

#Model's plot 
plot(y_ts, xlab="Quarter", ylab="Revenues (billion USD)", main="Auto ARIMA (0,1,2)", pch=16, lty=3, cex=0.6, xaxt="n")
lines(fitted(auto_arima_starbucks), col="red") #fitted values of the model
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))
#ticks allows us to add a title per year on the x-axis
ticks <- x_q[seq(1, length(x_q), by = 4)] #by = 4 allows one title per year 
axis(1, at=ticks, labels=FALSE) #Drawing the ticks without the labels
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)


#In order to fit the ARIMAX model, we need an additional dataset, which 
#in this case is about advertising spending of Starbucks between 2011 
#and 2024 in million USD. However, our main dataset about Starbucks' 
#revenues is provided by quarters so it's necessary to convert this 
#dataset to yearly data. 

#ARIMAX 
#-------------------------------------------------------------
#Import additional dataset for ARIMAX model
advertising <- read.csv("starbucks_advertising.csv", head=TRUE)

#Extract the spendings and convert into a time series
spendings <- advertising$Spending
spendings_ts <- ts(spendings, start = c(2011, 1), frequency = 1)

#Obtains the period from advertising dataset
x_yearly <- advertising$Year

#yearly revenues from 2009 to 2024
y_yearly <- aggregate(y_ts, nfrequency = 1, FUN = sum)

#we drop the first two points that correspond to the data of 2009 and 2010, since the advertising dataset starts from 2011.
y_yearly <- y_yearly[3:16] 
y_ts_yearly <- ts(as.numeric(y_yearly), start=2011, frequency=1)

#Another important detail is to take care of is the scale, since the 
#Starbucks' revenues are provided in billion USD, while the advertising 
#spendings in million USD. Since 1,000 million corresponds to 1 billion, 
#we'll simply divide our advertising dataset by 1000. 

#Advertising spendings in billion USD
spendings_ts <- spendings_ts/1000


#Yearly revenues' plot 
plot(y_ts_yearly, type = "b", pch = 16, lty = 3, xaxt = "n", ylab = "Revenue (billion USD)", xlab = "Year", main="Starbucks' yearly revenues from 2011 to 2024", col="blue")
#ticks allows us to add a title per year on the x-axis
ticks <- x_yearly #by = 4 allows one title per year 
axis(1, at=ticks, labels=FALSE) #Drawing the ticks without the labels 
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.3,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#The minimum in the graph is considerably less evident now that the 
#dataset is yearly. 


#Yearly advertising spendings 
plot(spendings_ts, type = "b", pch = 16, lty = 3, xaxt = "n", ylab = "Revenue (billion USD)", xlab = "Year", main="Starbucks' yearly advertising spendings (2011-2024)", col="blue")
#ticks allows us to add a title per year on the x-axis
ticks <- x_yearly #by = 4 allows one title per year 
#Drawing the ticks without the labels 
axis(1, at=ticks, labels=FALSE)
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.3,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#ARIMAX (1,1,1)
armax1<- Arima(y_ts_yearly, xreg=spendings_ts, order=c(1,1,1))
summary(armax1)
res1_armax1<- residuals(armax1)
Acf(res1_armax1)
fitted(armax1)
tsdisplay(res1_armax1)

#MSE
mse_arimax1 <- mean((res1_armax1)^2)
mse_arimax1

#Model's plot 
plot(y_ts_yearly, pch = 16, lty = 3, ylab="Revenues (billion USD)", xlab = "Year", main="ARIMAX (1,1,1)")
lines(fitted(armax1), col="red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))

#ARIMAX (1,0,1)
armax2<- Arima(y_ts_yearly, xreg=spendings_ts, order=c(1,0,1))
summary(armax2)
res_armax2<- residuals(armax2)
Acf(res_armax2)
fitted(armax2)
tsdisplay(res_armax2)

#MSE
mse_arimax2 <- mean((res_armax2)^2)
mse_arimax2

#Models' plot
plot(y_ts_yearly, pch = 16, lty = 3, ylab="Revenues (billion USD)", xlab = "Year", main="ARIMAX (1,0,1)")
lines(fitted(armax2), col="red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))

#AUTO ARIMAX
armax3<- auto.arima(y_ts_yearly, xreg=spendings_ts)
summary(armax3)
res_armax3<- residuals(armax3)
Acf(res_armax3)
fitted(armax3)
tsdisplay(res_armax3)

#MSE
mse_aarimax <- mean((res_armax3)^2)
mse_aarimax

#Model's plot 
plot(y_ts_yearly, pch = 16, lty = 3, ylab="Revenues (billion USD)", xlab = "Year", main="ARIMAX (0,1,0)")
lines(fitted(armax3), col="red")
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = c(3, 1))

#Three models were fitted using ARIMAX, the one with the lowest AIC was 
#the one provided by the auto arima tool, which corresponds to the 
#ARIMA(0,1,0) model.
#-----------------------------------------------------------------

##4. Exponential smoothing
#-----------------------------------------------------------------
###Simple exponential smoothing
#The simple exponential smoothing is useful for forecasting data that have no evident trend or seasonality.
#This specific time series shows generally an increasing trend, despite the decline in 2020, 
#so the simple exponential smoothing is not so much appropriate for this specific case.
#We can choose manually a value for alpha and compare it with the best value of alpha, chosen automatically 
#and considering that lower values of alpha correspond to a flatter behavior, while higher values correspond to a jumpy one. 
SES_1<- ses(y_ts, alpha=0.3, initial="simple", h=8)
SES_2<-ses(y_ts, alpha=0.6, initial="simple", h=8)
SIMPLE<- ses(y_ts, h=8)

#Model's plot
plot(y_ts, ylab="Revenues", xlab="Quarter", main="Simple exponential smoothing", col="black", xaxt="n")
lines(fitted(SES_1), col="violet", type="o")
lines(fitted(SES_2), col="blue", type="o")
lines(fitted(SIMPLE), col="red", type="o")
legend("topleft",legend=c("data", expression(alpha== 0.3),expression(alpha==0.6),"auto"), col=c("black", "violet","blue","red"), lty=c(1,1,1,1))
ticks <- x_q[seq(1, length(x_q), by = 4)] #by = 4 allows one title per year 
#Drawing the ticks without the labels 
axis(1, at=ticks, labels=FALSE)
# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = as.character(ticks),
     srt = 45,         # rotation angle
     adj = 1.1,       # anchor point, how far we are from the tick line
     xpd = TRUE,
     cex = 0.8)

#The value of alpha chosen automatically is alpha = 0.7828, that is 
#the one which minimizes the SSE.
residuals_SIMPLE <-SIMPLE$residuals
tsdisplay(residuals_SIMPLE, main = "Residuals from Automatic Simple Exponential smoothing")
mse_SIMPLE = mean(residuals_SIMPLE^2)
mse_SIMPLE

summary(SIMPLE)
autoplot(y_ts) +
  autolayer(SIMPLE) +
  autolayer(fitted(SIMPLE), series = "Automatic Simple Exponential Smoothing") +
  ylab("Revenues")


###Trend methods (Holt's method)
#Since the Starbucks data show a trend, which is specifically increasing, 
#it's useful to do a double smoothing method, which is known like Holt's trend method. 
#The forecast now should be no longer flat but trending, also taking into account the fact that a straight line of linear regression, could be non adapt or non realistic.
#In this part, we decide to do an automatic Holt's method.
#Finally with the damped, we have again a flat forecast meaning that we depressed it.
HOLT_AUTO<- holt(y_ts, h=8) #automatic and no damped
HOLT_DAMPED<- holt(y_ts, damped=T, phi=0.95, h=8) #this is automatic and damped

plot(forecast(HOLT_AUTO), xlab = "Time", ylab = "Revenues")
plot(forecast(HOLT_DAMPED), xlab = "Time", ylab = "Revenues")

summary(HOLT_AUTO)
summary(HOLT_DAMPED)

autoplot(y_ts) +
  autolayer(HOLT_AUTO, PI = TRUE, alpha = 0.6, color = "red") +
  autolayer(fitted(HOLT_AUTO), series = "Automatic Holt's method") +
  autolayer(HOLT_DAMPED, PI = TRUE, alpha = 0.3, color = "deepskyblue") +
  autolayer(fitted(HOLT_DAMPED), series = "Damped Holt's method") +
  ylab("Revenues")

res_HOLT_AUTO<-residuals(HOLT_AUTO)
tsdisplay(res_HOLT_AUTO, main = "Residuals from Automatic Holt's method")
mse_AUTOHOLT = mean(res_HOLT_AUTO^2)
mse_AUTOHOLT

res_HOLT_DAMPED<-residuals(HOLT_DAMPED)
tsdisplay(res_HOLT_DAMPED, main = "Residuals from Damped Holt's method")
mse_DAMPED_HOLT = mean(res_HOLT_DAMPED^2)
mse_DAMPED_HOLT


###Holt-Winters method
#If there are both trend and seasonality, like in Starbucks revenues case, 
#we can use the Holt-Winters' seasonal method.
HW_ADD<- hw(y_ts, seasonal="additive")
HW_MULT<- hw(y_ts, seasonal="multiplicative")

plot(HW_ADD, xlab = "Time", ylab = "Revenues")
plot(HW_MULT, xlab = "Time", ylab = "Revenues")

summary(HW_ADD)
summary(HW_MULT)

res_HW_ADD<-residuals(HW_ADD)
tsdisplay(res_HW_ADD, main = "Residuals from HW with additive seasonality")
mse_HW_ADD = mean(res_HW_ADD^2)
mse_HW_ADD

res_HW_MULT<-residuals(HW_MULT)
tsdisplay(res_HW_MULT, main = "Residuals from HW with multiplicative seasonality")
mse_HW_MULT = mean(res_HW_MULT^2)
mse_HW_MULT

#Comparison between HW additive and HW multiplicative
autoplot(y_ts) +
  autolayer(HW_ADD, series = "HW additive", PI = TRUE, color = "red", alpha = 0.6) +
  autolayer(HW_MULT, series = "HW multiplicative", PI = TRUE, color = "deepskyblue", alpha = 0.3) + 
  autolayer(fitted(HW_ADD), series="HW additive") +
  autolayer(fitted(HW_MULT), series = "HW multiplicative") +
  ylab("Revenues")

xlim_zoom<-c(2022, 2028)
ylim_zoom<-c(7,10)
autoplot(y_ts) +
  autolayer(HW_ADD, series = "HW additive", PI = FALSE) +
  autolayer(HW_MULT, series = "HW multiplicative", PI = FALSE) +
  coord_cartesian(xlim = xlim_zoom, ylim = ylim_zoom) +
  ggtitle("Zoomed Forecast") +
  ylab("Revenues")


###ETS model
####ETS FOR QUARTERLY DATA
#The model selected automatically for quarterly data is the one with multiplicative error, additive trend and additive seasonality.
ETS=ets(y_ts, ic="aic")  #we do an automatic selection (model="ZZZ"), based on the AIC
summary(ETS)
autoplot(ETS)#notice the small scale of the slope, indicating a minimal change

residuals_ETS <- residuals(ETS)
tsdisplay(residuals_ETS, main = "Residuals from the quarterly ETS model")
mse_ETS = mean(residuals_ETS^2)
mse_ETS

forecast_ets<-forecast(ETS, h=8)
autoplot(forecast_ets) + 
  autolayer(fitted(ETS), series="Quarterly ETS") +
  ylab("Revenues")
#------------------------------------------------------------

##5. GENERALIZED ADDITIVE MODELS
#------------------------------------------------------------
#The first try is with the linear model, which is a special case of GAM. 
#Here we assume a linear relationship between both predictors and the response.
#LINEAR MODEL like a special case of GAM: same of lm(y_ts_yearly ~ tt+spendings_ts)
tt<-1:length(y_ts_yearly)

G0<-gam(y_ts_yearly ~ tt+spendings_ts)
summary(G0)
AIC(G0)
par(mfrow=c(1,2))
plot(G0, se=T)

#Residuals plot
tsdisplay(residuals(G0), main = "Residuals of linear GAM")

#MSE
mse_G0<-mean(residuals(G0)^2)
mse_G0


#GAM (SPLINE):
#we want to allow a little bit of flexibility with smoothing spline for the advertising spendings.
#Although the smooth term for the spendings does not capture significant nonlinearity, increasing also the AIC value.
G1<-gam(y_ts_yearly ~ tt+s(spendings_ts))
summary(G1)
AIC(G1)
par(mfrow=c(1,2))
plot(G1, se=T)


#Residuals plot
tsdisplay(residuals(G1), main = "Residuals of GAM with smoothing spline for the advertising spendings")

#MSE
mse_G1<-mean(residuals(G1)^2)
mse_G1



#there is no need to make the comparison between arima gam residuals and the gam itself
#----------------------------------------------------------

##5.PROPHET MODEL
#----------------------------------------------------------
#As a final model, we use the prophet one.
#We first plot the quarterly data 
  #-linear growth 
  #-logistic growth 
#Then we plot the yearly data
  #-linear growth 
  #-logistic growth

###LINEAR GROWTH: QUARTERLY DATA ADDITIVE 
#One more time, we keep a default number of changepoints.
dates<-as.Date(x_q)
quarterly_df<-data.frame(ds=dates, y=as.numeric(y))

m1 = prophet(quarterly_df,  growth="linear", yearly.seasonality=TRUE, weekly.seasonality=FALSE, daily.seasonality=FALSE)
future_m1 <- make_future_dataframe(m1, periods=8, freq="quarter",include_history=TRUE)
forecast_m1<- predict(m1, future_m1)
plot(m1, forecast_m1) +
  ggtitle("Quarterly data: Linear growth")

#Mean squared error
residuals_m1<-quarterly_df$y - forecast_m1$yhat[1:nrow(quarterly_df)]
mse_m1<-mean(residuals_m1^2)
mse_m1

#Residuals plot
tsdisplay(residuals_m1, main = "Quarterly data: Residuals from Prophet model with Linear growth")


###LINEAR GROWTH: QUARTERLY MULTIPLICATIVE
m1_mult = prophet(quarterly_df, growth="linear", yearly.seasonality=TRUE, weekly.seasonality=FALSE, daily.seasonality=FALSE, seasonality.mode="multiplicative")
future_m1_mult <- make_future_dataframe(m1_mult, periods=8, freq="quarter",include_history=TRUE)
forecast_m1_mult<- predict(m1_mult, future_m1_mult)
plot(m1_mult, forecast_m1_mult) + 
  ggtitle("Quarterly data: Linear growth (multiplicative)")

#Mean squared error
residuals_m1_mult<-quarterly_df$y - forecast_m1_mult$yhat[1:nrow(quarterly_df)]
mse_m1_mult<-mean(residuals_m1_mult^2)
mse_m1_mult

#Residuals plot
tsdisplay(residuals_m1_mult, main = "Quarterly data: Residuals from Prophet model with Linear growth (multiplicative)")

###LOGISTIC GROWTH: QUARTERLY DATA
#Now we set also a carry capacity to 12, after we checked the five-number summary.
quarterly_df$cap<-12
m2<-prophet(quarterly_df, growth="logistic", yearly.seasonality=TRUE, weekly.seasonality=FALSE, daily.seasonality=FALSE) 
future_m2<-make_future_dataframe(m2, periods=8, freq = "quarter", include_history=T)
future_m2$cap<-12
forecast_m2<-predict(m2, future_m2)
plot(m2, forecast_m2) + 
  ggtitle("Quarterly data: Logistic growth")
summary(quarterly_df$y) #to explain why cap is 12

#Mean squared error
residuals_m2<-quarterly_df$y - forecast_m2$yhat[1:nrow(quarterly_df)]
mse_m2<-mean(residuals_m2^2)
mse_m2

#Residuals plot
tsdisplay(residuals_m2, main = "Quarterly data: Residuals from Prophet model with Logistic growth")

###LOGISTIC GROWTH: QUARTERLY DATA MULTIPLICATIVE
#Now we set also a carry capacity to 12, after we checked the five-number summary.
quarterly_df$cap<-12
m2_mult<-prophet(quarterly_df, growth="logistic", yearly.seasonality=TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE, seasonality.mode = "multiplicative") 
future_m2_mult<- make_future_dataframe(m2_mult, periods=8, freq = "quarter", include_history=T)
future_m2_mult$cap<-12
forecast_m2_mult <- predict(m2_mult, future_m2_mult)
plot(m2_mult, forecast_m2_mult) + 
  ggtitle("Quarterly data: Logistic growth (multiplicative)")
summary(quarterly_df$y) #to explain why cap is 12

#Mean squared error
residuals_m2_mult<-quarterly_df$y - forecast_m2_mult$yhat[1:nrow(quarterly_df)]
mse_m2_mult<-mean(residuals_m2_mult^2)
mse_m2_mult

#Residuals plot
tsdisplay(residuals_m2_mult, main = "Quarterly data: Residuals from Prophet model with Logistic growth (multiplicative)")

###SHOCK AS HOLIDAY WITH LINEAR GROWTH
##ADDITIVE
#Consider the shock as holiday (m3) with predefined lockdown dates (found on prophet model website)
#It captures better the shock an ACF plot shows an almost white-noise behavior.
lockdowns <- data.frame(
  holiday = c("lockdown_1", "lockdown_2", "lockdown_3","lockdown_4"),
  ds = as.Date (c("2020-03-21","2020-07-09","2021-02-13","2021-05-28")),
  lower_window = c(0,0,0,0),
  ds_upper = as.Date(c("2020-06-06", "2020-10-27", "2021-02-17","2021-06-10"))
)

lockdowns$upper_window <- as.numeric(lockdowns$ds_upper - lockdowns$ds) #duration of the lockdowns effect
lockdowns

m3<-prophet(quarterly_df, growth="linear", yearly.seasonality=TRUE, weekly.seasonality=FALSE, daily.seasonality=FALSE, holidays = lockdowns)
future_m3<- make_future_dataframe(m3, periods=8, freq="quarter")
forecast_m3<- predict(m3, future_m3)
plot(m3, forecast_m3) + 
  ggtitle("Quarterly data: Prophet linear model with shock as holiday")

#Mean squared error
residuals_m3<-quarterly_df$y - forecast_m3$yhat[1:nrow(quarterly_df)]
mse_m3<-mean(residuals_m3^2)
mse_m3

#Residuals plot
tsdisplay(residuals_m3, main = "Quarterly data: Residuals from Prophet linear model with shock as holiday")


#MULTIPLICATIVE
m3_mult<-prophet(quarterly_df, growth="linear", yearly.seasonality=TRUE, weekly.seasonality=FALSE, daily.seasonality=FALSE, holidays = lockdowns, seasonality.mode = "multiplicative")
future_m3_mult<- make_future_dataframe(m3_mult, periods=8, freq="quarter")
forecast_m3_mult<- predict(m3_mult, future_m3_mult)
plot(m3_mult, forecast_m3_mult) + 
  ggtitle("Quarterly data: Prophet linear model with shock as holiday (multiplicative)")

#Mean squared error
residuals_m3_mult<-quarterly_df$y - forecast_m3_mult$yhat[1:nrow(quarterly_df)]
mse_m3_mult<-mean(residuals_m3_mult^2)
mse_m3_mult

#Residuals plot
tsdisplay(residuals_m3_mult, main = "Quarterly data: Residuals from Prophet linear model with shock as holiday (multiplicative)")

#------------------------------------------------------------------
years<-2011:2024
yearly_df<-data.frame(ds=as.Date(paste0(years,"-01-01")),y=y_yearly, spendings=spendings_ts)

###LINEAR GROWTH: YEARLY DATA
#We set a default number of changepoints, which is 25 but since in this case we have just 14 observations, 
#Prophet automatically reduces the number of potential changepoints to 10.
#We also add the spendings variable in order to do later a comparison between prophet and gam.
m4<-prophet(growth="linear", yearly.seasonality=FALSE, weekly.seasonality=FALSE, daily.seasonality=FALSE)
m4<-add_regressor(m4, "spendings")
m4<-fit.prophet(m4,yearly_df)
future_m4<- make_future_dataframe(m4, periods=2, freq="year")
last_spending<-tail(spendings_ts,1)
future_spendings <- c(last_spending+0.09, last_spending+0.18) #we suppose that from year to year, in 2025 and 2026, 
#the spendings will increase by 0.09 as the latest years
future_m4$spendings <- c(spendings_ts, future_spendings)
forecast_m4<- predict(m4, future_m4)
plot(m4, forecast_m4) + 
  ggtitle("Yearly data: Linear growth")
m4$params$beta

#Mean squared error
residuals_m4<-yearly_df$y - forecast_m4$yhat[1:nrow(yearly_df)]
mse_m4<-mean(residuals_m4^2)
mse_m4

#Residuals plot
tsdisplay(residuals_m4, main = "Yearly data: Residuals from Prophet model with Linear growth")
  
###LOGISTIC GROWTH: YEARLY DATA
#Now we set also a carry capacity to 45, after we checked the five-number summary.
yearly_df$cap <- 45
m5<-prophet(growth="logistic", yearly.seasonality=FALSE, weekly.seasonality=FALSE, daily.seasonality=FALSE)
m5<-add_regressor(m5, "spendings")
m5<-fit.prophet(m5,yearly_df)
future_m5<- make_future_dataframe(m5, periods=2, freq="year")
future_m5$cap<-45
last_spending<-tail(spendings_ts,1)
future_spendings <- c(last_spending+0.09, last_spending+0.18)
future_m5$spendings <- c(spendings_ts, future_spendings)
forecast_m5<- predict(m5, future_m5)
plot(m5, forecast_m5) + 
  ggtitle("Yearly data: Logistic growth")
m5$params$beta

#Mean squared error
residuals_m5<-yearly_df$y - forecast_m5$yhat[1:nrow(yearly_df)]
mse_m5<-mean(residuals_m5^2)
mse_m5

#Residuals plot
tsdisplay(residuals_m5, main = "Yearly data: Residuals from Prophet model with Logistic growth")


#YEARLY DATA G0 AND PROPHET LINEAR
#The comparison is made between the simple GAM model   
#and the prophet model with linear growth. 
N <- nrow(yearly_df)  
yearly_df$prophet_pred <- forecast_m4$yhat[1:N]
yearly_df$gam_pred <- G0$fitted.values

plot(yearly_df$ds, yearly_df$y, type="l", col="black", pch = 16, lty = 3, lwd=2,
     main="GAM vs Prophet",
     xlab="Year", ylab="Revenues", xaxt="n")

lines(yearly_df$ds, yearly_df$gam_pred, col="violet", lty = 1, lwd = 5)
lines(yearly_df$ds, yearly_df$prophet_pred, col="lightgreen",lty = 2, lwd=5)


legend("topleft",legend=c("Actual", "GAM", "Prophet"),
       col=c("black","violet","lightgreen"),
       lwd=2, lty = c(3,1,2), cex=0.8)

ticks <- yearly_df$ds 

#Drawing the ticks without the labels 
axis(1, at=ticks, labels=FALSE)

# Add rotated labels
text(x = ticks,
     y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = format(ticks, "%Y"),
     srt = 45,      
     adj = 1.3,       
     xpd = TRUE,
     cex = 0.8)



#------------------------------------------------------------

##6.COMPARISON BETWEEN AICs OF QUARTERLY MODELS
#------------------------------------------------------------
QUARTERLY_AIC_table <- data.frame(
  Model = c("TSLM","ARIMA(0,0,1)","ARIMA(1,1,1)","ARIMA(3,1,1)","ARIMA(3,2,2)","Auto.ARIMA(0,1,2)","SARIMA(0,1,1)(0,0,1)[4]", "SARIMA(1,1,1)(1,1,1)[4]","SES", "HOLT_AUTO", "HOLT_DAMPED", "HW_ADD","HW_MULT","ETS"),
  AIC   = c(
    AIC(TSLM_starbucks),
    AIC(arima1),
    AIC(arima2),
    AIC(arima3),
    AIC(arima4),
    AIC(auto_arima_starbucks),
    AIC(sarima1),
    AIC(SARIMA2),
    AIC(SIMPLE$model),
    AIC(HOLT_AUTO$model),
    AIC(HOLT_DAMPED$model),
    AIC(HW_ADD$model),
    AIC(HW_MULT$model),
    AIC(ETS)
  )
)

kable(QUARTERLY_AIC_table, caption = "Comparison of AIC values across models for quarterly data")
#-----------------------------------------------------------

##7.COMPARISON BETWEEN AICs OF YEARLY MODELS
#-----------------------------------------------------------
YEARLY_AIC_table <- data.frame(
  Model = c("ARIMAX(1,1,1)","ARIMAX(1,0,1)","AUTO ARIMAX","Linear GAM","GAM with smoothing"),
  AIC   = c(
    AIC(armax1),
    AIC(armax2),
    AIC(armax3),
    AIC(G0),
    AIC(G1)
  )
)

kable(YEARLY_AIC_table, caption = "Comparison of AIC values across models for yearly data")
#-----------------------------------------------------------

##8. COMPARISON BETWEEN MSEs OF QUARTERLY MODELS
#-----------------------------------------------------------
QUARTERLY_MSE_table <- data.frame(
  Model = c("TSLM","BM","GBM rectangular","GBM exponential", "GGM",
            "ARIMA(0,0,1)","ARIMA(1,1,1)","ARIMA(3,1,1)","ARIMA(3,2,2)","Auto.ARIMA(0,1,2)","SARIMA(0,1,1)(0,0,1)[4]", "SARIMA(1,1,1)(1,1,1)[4]",
            "SES", "HOLT_AUTO", "HOLT_DAMPED", "HW_ADD","HW_MULT","ETS",
            "Prophet: Linear growth","Prophet (mult): Linear growth", "Prophet: Logistic growth", "Prophet(mult): Logistic growth", "Prophet: Linear growth with holiday", "Prophet(mult): Linear growth with holiday"),
  MSE   = c(
    mse_TSLM,
    mse_BM,
    mse_GBMr,
    mse_GBMe,
    mse_GGM,
    mse_arima1,
    mse_arima2,
    mse_arima3,
    mse_arima4,
    mse_aarima,
    mse_sarima1,
    mse_sarima2,
    mse_SIMPLE,
    mse_AUTOHOLT,
    mse_DAMPED_HOLT,
    mse_HW_ADD,
    mse_HW_MULT,
    mse_ETS,
    mse_m1,
    mse_m1_mult,
    mse_m2,
    mse_m2_mult,
    mse_m3,
    mse_m3_mult
  )
)

kable(QUARTERLY_MSE_table, caption = "Comparison of MSE values across models for quarterly data")
#-----------------------------------------------------------

##9. COMPARISON BETWEEN MSEs OF YEARLY MODELS
#-----------------------------------------------------------
YEARLY_MSE_table <- data.frame(
  Model = c("ARIMAX(1,1,1)","ARIMAX(1,0,1)","AUTO ARIMAX",
    "Linear GAM","GAM with smoothing",
    "Prophet: Linear growth","Prophet: Logistic growth"
    ),
  MSE   = c(
    mse_arimax1,
    mse_arimax2,
    mse_aarimax,
    mse_G0,
    mse_G1,
    mse_m4,
    mse_m5
  )
)

kable(YEARLY_MSE_table, caption = "Comparison of MSE values across models for yearly data")

#-----------------------------------------------------------

