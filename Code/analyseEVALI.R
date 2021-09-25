# Author: Kamila Janmohamed
# Purpose: ITS segmented regression on vape sales data for 2019

library(readxl)
library(dplyr)
library(stringr)
library(gtrendsR) 
library(ggplot2)
library(lubridate)
library(questionr)
library(forecast)
library(lmtest)
library(ggpubr)
library(aTSA)
library(dvmisc)
library(tidyverse)
library(gtools)
library(Matrix)
library(gridExtra)
library(segmented)

# Load data ----
itsData <- read.csv("Data/Final/vapeSalesITS.csv", header = T)


# Create categorical variables to address NAs -----------------------------
# Vape hospitalisations
# Replace NAs with 0s, create deciles and label from 0 to 9 0  0 includes NAs and the bottom 10%
itsData$vapeHospCat <- (ntile(itsData$vapeHospitalisations, 10)-1) %>%
  replace_na(0)

# cumulative sum of hospitalisations
itsData$vapeHospSum <- itsData$vapeHospitalisations
itsData$vapeHospSum[is.na(itsData$vapeHospSum)] <- 0
itsData$vapeHospSum <- cumsum(itsData$vapeHospSum)

# Covid cases 
itsData$covidCasesCat <- (ntile(itsData$meanCovidCases, 10)-1) %>%
  replace_na(0)

itsData$covidDeathsCat <- (ntile(itsData$meanCovidDeaths, 10)-1) %>%
  replace_na(0)


# Visualise series --------------------------------------------------------
ggplot(itsData, aes(x=X)) + 
  geom_point(aes(y = (revenueV/1000000))) +
  geom_vline(xintercept = 33, linetype = "dotted") + 
  geom_vline(xintercept = 37, linetype = "dotted") + 
  geom_vline(xintercept = 40, linetype = "dotted") + 
  geom_vline(xintercept = 45, linetype = "dotted") + 
  geom_vline(xintercept = 53, linetype = "dotted") + 
  geom_vline(xintercept = 63, linetype = "dotted") + 
  geom_vline(xintercept = 74, linetype = "dotted") + 
  geom_vline(xintercept = 80, linetype = "dotted") + 
  geom_vline(xintercept = 92, linetype = "dotted") + 
  geom_vline(xintercept = 95, linetype = "dotted") + 
  scale_x_continuous(name = "Weeks") + 
  scale_y_continuous(name = "Vape Sales (millions)") + 
  ggtitle("Plot of weekly vape sales in millions of USD") +
  ggplot2::annotate("label", x = 33, y = 115, label = "a", size = 4) + 
  ggplot2::annotate("label", x = 37, y = 115, label = "b", size = 4) + 
  ggplot2::annotate("label", x = 40, y = 115, label = "c", size = 4) +
  ggplot2::annotate("label", x = 45, y = 115, label = "d", size = 4) + 
  ggplot2::annotate("label", x = 53, y = 115, label = "e", size = 4) +
  ggplot2::annotate("label", x = 63, y = 115, label = "f", size = 4) +
  ggplot2::annotate("label", x = 74, y = 115, label = "g", size = 4) +
  ggplot2::annotate("label", x = 80, y = 115, label = "h", size = 4) +
  ggplot2::annotate("label", x = 92, y = 115, label = "i", size = 4) +
  ggplot2::annotate("label", x = 95, y = 115, label = "j", size = 4) +
  
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"))
ggsave("Figures/salesSeries.png", width=9, height=4)

# EVALI analysis -----------------------------------------------------
evaliITS <- itsData[which(itsData$weekEnd <= "2019-12-28"),]

# Summary statistics
mean(evaliITS$revenueV)
sd(evaliITS$revenueV)
# create the time series 
# remember to log transform the data
tsVapeSalesV <- ts(log(evaliITS[,5]), freq = 52)
plot(tsVapeSalesV)
abline(reg=lm(tsVapeSalesV~time(tsVapeSalesV)))
cycle(tsVapeSalesV)
plot(aggregate(tsVapeSalesV,FUN=mean))
boxplot(tsVapeSalesV~cycle(tsVapeSalesV))

# Examine the time series before the strat of EVALI - 17 Aug 2019 - in row 33
noise <- ts(evaliITS[1:33,5], freq = 52)
plot(noise)

# decompose to see how the different components contribute to the time series
plot(decompose(noise),xlab="Weeks")

# check the contribution of each component to the overall variablility
mean(noise)/sd(noise)
(1-sd(noise-na.omit(decompose(noise)$trend))/sd(noise) )*100
(1-sd(noise-na.omit(decompose(noise)$seasonal))/sd(noise) )*100
(1-sd(na.omit(decompose(noise)$random))/sd(noise) )*100

# Check for stationarity
par(mfrow=c(2,1))
acf(noise,main="Sensitivity analysis of vape sales")
pacf(noise,main="Sensitivity analysis of vape sales")
print(acf(noise,main="Sensitivity analysis of vape sales"))
print(pacf(noise,main="Sensitivity analysis of vape sales"))

# unit root test for stationarity
adf.test(noise)
# large p values, differencing not required. 

## fit autoregressive moving average models for the noise series. Use auto to detect the model based on AIC and BIC
arima_aic <- auto.arima(noise,trace=TRUE,test="kpss",ic="aic")
summary(arima_aic)
confint(arima_aic)
arima_bic <- auto.arima(noise,trace=TRUE,test="kpss",ic="bic")
summary(arima_bic)
confint(arima_bic)

# arima 110


# Forecast based on model -------------------------------------------------
preDat <- evaliITS[1:33,]

fit <- Arima(log(preDat$revenueV),
            order = c(1,1,0),
            xreg = cbind(log(preDat$revenueC), preDat$vapeHospCat, preDat$X),
            include.constant = F)

forecast <- forecast::forecast(fit, xreg = cbind(log(evaliITS$revenueC[34:52]), evaliITS$vapeHospCat[34:52], evaliITS$X[34:52]), h=19, level=c(95))

plot(forecast)
lines(c(34:52), log(evaliITS$revenueV[34:52]), lty = 2, col = "red")

forecast1 <- as.data.frame(forecast)
# Plot forecast
ggplot(preDat, aes(x=X)) + 
  geom_line(aes(y=log(revenueV))) +
  geom_ribbon(data = forecast1, aes(x = c(34:52), ymin = `Lo 95`, ymax = `Hi 95`), fill = "grey70") +
geom_line(data = forecast1, aes(x = c(34:52), y=`Point Forecast`), color = "blue") +
geom_line(data = itsData[34:52,], aes(x = c(34:52), y = log(revenueV)), linetype = "dashed", color="red") +
  scale_x_continuous(name = "Weeks", 
                     breaks = seq(1,53,4),
                     labels = c("30 Dec 2018 \n(Week 1)", "27 Jan 2019 \n(Week 5)", "24 Feb 2019 \n(Week 9)", "24 Mar 2019 \n(Week 13)", "21 Apr 2019 \n(Week 17)", "19 May 2019 \n(Week 21)", "16 Jun 2019 \n(Week 25)", " 14 Jul 2019 \n(Week 29)", "11 Aug 2019 \n(Week 33)", "8 Sep 2019 \n(Week 37)", "6 Oct 2019 \n(Week 41)", "3 Nov 2019 \n(Week 45)", "1 Dec 2019 \n(Week 49)", "22 Dec 2019 \n(Week 52)")) + 
  scale_y_continuous(name = "Log vape sales",
                     breaks = seq(18.6, 19.6, 0.1)) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"))
ggsave("Figures/evaliForecast.png", width=9, height=6)


# Get averted sales from forecast -----------------------------------------
sales <- itsData$revenueV[34:52]
forecasted <- exp(forecast1$`Point Forecast`)
(difference <- forecasted - sales)
(total_diff <- sum(difference))
(pct <- 100*total_diff/sum(forecasted))

# Define intervention variables -------------------------------------------
# 17 August 2019,  CDC announced that they would be actively investigating approximately 94 cases of vaping-related illnesses in 14 states = week 33
# 11 September 2019, Trump administration considers ban on vaping products = week 37
# 24 September 2019, masachusettes bans vaping products = week 39
# 4 October 2019, The FDA warned consumers not to use any THC-containing vapes = week 40
# November 8 2019, Vitamin E acetate responsible for EVALI (CDC announcement) = week 45

gtHitsV <- evaliITS$relativeHits
vapeHospV <- evaliITS$vapeHospitalisations
vapeHospCatV <- evaliITS$vapeHospCat
vapeHospSum <- evaliITS$vapeHospSum
cigSalesV <- evaliITS$revenueC
lCigSalesV <- log(evaliITS$revenueC)

WeeksV <- 1:52
cdcEvali <- as.numeric(1*seq(tsVapeSalesV) > 33)
trumpBan <- as.numeric(1*seq(tsVapeSalesV) > 37)
masBan <- as.numeric(1*seq(tsVapeSalesV) > 39)
fdaTHC <- as.numeric(1*seq(tsVapeSalesV) > 40)
acetate <- as.numeric(1*seq(tsVapeSalesV) > 45)

IcdcEvali <- cumsum(cdcEvali)
ItrumpBan <- cumsum(trumpBan)
ImasBan <- cumsum(masBan)
IfdaTHC <- cumsum(fdaTHC)
Iacetate <- cumsum(acetate)

# Checking correlations ---------------------------------------------------
# Check correlations to see which variables will cause problems 
cor(cbind(gtHitsV, vapeHospCatV, vapeHospSum, cigSalesV, WeeksV, cdcEvali, trumpBan, masBan, fdaTHC, acetate, IcdcEvali, ItrumpBan, ImasBan, IfdaTHC, Iacetate))

# CDC evali and trump ban
# Mas ban, FDA thc 


# Fitting model -----------------------------------------------------------
fitTSV <- Arima(tsVapeSalesV, 
                 order = c(1,1,0),
                 xreg = cbind(vapeHospCatV, lCigSalesV, WeeksV, cdcEvali, trumpBan, fdaTHC, acetate, IcdcEvali, ItrumpBan, IfdaTHC, Iacetate), 
                 include.constant = F)

summary(fitTSV)
confint(fitTSV, level = 0.95)
coeftest(fitTSV)



# Graphs ------------------------------------------------------------------
par(mfrow=c(1,1))
plot(fitTSV)
plot(fitted(fitTSV))

evaliITS$model <- as.numeric(fitted(fitTSV))
evaliITS$upperCI <- as.numeric(fitted(fitTSV) + 1.96*sqrt(fitTSV$sigma2)) #confidence intervals
evaliITS$lowerCI <- as.numeric(fitted(fitTSV) - 1.96*sqrt(fitTSV$sigma2))

ggplot(evaliITS, aes(x=X)) + 
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI), fill = "grey70") + 
  geom_line(aes(y = model, colour = 1)) + 
  geom_point(aes(y = log(revenueV))) +
  geom_vline(xintercept = 33, linetype = "dotted", col = "red") + 
  geom_vline(xintercept = 37, linetype = "dotted", col = "black") + 
  geom_vline(xintercept = 40, linetype = "dotted", col = "black") + 
  geom_vline(xintercept = 45, linetype = "dotted", col = "red") + 
  scale_x_continuous(name = "Weeks", 
                     breaks = seq(1,53,4),
                     labels = c("30 Dec 2018 \n(Week 1)", "27 Jan 2019 \n(Week 5)", "24 Feb 2019 \n(Week 9)", "24 Mar 2019 \n(Week 13)", "21 Apr 2019 \n(Week 17)", "19 May 2019 \n(Week 21)", "16 Jun 2019 \n(Week 25)", " 14 Jul 2019 \n(Week 29)", "11 Aug 2019 \n(Week 33)", "8 Sep 2019 \n(Week 37)", "6 Oct 2019 \n(Week 41)", "3 Nov 2019 \n(Week 45)", "1 Dec 2019 \n(Week 49)", "22 Dec 2019 \n(Week 52)")) + 
  scale_y_continuous(name = "Log vape sales") + 
  ggplot2::annotate("label", x = 33, y = 18.65, label = "a", size = 4, col = "red") + 
  ggplot2::annotate("label", x = 37, y = 18.65, label = "b", size = 4, col = "black") + 
  ggplot2::annotate("label", x = 40, y = 18.65, label = "c", size = 4, col = "black") +
  ggplot2::annotate("label", x = 45, y = 18.65, label = "d", size = 4, col = "red") + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"))
ggsave("Figures/evaliITS.png", width=9, height=6)
