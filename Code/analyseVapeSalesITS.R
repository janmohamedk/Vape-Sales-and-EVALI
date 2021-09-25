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

# Load data ----
itsData <- read.csv("Data/Final/vapeSalesITS.csv", header = T)

# Create categorical variables to address NAs -----------------------------
# Vape hospitalisations
# Replace NAs with 0s, create deciles and label from 0 to 9 0  0 includes NAs and the bottom 10%
itsData$vapeHospCat <- (ntile(itsData$vapeHospitalisations, 10)-1) %>%
  replace_na(0)

# Covid cases 
itsData$covidCasesCat <- (ntile(itsData$meanCovidCases, 10)-1) %>%
  replace_na(0)

itsData$covidDeathsCat <- (ntile(itsData$meanCovidDeaths, 10)-1) %>%
  replace_na(0)

# EVALI analysis -----------------------------------------------------
vapeITS <- itsData[which(itsData$weekEnd <= "2019-12-28"),]

# create the time series 
  # remember to log transform the data
tsVapeSalesV <- ts(log(vapeITS[,5]), freq = 52)
plot(tsVapeSalesV)
abline(reg=lm(tsVapeSalesV~time(tsVapeSalesV)))
cycle(tsVapeSalesV)
plot(aggregate(tsVapeSalesV,FUN=mean))
boxplot(tsVapeSalesV~cycle(tsVapeSalesV))

# Examine the time series before the strat of EVALI - 17 Aug 2019 - in row 33
noise <- ts(vapeITS[1:33,5], freq = 52)
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

# Define intervention variables -------------------------------------------
# 17 August 2019,  CDC announced that they would be actively investigating approximately 94 cases of vaping-related illnesses in 14 states = week 33
# 11 September 2019, Trump administration considers ban on vaping products = week 37
# 24 September 2019, masachusettes bans vaping products = week 39
# 4 October 2019, The FDA warned consumers not to use any THC-containing vapes = week 40
# November 8 2019, Vitamin E acetate responsible for EVALI (CDC announcement) = week 45

gtHitsV <- vapeITS$relativeHits
vapeHospV <- vapeITS$vapeHospitalisations
vapeHospCatV <- vapeITS$vapeHospCat
cigSalesV <- vapeITS$revenueC
lCigSalesV <- log(vapeITS$revenueC)

WeeksV <- 1:52
cdcEvali <- as.numeric(1*seq(tsVapeSalesV) > 33)
trumpBan <- as.numeric(1*seq(tsVapeSalesV) > 37)
masBan <- as.numeric(1*seq(tsVapeSalesV) > 39)
fdaTHC <- as.numeric(1*seq(tsVapeSalesV) > 40)
acetate <- as.numeric(1*seq(tsVapeSalesV) > 45)

WE <- WeeksV*cdcEvali
WT <- WeeksV*trumpBan
WM <- WeeksV*masBan
WF <- WeeksV*fdaTHC
WA <- WeeksV*acetate

IcdcEvali <- cumsum(cdcEvali)
ItrumpBan <- cumsum(trumpBan)
ImasBan <- cumsum(masBan)
IfdaTHC <- cumsum(fdaTHC)
Iacetate <- cumsum(acetate)


# Checking correlations ---------------------------------------------------
# Check correlations to see which variables will cause problems 
rankMatrix(cbind(gtHitsV, vapeHospCatV, cigSalesV, WeeksV, cdcEvali, trumpBan, masBan, fdaTHC, acetate, IcdcEvali, ItrumpBan, ImasBan, IfdaTHC, Iacetate))

cor(cbind(gtHitsV, vapeHospCatV, cigSalesV, WeeksV, cdcEvali, trumpBan, masBan, fdaTHC, acetate, IcdcEvali, ItrumpBan, ImasBan, IfdaTHC, Iacetate))

# CDC evali and trump ban
# Mas ban, FDA thc 


#timeDummies <- us100kdeath+us2Mcase+global1Mdeath+global40Mcase
#weeksDum <- Weeks*timeDummies

variablesV <- cbind(gtHitsV, vapeHospCatV, cigSalesV, WeeksV, cdcEvali, trumpBan, masBan, fdaTHC, acetate, IcdcEvali, ItrumpBan, ImasBan, IfdaTHC, Iacetate)
cor(variablesV)

# cbind(gtHits, vapeHosp, cigSales, covidCases, covidDeaths, Weeks, us100kdeath, us2Mcase, global1Mdeath, global40Mcase, weeksglobal1Mdeath, weeksglobal40Mcase, weeksus100kdeath, weeksus2Mcase)
# Fit model 

fitTSV <- Arima(tsVapeSalesV, 
                order = c(1,1,0),
                xreg = cbind(vapeHospCatV, lCigSalesV, WeeksV, cdcEvali, trumpBan, fdaTHC, acetate, WE, WT, WF, WA), 
                include.constant = F)

summary(fitTSV)
confint(fitTSV, level = 0.95)
coeftest(fitTSV)


# Checking the differences between new and old method ---------------------
old <- Arima(tsVapeSalesV, 
             order = c(1,1,0),
             xreg = cbind(vapeHospCatV, lCigSalesV, WeeksV, cdcEvali, trumpBan, fdaTHC, acetate, WE, WT, WF, WA), 
             include.constant = F)

new <- Arima(tsVapeSalesV, 
             order = c(1,1,0),
             xreg = cbind(vapeHospCatV, lCigSalesV, WeeksV, cdcEvali, trumpBan, fdaTHC, acetate,  IcdcEvali, ItrumpBan, IfdaTHC, Iacetate), 
             include.constant = F)

summary(old)
confint(old, level = 0.95)
coeftest(old)

summary(new)
confint(new, level = 0.95)
coeftest(new)


vapeITS$modelOld <-  as.numeric(fitted(old))
vapeITS$upperOld <- as.numeric(fitted(old) + 1.96*sqrt(old$sigma2)) #confidence intervals
vapeITS$lowerOld <- as.numeric(fitted(old) - 1.96*sqrt(old$sigma2))

vapeITS$modelNew <-  as.numeric(fitted(new))
vapeITS$upperNew <- as.numeric(fitted(new) + 1.96*sqrt(new$sigma2)) #confidence intervals
vapeITS$lowerNew <- as.numeric(fitted(new) - 1.96*sqrt(new$sigma2))


plotOld <- ggplot(vapeITS, aes(x=X)) + 
  geom_ribbon(aes(ymin = lowerOld, ymax = upperOld), fill = "grey70") + 
  geom_line(aes(y = modelOld, colour = 1)) + 
  geom_point(aes(y = log(revenueV))) +
  geom_vline(xintercept = 33, linetype = "dotted") + 
  geom_vline(xintercept = 37, linetype = "dotted") + 
  geom_vline(xintercept = 40, linetype = "dotted") + 
  geom_vline(xintercept = 45, linetype = "dotted") + 
  scale_x_continuous(name = "Weeks") + 
  scale_y_continuous(name = "Log Vape Sales") + 
  ggplot2::annotate("label", x = 33, y = 18.6, label = "a", size = 4) + 
  ggplot2::annotate("label", x = 37, y = 18.6, label = "b", size = 4) + 
  ggplot2::annotate("label", x = 40, y = 18.6, label = "c", size = 4) +
  ggplot2::annotate("label", x = 45, y = 18.6, label = "d", size = 4) + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"))

plotNew <- ggplot(vapeITS, aes(x=X)) + 
  geom_ribbon(aes(ymin = lowerNew, ymax = upperNew), fill = "grey70") + 
  geom_line(aes(y = modelNew, colour = 1)) + 
  geom_point(aes(y = log(revenueV))) +
  geom_vline(xintercept = 33, linetype = "dotted") + 
  geom_vline(xintercept = 37, linetype = "dotted") + 
  geom_vline(xintercept = 40, linetype = "dotted") + 
  geom_vline(xintercept = 45, linetype = "dotted") + 
  scale_x_continuous(name = "Weeks") + 
  scale_y_continuous(name = "Log Vape Sales") + 
  ggplot2::annotate("label", x = 33, y = 18.6, label = "a", size = 4) + 
  ggplot2::annotate("label", x = 37, y = 18.6, label = "b", size = 4) + 
  ggplot2::annotate("label", x = 40, y = 18.6, label = "c", size = 4) +
  ggplot2::annotate("label", x = 45, y = 18.6, label = "d", size = 4) + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"))

grid.arrange(plotOld, plotNew, nrow = 2)

par(mfrow=c(1,2))
plotOld
plotNew

# Graphs ------------------------------------------------------------------
par(mfrow=c(1,1))
plot(fitTSV)
plot(fitted(fitTSV))

vapeITS$model <- as.numeric(fitted(fitTSV))
vapeITS$upper <- as.numeric(fitted(fitTSV) + 1.96*sqrt(fitTSV$sigma2)) #confidence intervals
vapeITS$lower <- as.numeric(fitted(fitTSV) - 1.96*sqrt(fitTSV$sigma2))

plotEVALI <- ggplot(vapeITS, aes(x=X)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") + 
  geom_line(aes(y = model, colour = 1)) + 
  geom_point(aes(y = log(revenueV))) +
  geom_vline(xintercept = 33, linetype = "dotted") + 
  geom_vline(xintercept = 37, linetype = "dotted") + 
  geom_vline(xintercept = 40, linetype = "dotted") + 
  geom_vline(xintercept = 45, linetype = "dotted") + 
  scale_x_continuous(name = "Weeks") + 
  scale_y_continuous(name = "Log Vape Sales") + 
  ggplot2::annotate("label", x = 33, y = 18.6, label = "a", size = 4) + 
  ggplot2::annotate("label", x = 37, y = 18.6, label = "b", size = 4) + 
  ggplot2::annotate("label", x = 40, y = 18.6, label = "c", size = 4) +
  ggplot2::annotate("label", x = 45, y = 18.6, label = "d", size = 4) + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"))

# breaks= c(1, 5, 8, 13, 17, 21, 26, 30, 35, 40, 44, 49),
 #labels = c("Jan 2019", "Feb 2019", "Mar 2019", "Apr 2019", "May 2019", "Jun 2019", "Jul 2019", "Aug 2019", "Sep 2019", "Oct 2019", "Nov 2019", "Dec 2019")
# Covid ITS ---------------------------------------------------------------
covidITS <- itsData[which(itsData$weekEnd > "2019-12-28"),]

# create the time series 
tsVapeSalesC <- ts(log(covidITS[,5]), freq = 7)
plot(tsVapeSalesC)
abline(reg=lm(tsVapeSalesC~time(tsVapeSalesC)))
cycle(tsVapeSalesC)
plot(aggregate(tsVapeSalesC,FUN=mean))
boxplot(tsVapeSalesC~cycle(tsVapeSalesC))

# Examine the time series before Covid - March 11 2020, which is in row 11 
noiseC <- ts(covidITS[1:11,5], freq = 52)
# noise <- ts(itsData[1:53,5], freq = 7) - uses dec 31
plot(noiseC)

# decompose to see how the different components contribute to the time series
plot(decompose(noiseC),xlab="Weeks")

# check the contribution of each component to the overall variablility
mean(noiseC)/sd(noiseC)
(1-sd(noiseC-na.omit(decompose(noiseC)$trend))/sd(noiseC) )*100
(1-sd(noiseC-na.omit(decompose(noiseC)$seasonal))/sd(noiseC) )*100
(1-sd(na.omit(decompose(noiseC)$random))/sd(noiseC) )*100

# Check for stationarity
par(mfrow=c(2,1))
acf(noiseC,main="Sensitivity analysis of the vape sales")
pacf(noiseC,main="Sensitivity analysis of vape sales")
print(acf(noiseC,main="Sensitivity analysis of the vape sales"))
print(pacf(noiseC,main="Sensitivity analysis of the vape sales"))

# unit root test for stationarity
adf.test(noise)

# difference to remove trend
diff1 <- diff(noise)
par(mfrow=c(1,1))
plot(diff1,main="First order differencing")

# check for the stationarity of the difference data
par(mfrow=c(2,1))
acf(diff1,main="ACF after first order differencing")
pacf(diff1,main="PACF after first order differencing")
adf.test(diff1)

# trend
mean(diff1)/sd(diff1)
## fit autoregressive moving average models for the noise series. Use auto to detect the model based on AIC and BIC
arima_aic <- auto.arima(noise,trace=TRUE,test="kpss",ic="aic")
summary(arima_aic)
confint(arima_aic)
arima_bic <- auto.arima(noise,trace=TRUE,test="kpss",ic="bic")
summary(arima_bic)
confint(arima_bic)

# Arima_AIC AIC = 2049.14 < Arima_BIC = 2053.77, so chose aic (2,1,0)
# With Dec 31 as start date, arima suggection is (0,2,1)


# Create categorical variables to address NAs -----------------------------
  # Vape hospitalisations
  # Replace NAs with 0s, create deciles and label from 0 to 9 0  0 includes NAs and the bottom 10%
itsData$vapeHospCat <- (ntile(itsData$vapeHospitalisations, 10)-1) %>%
  replace_na(0)

  # Covid cases 
itsData$covidCasesCat <- (ntile(itsData$meanCovidCases, 10)-1) %>%
  replace_na(0)

itsData$covidDeathsCat <- (ntile(itsData$meanCovidDeaths, 10)-1) %>%
  replace_na(0)

# Define intervention variables
# 17 August 2019,  CDC announced that they would be actively investigating approximately 94 cases of vaping-related illnesses in 14 states = week 33
# 11 September 2019, Trump administration considers ban on vaping products = week 37
# 24 September 2019, masachusettes bans vaping products = week 39
# 4 October 2019, The FDA warned consumers not to use any THC-containing vapes = week 40
# November 8 2019, Vitamin E acetate responsible for EVALI (CDC announcement) = week 45
##1 Jan 2020, “Covid-19 first reported to the WHO” = week 53
##11 March 2020,  “WHO declares Covid-19 as pandemic” = week 63
# May 28 — US COVID-19 Deaths Pass the 100,000 Mark = week 74
# June 10 — US COVID-19 Cases Reach 2 Million = week 76
# July 7 — US Surpasses 3 Million = week 80
# September 28 — Global COVID-19 Deaths Surpass 1 Million Infections, Begins WHO Withdrawal = week 92
# October 19 — Global Cases Top 40 Million = week 95

gtHits <- itsData$relativeHits
vapeHosp <- itsData$vapeHospitalisations
vapeHospCat <- itsData$vapeHospCat
cigSales <- itsData$revenueC
covidCases <- itsData$meanCovidCases
covidDeaths <- itsData$meanCovidDeaths
covidCasesCat <- itsData$covidCasesCat
covidDeathsCat <- itsData$covidDeathsCat

Weeks <- 1:96
cdcEvali <- as.numeric(1*seq(tsVapeSales) > 33)
trumpBan <- as.numeric(1*seq(tsVapeSales) > 37)
masBan <- as.numeric(1*seq(tsVapeSales) > 39)
fdaTHC <- as.numeric(1*seq(tsVapeSales) > 40)
acetate <- as.numeric(1*seq(tsVapeSales) > 45)
covidWHO <- as.numeric(1*seq(tsVapeSales) > 53)
covidPandemic <- as.numeric(1*seq(tsVapeSales) > 63)
us100kdeath <- as.numeric(1*seq(tsVapeSales) > 74)
us2Mcase <- as.numeric(1*seq(tsVapeSales) > 76)
global1Mdeath  <- as.numeric(1*seq(tsVapeSales) > 92)
global40Mcase  <- as.numeric(1*seq(tsVapeSales) > 95)

weeksus100kdeath <- Weeks * us100kdeath
weeksus2Mcase <- Weeks * us2Mcase
weeksglobal1Mdeath <- Weeks * global1Mdeath
weeksglobal40Mcase <- Weeks * global40Mcase


# Checking correlations ---------------------------------------------------
# Check correlations to see which variables will cause problems 
rankMatrix(cbind(gtHits, vapeHospCat, covidCasesCat, covidDeathsCat, cigSales,Weeks, cdcEvali, trumpBan, masBan, fdaTHC, acetate, covidWHO, covidPandemic, us100kdeath, us2Mcase, global1Mdeath, global40Mcase))

cor(cbind(gtHits, vapeHospCat, covidCasesCat, covidDeathsCat, cigSales,Weeks, cdcEvali, trumpBan, masBan, fdaTHC, acetate, covidWHO, covidPandemic, us100kdeath, us2Mcase, global1Mdeath, global40Mcase))

  # CDC evali and trump ban
  # Mas ban, FDA thc 
  # acetate, fdaTHX
  # 100K deaths and 2M cases 
  # covid cases cat and US 2M
  # covid pandemic and covid cases cat
  # covid cases cat and us 100k
  # weeks and covid WHO
  # cdc evali and fda thc
  # fdathc and weeks  
  
#timeDummies <- us100kdeath+us2Mcase+global1Mdeath+global40Mcase
#weeksDum <- Weeks*timeDummies

variables <- cbind(gtHits, vapeHospCat, covidDeathsCat, cigSales, Weeks, cdcEvali, acetate, covidWHO, covidPandemic, us100kdeath, global1Mdeath, global40Mcase)
cor(variables)

# cbind(gtHits, vapeHosp, cigSales, covidCases, covidDeaths, Weeks, us100kdeath, us2Mcase, global1Mdeath, global40Mcase, weeksglobal1Mdeath, weeksglobal40Mcase, weeksus100kdeath, weeksus2Mcase)
# Fit model 

fitTS <- Arima(tsVapeSales, order = c(2,1,0), xreg = variables, include.constant = F)
summary(fitTS)
confint(fitTS, level = 0.95)
coeftest(fitTS)

fitTS <- Arima(tsVapeSales, order = c(2,1,0), xreg = cbind(gtHits, vapeHosp, cigSales, covidCases, covidDeaths, Weeks, timeDummies, weeksDum), include.constant = F,)


summary(fitTS)
confint(fitTS, level = 0.95)
coeftest(fitTS)

# Graphs ------------------------------------------------------------------
par(mfrow=c(1,1))
plot(fitTS)
plot(fitted(fitTS))

all$model <- as.numeric(fitted(fitTS))
all$upper <- as.numeric(fitted(fitTS) + 1.96*sqrt(fitTS$sigma2)) #confidence intervals
all$lower <- as.numeric(fitted(fitTS) - 1.96*sqrt(fitTS$sigma2))


