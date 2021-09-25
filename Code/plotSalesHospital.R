# Author: Kamila Janmohamed
# Purpose: Plot EVALI hospitalisations over vape sales for 2019

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

itsData <- read.csv("Data/Final/vapeSalesITS.csv", header = T) %>%
  filter(X <= 53)

par(mfrow=c(2,1))
a<- ggplot(itsData, aes(x = X, y = revenueV)) + 
  geom_point() +
  scale_x_continuous(name = "Weeks", 
                     breaks = seq(1,53,4),
                     labels = c("12 Dec 2018 \n(Week 1)", "27 Jan 2019 \n(Week 5)", "24 Feb 2019 \n(Week 9)", "24 Mar 2019 \n(Week 13)", "21 Apr 2019 \n(Week 17)", "19 May 2019 \n(Week 21)", "16 Jun 2019 \n(Week 25)", " 14 Jul 2019 \n(Week 29)", "11 Aug 2019 \n(Week 33)", "8 Sep 2019 \n(Week 37)", "6 Oct 2019 \n(Week 41)", "3 Nov 2019 \n(Week 45)", "1 Dec 2019 \n(Week 49)", "22 Dec 2019 \n(Week 52)")) + 
  scale_y_continuous(name = "Vape sales") + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"))

b<- ggplot(itsData, aes(x = X, y = vapeHospitalisations)) + 
  geom_point() +
  scale_x_continuous(name = "Weeks", 
                     breaks = seq(1,53,4),
                     labels = c("12 Dec 2018 \n(Week 1)", "27 Jan 2019 \n(Week 5)", "24 Feb 2019 \n(Week 9)", "24 Mar 2019 \n(Week 13)", "21 Apr 2019 \n(Week 17)", "19 May 2019 \n(Week 21)", "16 Jun 2019 \n(Week 25)", " 14 Jul 2019 \n(Week 29)", "11 Aug 2019 \n(Week 33)", "8 Sep 2019 \n(Week 37)", "6 Oct 2019 \n(Week 41)", "3 Nov 2019 \n(Week 45)", "1 Dec 2019 \n(Week 49)", "22 Dec 2019 \n(Week 52)")) + 
  scale_y_continuous(name = "EVALI hospitalisations") + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"))

ggarrange(a,b, ncol = 1, nrow = 2)
ggsave("Figures/salesHosp.png", width=9, height=8)


ggplot(itsData, aes(x = log(revenueV), y = vapeHospitalisations)) + 
  geom_point() +
  scale_x_continuous(name = "Log Vape Sales") + 
  scale_y_continuous(name = "EVALI hospitalisations") + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin=unit(c(0.75,0.75,0.75,0.75),"cm"))
ggsave("Figures/salesVHosp.png", width=9, height=4)
