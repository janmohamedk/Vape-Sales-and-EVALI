# Author: Kamila Janmohamed
# Date: Feb 4 2021
# Purpose: Get, clean and merge vape and cigarette sales, google trends, US vape hospitalisations, covid cases

# Clean vape --------------------------------------------------------------
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

  # Load data ----
vape <- read_excel("Data/Raw/Nielsen_vape_data/vape_data.xlsx")
cig1 <- read_excel("Data/Raw/Nielsen_vape_data/cigarette_data_part1.xlsx")
cig2 <- read_excel("Data/Raw/Nielsen_vape_data/cigarette_data_part_2.xlsx")


  # Cleaning procedure ----
    # subset columns for date and revenue
    # convert period description to date
    # drop dates outside range
    # aggregate revenue by date

cleanV <- vape[,c(26,28)] %>%
  rename(period = `[Period Description Short]`,
         revenueV = `[$]`) %>%
  dplyr::mutate(weekEnd = as.Date(substr(period, start = 7, stop = 14), format = "%m/%d/%y")) %>%
  group_by(weekEnd) %>%
  summarise(revenueV = sum(revenueV, na.rm = TRUE))

cleanC <- rbind(cig1, cig2)[,c(26, 28)] %>%
  rename(period = `[Period Description Short]`,
         revenueC = `[$]`) %>%
  dplyr::mutate(weekEnd = as.Date(substr(period, start = 7, stop = 14), format = "%m/%d/%y")) %>%
  group_by(weekEnd) %>%
  summarise(revenueC = sum(revenueC, na.rm = TRUE))

cleanedNielsen <- left_join(cleanC, cleanV, by=c("weekEnd"))

cleanedNielsen$weekStart <- cleanedNielsen$weekEnd-6

cleanedNielsen$days <- difftime(cleanedNielsen$weekEnd, min(cleanedNielsen$weekEnd), units = "days")

write.csv(cleanedNielsen, "Data/Intermediate/cleanedNielsenFull.csv", row.names = FALSE)

# Get google trends -------------------------------------------------------
  # Vape data used week end, so get gTrends for preceeding week 30 Dec 2018 - 2020-10-31

  # terms e-cigarette, electronic cigarette, electronic cigarettes, electronic nicotine delivery, vape, vaping,  personal  vaporizer,  vape pen,  electric  cigarette,electric nicotine delivery system, e-hookah, e-juice, e-liquid

terms <- c("electronic cigarette", "electronic cigarettes", "electronic nicotine delivery", "vape", "vaping",  "personal vaporizer",  "vape pen",  "electric cigarette", "e-hookah", "e-juice", "e-liquid")

interest <- gtrends(keyword = c("e-cigarette"), geo="US", time = "2018-12-30 2020-10-31", onlyInterest=TRUE)$interest_over_time

for (term in terms) {
  interest1 <- gtrends(keyword = term, geo="US", time = "2018-12-30 2020-10-31", onlyInterest=TRUE)$interest_over_time
  interest <- rbind(interest, interest1, deparse.level = 1)
  Sys.sleep(15)
}

interest$date <- as.Date(interest$date)
interest$Days <- difftime(interest$date, min(interest$date), units = "days")
names(interest)[names(interest) == "date"] <- "weekStart"

write.csv(interest, "Data/Raw/googleTrendsFreq20181230_20201031.csv", row.names = F)

gTrends <- interest %>%
  group_by(weekStart) %>%
  summarise(totalHits = sum(hits, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(relativeHits = totalHits/(max(totalHits)))

gTrends$days <- difftime(gTrends$weekStart, min(gTrends$weekStart), units = "days")
  
pdf(file="Figures/gtVapeFreq20181230_20201031.pdf",width=22,height=12)
ggplot(data = gTrends, aes(x=days, y=totalHits)) +
  geom_line()
dev.off()

itsData <- left_join(cleanedNielsen, gTrends, by=c("weekStart"))

# Add US vape hospitalisations --------------------------------------------
vapeHosp <- read.csv("Data/Raw/vapingHospitalisation.csv", header = T)

  # subtract 1 to make sure dates match 
vapeHosp$weekEnd <- as.Date(vapeHosp$Date, format = "%m/%d/%y")-1

itsData <- left_join(itsData, vapeHosp, by=c("weekEnd"))
# Add covid cases/deaths --------------------------------------------------
covid <- read.csv("Data/Raw/Covariate Data/JHU-COVID-19-data.csv", header = T)

covid <- covid %>%
  filter(location == "United States") %>%
  select(date, new_cases, new_deaths)

covid$week <- strftime(covid$date, format = "%V")

covid <- covid %>%
  group_by(week) %>%
  summarise(meanCovidCases = mean(new_cases),
            meanCovidDeaths = mean(new_deaths))

covid$weekStart <- seq(as.Date("2020-01-19"), as.Date("2020-12-27"), by = "week")

itsData <- left_join(itsData, covid, by=c("weekStart"))

  # drop unwanted variables
names(itsData)
itsData <- itsData %>% 
  select(weekEnd, weekStart, revenueV, revenueC, totalHits, relativeHits, days.x, Hospitalisations, meanCovidCases, meanCovidDeaths) %>%
  rename(days = days.x, 
         vapeHospitalisations = Hospitalisations)

itsData <- itsData[,c(1,2,7,3,4,5,6,8,9,10)]

write.csv(itsData, "Data/Final/vapeSalesITS.csv")