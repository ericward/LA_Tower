library(dplyr)
library(tidyr)
library(gt)
library(lubridate)

setwd("C:/Users/ejward2/OneDrive - NASA/Desktop/Tower Data/Melinda/LA_Tower/Data")

list.files(,pattern="csv")

la2<-read.csv("Daily_Gapfilled_LA2.csv")
head(la2)
summary(la2 %>% select(FC_filled, FCH4_filled, LE_filled, Salinity))

la3<-read.csv("Daily_Gapfilled_LA3.csv")
head(la3)
summary(la3 %>% select(FC_filled, FCH4_filled, LE_filled, Salinity))
str(la3)

###### Annual Summary #####

# Average by Year with standard deviation
# US-LA3
la3_annual_summary <- la3 %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    CO2_mean = mean(FC_filled, na.rm = TRUE),
    CO2_sd   = sd(FC_filled, na.rm = TRUE),
    
    CH4_mean = mean(FCH4_filled, na.rm = TRUE),
    CH4_sd   = sd(FCH4_filled, na.rm = TRUE),
    
    LE_mean  = mean(LE_filled, na.rm = TRUE),
    LE_sd    = sd(LE_filled, na.rm = TRUE),
    
    Days = n()
  )

la3_annual_summary

la2_annual_summary <- la2 %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    CO2_mean = mean(FC_filled, na.rm = TRUE),
    CO2_sd   = sd(FC_filled, na.rm = TRUE),
    
    CH4_mean = mean(FCH4_filled, na.rm = TRUE),
    CH4_sd   = sd(FCH4_filled, na.rm = TRUE),
    
    LE_mean  = mean(LE_filled, na.rm = TRUE),
    LE_sd    = sd(LE_filled, na.rm = TRUE),
    
    Days = n()
  )

la2_annual_summary

# Cumulative Summary by Year 
mwCH4<-16.043 #g/mol
nmol2gCH4<-1/(10^6*mwCH4)
mwCO2<-44.009 #g/mol
mmol2gCH4<-1/(10^3*mwCO2)
d2s<-(24*60*60) #seconds per day
ucCH4<-nmol2gCH4*d2s #nmol m-2 s-1 to g m-2 y-1
ucCO2<-mmol2gCH4*d2s #nmol m-2 s-1 to g m-2 y-1

la3_annual_cumulative <- la3 %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    CO2_cumulative = sum(FC_filled, na.rm = TRUE)*ucCO2,
    CH4_cumulative = sum(FCH4_filled, na.rm = TRUE)*ucCH4,
    LE_cumulative  = sum(LE_filled, na.rm = TRUE),
    Days = n()
  )

la3_annual_cumulative

la2_annual_cumulative <- la2 %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    CO2_cumulative = sum(FC_filled, na.rm = TRUE)*ucCO2,
    CH4_cumulative = sum(FCH4_filled, na.rm = TRUE)*ucCH4,
    LE_cumulative  = sum(LE_filled, na.rm = TRUE),
    Days = n()
  )

la2_annual_cumulative
