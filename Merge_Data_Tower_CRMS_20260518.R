library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)

setwd("~/GitHub/LA_Tower")

# Increase the penalty for scientific notation
options(scipen = 999)

###### CRMS Data #####
# CRMS 3166 - Freshwater Site (US-LA2)
hydro_3166<-read.csv("Data/CRMS Data/CRMS_3166_Hydro_Hourly/CRMS_3166_Hydro_Hourly.csv",
                     check.names = F, stringsAsFactors = TRUE)
# CHeck column names
head(hydro_3166)
str(hydro_3166)

# There was an issue with the degree symbol so convert to UTF-8
colnames(hydro_3166) <- iconv(colnames(hydro_3166),
                              from = "latin1",
                              to = "UTF-8")
# Remove parentheses from column names
names(hydro_3166) <- gsub("\\s*\\([^\\)]+\\)", "", names(hydro_3166))

# Combine Date and Time Columns
hydro_3166$DateTime<-paste(hydro_3166$Date,hydro_3166$Time, sep=" ")
# Format new Date Time column
hydro_3166$DateTime<-as.POSIXct(hydro_3166$DateTime,format= "%m/%d/%Y %H:%M:%S")

# CRMS 0224 - Salt Marsh Site (US-LA3)
hydro_0224<-read.csv("Data/CRMS Data/CRMS_0224_Hydro_Hourly/CRMS_0224_Hydro_Hourly.csv",
                     check.names = F, stringsAsFactors = TRUE)

# Check column names
head(hydro_0224)
str(hydro_0224)

# There was an issue with the degree symbol so convert to UTF-8
colnames(hydro_0224) <- iconv(colnames(hydro_0224),
                              from = "latin1",
                              to = "UTF-8")
# Remove parentheses from column names
names(hydro_0224) <- gsub("\\s*\\([^\\)]+\\)", "", names(hydro_0224))

hydro_0224$DateTime<-paste(hydro_0224$Date,hydro_0224$Time, sep=" ")
hydro_0224$DateTime<-as.POSIXct(hydro_0224$DateTime,format= "%m/%d/%Y %H:%M:%S")
#hydro_0224 %>% filter(DateTime > "2024-02-28*" & DateTime < "2024-03-01*")

##### AmeriFlux Data #####

la2_amf<-read.csv("Data/AMF_US-LA2_BASE-BADM_4-5/AMF_US-LA2_BASE_HH_4-5.csv",
                      skip = 2, header = TRUE)
la3_amf<-read.csv("Data/AMF_US-LA3_BASE-BADM_2-5/AMF_US-LA3_BASE_HH_2-5.csv",
                      skip = 2, header = TRUE)

# Checking colnames
names(la2_amf)
names(la3_amf)
# Need to remove the "_1_1_1" from LA3 names
names(la3_amf) <- gsub("_1_1_1$", "", names(la3_amf))


# Editing dataframe
la2_amf$DateTime<-ymd_hm(la2_amf$TIMESTAMP_START)
la3_amf$DateTime<-ymd_hm(la3_amf$TIMESTAMP_START)

# Checking Data available
ggplot(la2_amf %>% filter(FC != -9999 & DateTime > "2020-01-01"), aes(x=DateTime, y=FC)) + geom_point()
ggplot(la3_amf %>% filter(FC != -9999), aes(x=DateTime, y=FC)) + geom_point()

##### Merging Tower and CRMS Hydro Data #####
# LA2 Site - CRMS 3166
head(hydro_3166)
colnames(hydro_3166)

# Find column numbers that start with "Adjusted"
adjusted_columns <-  select(hydro_3166, starts_with("Adjusted")) %>% names()

# checking range of dates between two datasets
range(la2_amf$DateTime)
range(hydro_3166$DateTime,na.rm = TRUE)

# Selecting only adjusted columns from hydo_3166 and dates after the start of la2_amf
hydro_3166_select_H01<-hydro_3166 %>% filter(`Station ID` == "CRMS3166-H01" & DateTime > "2011-01-01*") %>% 
  select(`Station ID`, DateTime, `Sensor Environment`, all_of(adjusted_columns))

range(hydro_3166_select_H01$DateTime,na.rm = TRUE)

# Checking Time Zones to make sure they match
attr(la2_amf$DateTime, "tzone")
attr(hydro_3166_select_H01$DateTime, "tzone")

# Forcing to be the same timezone UTC
la2_amf$DateTime <- force_tz(la2_amf$DateTime, tzone = "UTC")
hydro_3166_select_H01$DateTime <- force_tz(
  hydro_3166_select_H01$DateTime,
  tzone = "UTC"
)

# Merging the two datasets
la2_hydro<-left_join(la2_amf,hydro_3166_select_H01,by="DateTime")

# Checking correlations with quick plot
plot(FCH4~`Adjusted Water Elevation to Datum`, la2_hydro %>% filter(FCH4 != -9999))
plot(FC~`Adjusted Water Elevation to Datum`, la2_hydro %>% filter(FC != -9999))

# LA3 Site - CRMS 0224
head(hydro_0224)
colnames(hydro_0224)
unique(hydro_0224$`Station ID`)
# Find column numbers that start with "Adjusted"
adjusted_columns_224 <-  select(hydro_0224, starts_with("Adjusted")) %>% names()

range(la3_amf$DateTime)
range(hydro_0224$DateTime,na.rm = TRUE)

# Checking Time Zones to make sure they match
attr(la3_amf$DateTime, "tzone")
attr(hydro_0224$DateTime, "tzone")

# Forcing to be the same timezone UTC
la3_amf$DateTime <- force_tz(la3_amf$DateTime, tzone = "UTC")
hydro_0224$DateTime <- force_tz(
  hydro_0224$DateTime,
  tzone = "UTC"
)

hydro_0224_select_H01<-hydro_0224 %>% filter(DateTime >= as.POSIXct("2019-01-01 00:00:00", tz = "UTC")) %>% 
  select(`Station ID`, DateTime, `Sensor Environment`, all_of(adjusted_columns_224))

range(hydro_0224_select_H01$DateTime,na.rm = TRUE)

# Merging the two datasets
la3_hydro<-left_join(la3_amf,hydro_0224_select_H01,by="DateTime")
head(la3_hydro)
colnames(la3_hydro)

# Checking the merge
head(la3_amf %>% select(DateTime,FCH4, FC))
head(hydro_0224_select_H01 %>% select(DateTime,`Adjusted Water Level`))
head(la3_hydro %>% select(DateTime,`Adjusted Water Level`,FC,FCH4))

# Checking relationships with simple plot
plot(FCH4~`Adjusted Water Level`, la3_hydro %>% filter(FCH4 != -9999))
plot(FC~`Adjusted Water Level`, la3_hydro %>% filter(FC != -9999))


#### Adding GridMet Climate Data #####
la2_climate<-read_excel("Data/GridMet_LA2_All.xlsx",sheet = "Data")
la3_climate<-read_excel("Data/GridMet_LA3_All.xlsx",sheet = "Data")

# LA2 Site - Climate
head(la2_climate)
str(la2_climate)

la2_climate<-la2_climate %>% filter(Date > "2010-12-31")
head(la2_climate %>% select(Date,Precip))
head(la2_hydro %>% select(DateTime,FC,FCH4, `Adjusted Water Level`))


# Merge and fill in climate data for ever half hour
la2_hydro_climate <- la2_hydro %>%
  mutate(Date = as.Date(DateTime)) %>%
  left_join(
    la2_climate %>%
      mutate(Date = as.Date(Date)),
    by = "Date"
  ) %>%
  select(-Date)

# Checking merged dataset
head(la2_hydro_climate %>% select(DateTime, Precip, `Adjusted Water Level`,FCH4))
names(la2_hydro_climate)[names(la2_hydro_climate) == "WS.x"] <- "WS_Eddy"
names(la2_hydro_climate)[names(la2_hydro_climate) == "WS.y"] <- "WS_Grid"
str(la2_hydro_climate)

# LA3 Site - Climate
head(la3_climate)
str(la3_climate)

la3_climate<-la3_climate %>% filter(Date > "2018-12-31*")

# Merge and fill in climate data for ever half hour
la3_hydro_climate <- la3_hydro %>%
  mutate(Date = as.Date(DateTime)) %>%
  left_join(
    la3_climate %>%
      mutate(Date = as.Date(Date)),
    by = "Date"
  ) %>%
  select(-Date)
head(la3_hydro_climate)


names(la3_hydro_climate)[names(la3_hydro_climate) == "WS.x"] <- "WS_Eddy"
names(la3_hydro_climate)[names(la3_hydro_climate) == "WS.y"] <- "WS_Grid"


source("ec_gap_fill.R")
result <- run_gap_fill(la2_hydro_climate, out_file = "la2_filled.csv")
gap_fill_summary(result)


head(result)
ggplot() +
  # Original data (black), NA breaks the line
  geom_line(data = result, aes(x = DateTime, y = FCH4, color = "Original"), na.rm = FALSE) +
  # Filled data only where FCH4 is NA (red), NA breaks the line
  geom_line(
    data = result,
    aes(x = DateTime, y = FCH4_filled, color = "Filled"),
    na.rm = FALSE
  ) +
  scale_color_manual(values = c("Original" = "black", "Filled" = "red"))
