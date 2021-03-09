library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(GGally)
library(scales)
library(gridExtra)
library(readr)
library(maps)
library(viridis)
library(hrbrthemes)
library(plyr)
library(shiny)
library(leaflet)
library(leafpop)

# reading the lockdown data, which has the dates when individuall countries went on lockdown
lockdownData <- read.csv("countryLockdowndates.csv")

# checking the data
str(newd)
head(lockdownData)
summary(lockdownData$Type)

# some countries had data related to its states and provinces, so taking those out separately first
newd <-  lockdownData %>% group_by(Country.Region) %>% filter(n()>1)
newd$Date <- dmy(newd$Date) # formatting the date

# working on the above extracted data, considering that a country imposed some form of lockdown if a lockdown date is present for any of the provinces or states 
aggd <- aggregate(Date ~ Country.Region, newd, function(x) min(x))
aggd$Type <- "Partial"
aggd

# opposite of in
`%nin%` = Negate(`%in%`)

# taking out the countries not in the newly created dataframe
lockdownDataCountry <- lockdownData[(lockdownData$Country.Region %nin% aggd$Country.Region),]
drops <- c("Province","Reference") # list of columns to drop and then dropping it
lockdownDataCountry <- lockdownDataCountry[ , !(names(lockdownDataCountry) %in% drops)]
lockdownDataCountry$Date <- dmy(lockdownDataCountry$Date) # formatting the date

# now combining the two, which now only has one single entry of every country with its lockdown date
clean_lockdown_data <- rbind(lockdownDataCountry,aggd)
clean_lockdown_data <- clean_lockdown_data[order(clean_lockdown_data$Country.Region),]

clean_lockdown_data

unique(clean_lockdown_data$Country.Region)

# if any date is present then setting the type to 0 else 1
clean_lockdown_data$Type <- ifelse(clean_lockdown_data$Type == "None", 0, 1)

clean_lockdown_data


rownames(clean_lockdown_data) <- 1:nrow(clean_lockdown_data)

# write.csv(clean_lockdown_data, "clean_lockdown_data.csv")
#################################################################################################################
# reading the data which has confirmed case count for countires and also based on region
covid_cases <- read.csv("who_covid_19_sit_rep_time_series.csv")

head(covid_cases)

covid_global <- covid_cases[1:2,] # contains total count globally

confirmed_covid_country <- covid_cases[41:246,] # contains total count country wise

confirmed_covid_region <- covid_cases[248:259,] # contains total count region wise

########
# formatting the extra columns in the formm of dates to a column date and its respective case count to cases
covid_global <- covid_global %>% pivot_longer(-c(Province.States,Country.Region,WHO.region), names_to = "Date", values_to = "cases")
str(covid_global)

# removed the extra characters from the date column and converted it into the correct format
covid_global$Date <- gsub('^.', '', covid_global$Date) 
covid_global$Date <- mdy(covid_global$Date)
covid_global[is.na(covid_global)] <- 0
########
# checking which country names are different in both or not there
confirmed_covid_country <- confirmed_covid_country[ , !(names(confirmed_covid_country) == "Province.States")]
head(confirmed_covid_country)

unique(confirmed_covid_country$Country.Region)

# fixing the spellings of the countries to get the data for all the countries
confirmed_covid_country$Country.Region <- as.character(confirmed_covid_country$Country.Region)
confirmed_covid_country$Country.Region[confirmed_covid_country$Country.Region == "Republic of Korea"] <- "South Korea"
confirmed_covid_country$Country.Region[confirmed_covid_country$Country.Region == "The United Kingdom"] <- "United Kingdom"
confirmed_covid_country$Country.Region[confirmed_covid_country$Country.Region == "Russian Federation"] <- "Russia"
confirmed_covid_country$Country.Region[confirmed_covid_country$Country.Region == "Republic of Moldova*"] <- "Moldova"
confirmed_covid_country$Country.Region[confirmed_covid_country$Country.Region == "Syrian Arab Republic"] <- "Syria"
confirmed_covid_country$Country.Region[confirmed_covid_country$Country.Region == "United States of America"] <- "United States"
confirmed_covid_country$Country.Region[confirmed_covid_country$Country.Region == "Bahamas"] <- "The Bahamas"
confirmed_covid_country$Country.Region[confirmed_covid_country$Country.Region == "CÃ´te dâ€™Ivoire"] <- "CÃ´te d'Ivoire"

unique(clean_lockdown_data$Country.Region)

# doing the same for the other dataset
clean_lockdown_data$Country.Region <- as.character(clean_lockdown_data$Country.Region)
clean_lockdown_data$Country.Region[clean_lockdown_data$Country.Region == "Korea, South"] <- "South Korea"
clean_lockdown_data$Country.Region[clean_lockdown_data$Country.Region == "US"] <- "United States"

# checking if I have all the countires required
confirmed_covid_country <- confirmed_covid_country[(confirmed_covid_country$Country.Region %in% clean_lockdown_data$Country.Region),]
new_clean_lockdown_data <- clean_lockdown_data[(clean_lockdown_data$Country.Region %in% confirmed_covid_country$Country.Region),]
unique(confirmed_covid_country$Country.Region)

confirmed_covid_country

# formatting the extra columns in the formm of dates to a column date and its respective case count to cases and formatting the data as well
confirmed_covid_country <- confirmed_covid_country %>% pivot_longer(-c(Country.Region,WHO.region), names_to = "Dates", values_to = "Cases")
str(confirmed_covid_country)
confirmed_covid_country$Dates <- gsub('^.', '', confirmed_covid_country$Dates)
confirmed_covid_country$Dates <- mdy(confirmed_covid_country$Dates)
confirmed_covid_country[is.na(confirmed_covid_country)] <- 0


head(confirmed_covid_country)
head(new_clean_lockdown_data)
unique(confirmed_covid_country$Country.Region)
unique(new_clean_lockdown_data$Country.Region)

# merging the 2 cleaned and formatted dataframes 
comb_confirmed_covid_country <- merge(confirmed_covid_country, new_clean_lockdown_data, by.x="Country.Region", by.y="Country.Region")
comb_confirmed_covid_country$Date_Flag <- !(comb_confirmed_covid_country$Dates < comb_confirmed_covid_country$Date)
comb_confirmed_covid_country$Date_Flag <- as.integer(as.logical(comb_confirmed_covid_country$Date_Flag))
comb_confirmed_covid_country <- comb_confirmed_covid_country[ , !(names(comb_confirmed_covid_country) %in% dropsAgainNew)]

# creating a new column which will have a flag based on Date_Flag column
comb_confirmed_covid_country$lockdown <- ifelse(is.na(comb_confirmed_covid_country$Date_Flag), 0, 1)

# setting all NA values to 0
comb_confirmed_covid_country[is.na(comb_confirmed_covid_country)] <- 0
head(comb_confirmed_covid_country)

# reading the latitude longitude dataset
lat_long_covid_cases <- read.csv("WHO COVID Cases Data - covid-19 cases by country.csv")
head(lat_long_covid_cases)

# dropping the useless columns
dropShit <- c("DateOfReport","cum_conf","cum_death","ID","ADM0_VIZ_NAME","Short_Name_ZH","Short_Name_FR","Short_Name_RU","Short_Name_ES","Short_Name_AR")
lat_long_covid_cases <- lat_long_covid_cases[ , !(names(lat_long_covid_cases) %in% dropShit)]

# correcting the country names to not miss out on any
lat_long_covid_cases$ADM0_NAME <- as.character(lat_long_covid_cases$ADM0_NAME)
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "Republic of Korea"] <- "South Korea"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "The United Kingdom"] <- "United Kingdom"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "Russian Federation"] <- "Russia"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "Syrian Arab Republic"] <- "Syria"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "United States of America"] <- "United States"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "Bahamas"] <- "The Bahamas"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "CÃ´te dâ€™Ivoire"] <- "CÃ´te d'Ivoire"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "Viet Nam"] <- "Vietnam"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "Kosovo[1]"] <- "Kosovo"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "Iran (Islamic Republic of)"] <- "Iran"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
lat_long_covid_cases$ADM0_NAME[lat_long_covid_cases$ADM0_NAME == "Bolivia (Plurinational State of)"] <- "Bolivia"


lat_long_country <- lat_long_covid_cases[(lat_long_covid_cases$ADM0_NAME %in% comb_confirmed_covid_country$Country.Region),]

# merging the lat long data with the previously merged data
comb_confirmed_covid_country <- merge(comb_confirmed_covid_country, lat_long_country, by.x="Country.Region", by.y="ADM0_NAME")

head(comb_confirmed_covid_country)

# write.csv(comb_confirmed_covid_country, "comb_confirmed_covid_country.csv")

# taking out countries with no lockdown and then aggregating it based on the maximum case count value
cv_no_lockdown_plot <- comb_confirmed_covid_country %>% filter(lockdown == 0)
unique(cv_no_lockdown_plot$Country.Region)
cv_no_lockdown_plot <- as.data.frame(aggregate(Cases ~ Country.Region+CENTER_LAT+CENTER_LON, data = cv_no_lockdown_plot, max))
cv_no_lockdown_plot$lockdown <- 0

# taking out countries with lockdown and then aggregating it based on the maximum case count value
cv_lockdown_plot <- comb_confirmed_covid_country %>% filter(lockdown == 1)
cv_lockdown_plot <- as.data.frame(aggregate(Cases ~ Country.Region+CENTER_LAT+CENTER_LON, data = cv_lockdown_plot, max))
cv_lockdown_plot$lockdown <- 1

# combining the two for plotting the world map in the app
country_lockdown_condition_data <- rbind(cv_no_lockdown_plot, cv_lockdown_plot)

head(country_lockdown_condition_data)

# write.csv(country_lockdown_condition_data, "country_lockdown_condition_data.csv")

# formatting the data
confirmed_covid_region_copy <- confirmed_covid_region
confirmed_covid_region_copy <- confirmed_covid_region_copy %>% pivot_longer(-c(Province.States,Country.Region,WHO.region), names_to = "Date", values_to = "cases")
confirmed_covid_region_copy$Date <- gsub('^.', '', confirmed_covid_region_copy$Date)
confirmed_covid_region_copy$Date <- mdy(confirmed_covid_region_copy$Date)
confirmed_covid_region_copy[is.na(confirmed_covid_region_copy)] <- 0
confirmed_covid_region_copy <- confirmed_covid_region_copy[ , !(names(confirmed_covid_region_copy) == "Country.Region")]
names(confirmed_covid_region_copy)[names(confirmed_covid_region_copy) == "Province.States"] <- "Case.Status"
head(confirmed_covid_region_copy)

# write.csv(confirmed_covid_region_copy, "confirmed_covid_region_copy.csv")

##############################################################################
#####GOOGLE MOBILITY REPORT
##############################################################################

# reading the mobility data
global_mobility <- read.csv("Global_Mobility_Report.csv")

# removing one column at a time so that do not miss out on any important information
global_mobility_report <- global_mobility[(global_mobility$sub_region_2 == ""),]

global_mobility_report <- global_mobility_report[(global_mobility_report$sub_region_1 == ""),]

# dropping the uselss columns 
dropsNew <- c("sub_region_1","sub_region_2")
global_mobility_report <- global_mobility_report[ , !(names(global_mobility_report) %in% dropsNew)]

# formatting the date
global_mobility_report$date <- ymd(global_mobility_report$date)

mobility <- global_mobility_report

# renaming the column names into much more readable format
mobility <- rename(mobility, c("retail_and_recreation_percent_change_from_baseline"="retail_recreation", "grocery_and_pharmacy_percent_change_from_baseline"="grocery_pharmacy",
                               "parks_percent_change_from_baseline"="parks", "transit_stations_percent_change_from_baseline"="transit_stations",
                               "workplaces_percent_change_from_baseline"="workplaces", "residential_percent_change_from_baseline"="residential"))

# dropping two more columns
dropsAgainNew <- c("parks","country_region_code")
mobility <- mobility[ , !(names(mobility) %in% dropsAgainNew)]

head(mobility)

# write.csv(mobility, "clean_mobility_data.csv")