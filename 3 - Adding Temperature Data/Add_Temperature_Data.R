#Adding temperature data

#Set working directory
setwd("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/3 - Adding Temperature Data")

#Load packages
library(dplyr)
library(R.utils)
library(raster)

#Import dataset and select variables on interest
merged_vaccinium_clean <- read.csv("clean_data.csv")
merged_vaccinium_clean <- merged_vaccinium_clean %>% 
  select(!(X))

#Convert date column to a date data type
merged_vaccinium_clean$eventDate<- as.Date(merged_vaccinium_clean$eventDate)

# Unzipping the dataset, the file in this folder is already unzipped so you can
#skip this step unless it's a fresh download
gunzip("cru_ts4.08.1901.2023.tmp.dat.nc.gz",
       remove = TRUE, overwrite = TRUE)

#Import temperature data
temperature_data <- raster::brick('cru_ts4.08.1901.2023.tmp.dat.nc')

#There are 1476 temperatures (nlayers), one for each month from across 122 years 
nlayers(temperature_data) 

#Temperature data doesn't cover any dates before 1901-01-01 so anything before then has 
#to be removed. Removed 3 observations.
sort(merged_vaccinium_clean$eventDate)[1]
vaccinium<- merged_vaccinium_clean %>% 
  filter(year >= 1901)

#Temperature data doesn't cover any dates after 2023 so anything after then has 
#to be removed. Removed 217 observations.
vaccinium<- vaccinium %>% 
  filter(year != 2024)

#Filter out any without a year
#Nothing removed
vaccinium <- vaccinium %>% filter(!is.na(vaccinium$year))

#Filter out any without lat and long 
#Nothing removed
vaccinium <- vaccinium %>% filter(decimalLatitude != 0) %>% filter(decimalLongitude != 0)

#How many samples do we have left for each dataset?
#ECSmith = 178, iNaturalist = 54, Online Herbarium = 700
table(vaccinium$dataset)

#Figure out which months we have data for
unique(merged_vaccinium_clean$month) #4, 5, 6, 7, 8, 9
#We have data for April, May, June, July, August, September 

#Order based on year
vaccinium <- vaccinium %>% arrange(year)

#Create empty vectors to store temperature data
march_temperature_data<- numeric(nrow(vaccinium))
april_temperature_data<- numeric(nrow(vaccinium))
may_temperature_data<- numeric(nrow(vaccinium))
june_temperature_data<- numeric(nrow(vaccinium))
july_temperature_data<- numeric(nrow(vaccinium))
august_temperature_data<- numeric(nrow(vaccinium))
sept_temperature_data<- numeric(nrow(vaccinium))

# Loop through each row in the coordinate file
for (i in 1:nrow(vaccinium)) {
  year <- vaccinium$year[i]
  lat <- vaccinium$decimalLatitude[i]
  lon <- vaccinium$decimalLongitude[i]
  
  # Calculate the index for the given year
  layer_march_index <- (year - 1901) * 12 + 3
  layer_april_index <- (year - 1901) * 12 + 4
  layer_may_index <- (year - 1901) * 12 + 5
  layer_june_index <- (year - 1901) * 12 + 6
  layer_july_index <- (year - 1901) * 12 + 7
  layer_august_index <- (year - 1901) * 12 + 8
  layer_sept_index <- (year - 1901) * 12 + 9
  
  # Extract the temperature value for the given coordinates
  point <- SpatialPoints(cbind(lon, lat))
  temp_value_march <- raster::extract(temperature_data[[layer_march_index]], point)
  temp_value_april <- raster::extract(temperature_data[[layer_april_index]], point)
  temp_value_may <- raster::extract(temperature_data[[layer_may_index]], point)
  temp_value_june <- raster::extract(temperature_data[[layer_june_index]], point)
  temp_value_july <- raster::extract(temperature_data[[layer_july_index]], point)
  temp_value_august <- raster::extract(temperature_data[[layer_august_index]], point)
  temp_value_sept <- raster::extract(temperature_data[[layer_sept_index]], point)
  
  # Store the temperature value in the vector
  march_temperature_data[i] <- temp_value_march
  april_temperature_data[i] <- temp_value_april
  may_temperature_data[i] <- temp_value_may
  june_temperature_data[i] <- temp_value_june
  july_temperature_data[i] <- temp_value_july
  august_temperature_data[i] <- temp_value_august
  sept_temperature_data[i] <- temp_value_sept
}

# Add the extracted temperature data to the dataframe
vaccinium$march_temperature <- march_temperature_data
vaccinium$april_temperature <- april_temperature_data
vaccinium$may_temperature <- may_temperature_data
vaccinium$june_temperature <- june_temperature_data
vaccinium$july_temperature <- july_temperature_data
vaccinium$august_temperature <- august_temperature_data
vaccinium$sept_temperature <- sept_temperature_data

#Filter out any observations without temperature data
#Removed 3 observations
vaccinium2 <- vaccinium %>% 
  filter(!is.na(may_temperature)) %>% 
  filter(!is.na(april_temperature)) %>% 
  filter(!is.na (march_temperature)) %>% 
  filter(!is.na (june_temperature)) %>% 
  filter(!is.na (july_temperature))%>% 
  filter(!is.na (august_temperature)) %>% 
  filter(!is.na (sept_temperature))

#Save data now that temperature data has been added
write.csv(vaccinium2, "/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/3 - Adding Temperature Data/vaccinium_with_weather_data_clean.csv")

