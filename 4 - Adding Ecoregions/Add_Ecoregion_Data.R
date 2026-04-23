#Add ecoregion data

#Load packages
library(dplyr)
library(terra)
library(lubridate)

#Import blueberry data
vaccinium_clean_final_dataset <- read.csv("./4 - Adding Ecoregions/vaccinium_with_weather_data_clean.csv")
vaccinium_clean_final_dataset <- vaccinium_clean_final_dataset %>% 
  select(!(X))

#Import ecozone data
ecozones<- vect("./4 - Adding Ecoregions/Eco_Level2/NA_CEC_Eco_Level2.shp")

#Convert blueberry points to a spatial object
blueberry_points<- vect(vaccinium_clean_final_dataset, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")

#Ensure both data sets are in the same CRS
blueberry_points<- project(blueberry_points, crs(ecozones))
crs(ecozones) #Sphere_ARC_INFO_Lambert_Azimuthal_Equal_Area, EPSG 9001
crs(blueberry_points) #Sphere_ARC_INFO_Lambert_Azimuthal_Equal_Area, EPSG 9001

#Perform spatial join: Extract ecozone for each blueberry location
blueberry_ecozones<- terra::intersect(blueberry_points, ecozones) 

#Convert the result back to a data frame
original_merged_data<- as.data.frame(blueberry_ecozones) #Removed the 27 NA values

#Find the index in the original data for each observation with an ecozone
new_index<-match(original_merged_data$reference, vaccinium_clean_final_dataset$reference)

#Create a new data set with only observations that had a clear ecozone
new_vaccinium_values<-vaccinium_clean_final_dataset[new_index, ]

#Add longitude and latitude to blueberry ecozone data for mapping 
original_merged_data$decimalLongitude<- new_vaccinium_values$decimalLongitude
original_merged_data$decimalLatitude<- new_vaccinium_values$decimalLatitude

#Find indices of all rows in the original data
all_indices <- 1:nrow(vaccinium_clean_final_dataset)

#Find the indices that did not match an ecozone
na_indices <- setdiff(all_indices, new_index)

#Subset the original data to get the NA observations
na_vaccinium_values <- vaccinium_clean_final_dataset[na_indices, ]

#Convert NA points to a spatial object
NA_points<- vect(na_vaccinium_values, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")

#Ensure both data sets are in the same CRS
NA_points<- project(NA_points, crs(ecozones))
crs(ecozones) #Sphere_ARC_INFO_Lambert_Azimuthal_Equal_Area, EPSG 9001
crs(NA_points) #Sphere_ARC_INFO_Lambert_Azimuthal_Equal_Area, EPSG 9001

#Add buffer to points
NA_points_buffered<- buffer(NA_points, width = 27000) 

#Perform spatial join: Extract ecozone for each NA location
NA_ecozones<- terra::intersect(NA_points_buffered, ecozones)

#Convert the result back to a data frame
merged_data<- as.data.frame(NA_ecozones)
NA_merged_data<- merged_data %>% 
  distinct(reference, .keep_all = TRUE) #Have to filter for distinct references
#because buffers around points could be in multiple ecozones

#Add long and lat back
NA_merged_data$decimalLongitude<- na_vaccinium_values$decimalLongitude
NA_merged_data$decimalLatitude<- na_vaccinium_values$decimalLatitude

#Re-merge the 27 NA (not NA anymore) with other data
vaccinium_with_zones<- rbind(original_merged_data, NA_merged_data)

#remove one water point 
table(vaccinium_with_zones$NA_L2NAME)
vaccinium_with_zones<- vaccinium_with_zones %>% 
  filter(NA_L2NAME != "WATER")

#Reformat so we have just month and day
#I've kept a placeholder year for this so that it still treated like a date 
vaccinium_with_zones <- vaccinium_with_zones %>% mutate(date=make_date(year = 1900, month = month(eventDate), day = day(eventDate)))

#Save new file
write.csv(vaccinium_with_zones, "./4 - Adding Ecoregions/vaccinium_with_zones_weather_clean.csv")

