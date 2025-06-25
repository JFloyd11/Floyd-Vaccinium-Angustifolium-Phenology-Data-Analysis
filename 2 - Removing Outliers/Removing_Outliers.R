#Removing outliers

#Set working directory
setwd("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/2 - Removing Outliers")

#Load packages
library(dplyr)
library(geodata)

#-------------------------------------------------------------------------------
# Remove all specimens that are more than two standard deviations away from the mean
#-------------------------------------------------------------------------------
#Import dataset and select variables on interest
vaccinium_with_jdates <- read.csv("initial_data.csv")
vaccinium_with_jdates <- vaccinium_with_jdates %>% 
  select(!X)

#Convert date column to date data type
vaccinium_with_jdates$eventDate<- as.Date(vaccinium_with_jdates$eventDate)

#Separate data into phenology stages
bud_data<- vaccinium_with_jdates %>% 
  filter(bud == "Y")
flower_data<- vaccinium_with_jdates %>% 
  filter(flower == "Y")
fruit_data<- vaccinium_with_jdates %>% 
  filter(fruit == "Y")
seed_disperse_data<- vaccinium_with_jdates %>% 
  filter(seed_disperse == "Y")
veg_only_data<- vaccinium_with_jdates %>% 
  filter(veg_only == "Y")

#Find the mean calendar date for each phenology stage
bud_mean<- mean(bud_data$jdate)
flower_mean<- mean(flower_data$jdate)
fruit_mean<- mean(fruit_data$jdate)
seed_disperse_mean<- mean(seed_disperse_data$jdate)
vo_mean<- mean(veg_only_data$jdate)

#Find the calendar date standard deviation for each phenology stage
bud_sd<- sd(bud_data$jdate)
flower_sd<- sd(flower_data$jdate)
fruit_sd<- sd(fruit_data$jdate)
seed_disperse_sd<- sd(seed_disperse_data$jdate)
vo_sd<- sd(veg_only_data$jdate)

#Find the max and min limits for each phenology stage
max_limit_bud<- bud_mean + bud_sd*2
min_limit_bud<- bud_mean - bud_sd*2
max_limit_flower<- flower_mean + flower_sd*2
min_limit_flower<- flower_mean - flower_sd*2
max_limit_fruit<- fruit_mean + fruit_sd*2
min_limit_fruit<- fruit_mean - fruit_sd*2
max_limit_sd<- seed_disperse_mean + seed_disperse_sd*2
min_limit_sd<- seed_disperse_mean - seed_disperse_sd*2
max_limit_vo<- vo_mean + vo_sd*2
min_limit_vo<- vo_mean - vo_sd*2

#Filter each phenology stage to omit any observations that are more than two standard
#deviations away from the mean calendar day
filtered_bud_data<- bud_data %>% 
  filter(!(jdate > max_limit_bud | jdate < min_limit_bud))
filtered_flower_data<- flower_data %>% 
  filter(!(jdate > max_limit_flower | jdate < min_limit_flower))
filtered_fruit_data<- fruit_data %>% 
  filter(!(jdate > max_limit_fruit | jdate < min_limit_fruit))
filtered_sd_data<- seed_disperse_data %>% 
  filter(!(jdate > max_limit_sd | jdate < min_limit_sd))
filtered_vo_data<- veg_only_data %>% 
  filter(!(jdate > max_limit_vo | jdate < min_limit_vo))

#Figure out how many observations where removed
removed_bud_data<- bud_data %>% 
  filter(jdate > max_limit_bud | jdate < min_limit_bud)
removed_flower_data<- flower_data %>% 
  filter(jdate > max_limit_flower | jdate < min_limit_flower)
removed_fruit_data<- fruit_data %>% 
  filter(jdate > max_limit_fruit | jdate < min_limit_fruit)
removed_sd_data<- seed_disperse_data %>% 
  filter(jdate > max_limit_sd | jdate < min_limit_sd)
removed_vo_data<- veg_only_data %>% 
  filter(jdate > max_limit_vo | jdate < min_limit_vo)

removed<- rbind(removed_bud_data, removed_flower_data, removed_fruit_data, removed_sd_data, removed_vo_data)
removed_unique<- unique(removed) #removed 71 unique specimens

#Combine the filtered data
merged_data<-rbind(filtered_bud_data, filtered_flower_data, filtered_fruit_data, filtered_sd_data, filtered_vo_data)
merged_data_unique<-unique(merged_data) 

#Make sure that any points that were removed for one stage but not another are removed.
#For example an observation might be scored as budding and flowering but only violates
#the outlier removal rules for budding. 
merged_data_unique<- merged_data_unique %>% 
  filter(!(reference %in% removed_unique$reference))
#-------------------------------------------------------------------------------
# Remove veg only observations
#-------------------------------------------------------------------------------
#Removed 224 vegetation only observations. 
clean_data <- merged_data_unique %>% 
  filter(is.na(veg_only))
#-------------------------------------------------------------------------------
# Remove odd Quebec point
#-------------------------------------------------------------------------------
#Create a base map
world<- world(path = "/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/2 - Removing Outliers")

us_map <- gadm(country = 'USA', level = 1, resolution = 2,
               path = "/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/2 - Removing Outliers") #USA basemap w. States

ca_map <- gadm(country = 'CA', level = 1, resolution = 2,
               path = "/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/2 - Removing Outliers") #Canada basemap w. Provinces

canUS_map <- rbind(us_map, ca_map) #combine US and Canada vector map

great_lakes<- vect("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/2 - Removing Outliers/Great Lakes")

#Visualize the point in Northern Quebec on the out skirts of Hudson Bay
plot(canUS_map, xlim = c(-96, -50), ylim = c(37, 60), background = 'lightblue', col = 'white', border = 'grey')
plot(great_lakes, add = TRUE, col = 'lightblue', border = 'grey')
points(x = clean_data$decimalLongitude, y = clean_data$decimalLatitude, col = "red")

#Remove the point
clean_data<- clean_data %>% 
  filter(reference != "70650")

#Check that the point has been removed
plot(canUS_map, xlim = c(-96, -50), ylim = c(37, 60), background = 'lightblue', col = 'white', border = 'grey')
plot(great_lakes, add = TRUE, col = 'lightblue', border = 'grey')
points(x = clean_data$decimalLongitude, y = clean_data$decimalLatitude, col = "red")

#Save the clean dataset
write.csv(clean_data, "/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/2 - Removing Outliers/clean_data.csv")
#-------------------------------------------------------------------------------