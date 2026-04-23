#Creating Cp plots

#Load packages
library(dplyr)
library(leaps)

#Import data and select important variables
vaccinium <- read.csv("./5 - Running The Models/vaccinium_with_zones_weather_clean.csv")
vaccinium <- vaccinium %>% 
  dplyr::select(!(X))

#Separate data
bud_data<- vaccinium %>% 
  filter(bud == "Y")
flower_data<- vaccinium %>% 
  filter(flower == "Y")
fruit_data<- vaccinium %>% 
  filter(fruit == "Y")
seed_disperse_data<- vaccinium %>% 
  filter(seed_disperse == "Y")
#_______________________________________________________________________________
#Bud Cp plot
#Rename columns so they fit on the plot
bud_data<- rename(bud_data,c("march_temp" = "march_temperature", 
                             "april_temp" = "april_temperature",
                             "may_temp" = "may_temperature",
                             "june_temp" = "june_temperature",
                             "july_temp" = "july_temperature",
                             "august_temp" = "august_temperature",
                             "sept_temp" = "sept_temperature"))

#Figure out which months we have bud data for
unique(bud_data$month) #4, 5, 6, 7

#Find the best combination of monthly temperatures
budSubsets<- regsubsets(jdate ~ march_temp + april_temp
                        + may_temp + june_temp + july_temp, data = bud_data)
summary(budSubsets)

#Create Cp plot
plot(budSubsets, labels = budSubsets$xnames, main ="Bud", scale = "Cp")
#Best bud model includes March, April, and May
#_______________________________________________________________________________
#Flower Cp plot
#Rename columns so they fit on the plot
flower_data<- rename(flower_data,c("march_temp" = "march_temperature", 
                                   "april_temp" = "april_temperature",
                                   "may_temp" = "may_temperature",
                                   "june_temp" = "june_temperature",
                                   "july_temp" = "july_temperature",
                                   "august_temp" = "august_temperature",
                                   "sept_temp" = "sept_temperature"))

#Figure out which months we have flower data for
unique(flower_data$month) #4, 5, 6, 7

#Find the best combination of monthly temperatures
flowerSubsets<- regsubsets(jdate ~ march_temp + april_temp
                           + may_temp + june_temp + july_temp, data = flower_data)
summary(flowerSubsets)

#Create Cp plot
plot(flowerSubsets, labels = flowerSubsets$xnames, main ="Flower", scale = "Cp")
#Best flower model includes March, April, May, and July
#_______________________________________________________________________________
#Fruit Cp plot
#Rename columns so they fit on the plot
fruit_data<- rename(fruit_data,c("march_temp" = "march_temperature", 
                                 "april_temp" = "april_temperature",
                                 "may_temp" = "may_temperature",
                                 "june_temp" = "june_temperature",
                                 "july_temp" = "july_temperature",
                                 "august_temp" = "august_temperature",
                                 "sept_temp" = "sept_temperature"))

#Figure out which months we have fruit data for
unique(fruit_data$month) #5, 6, 7, 8

#Find the best combination of monthly temperatures
fruitSubsets<- regsubsets(jdate ~ april_temp + may_temp + june_temp + 
                            july_temp + august_temp, data = fruit_data)
summary(fruitSubsets)

#Create Cp plot
plot(fruitSubsets, labels = fruitSubsets$xnames, main ="Fruit", scale = "Cp")
#Best fruit model includes April and July
#_______________________________________________________________________________
#Seed Disperse Cp plot
#Rename columns so they fit on the plot
seed_disperse_data<- rename(seed_disperse_data,c("march_temp" = "march_temperature", 
                                                 "april_temp" = "april_temperature",
                                                 "may_temp" = "may_temperature",
                                                 "june_temp" = "june_temperature",
                                                 "july_temp" = "july_temperature",
                                                 "august_temp" = "august_temperature",
                                                 "sept_temp" = "sept_temperature"))

#Figure out which months we have seed disperse data for
unique(seed_disperse_data$month) #6, 7, 8, 9

#Find the best combination of monthly temperatures
seed_disperseSubsets<- regsubsets(jdate ~ may_temp + june_temp + 
                                    july_temp + august_temp + sept_temp, data = seed_disperse_data)
summary(seed_disperseSubsets)

#Create Cp plot
plot(seed_disperseSubsets, labels = seed_disperseSubsets$xnames, main ="Seed Dispserse", scale = "Cp")
#Best seed disperse model includes May, June, and August

