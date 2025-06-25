#LMs for temperature phenology models

#Set working directory
setwd("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/5 - Running The Models")

#Load packages
library(dplyr)

#Import data and select important variables 
vaccinium <- read.csv("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/5 - Running The Models/vaccinium_with_zones_weather_clean.csv")
vaccinium <- vaccinium %>% 
  select(!(X))

#Separate data into phenology stages
bud_data<- vaccinium %>% 
  filter(bud == "Y")
flower_data<- vaccinium %>% 
  filter(flower == "Y")
fruit_data<- vaccinium %>% 
  filter(fruit == "Y")
seed_disperse_data<- vaccinium %>% 
  filter(seed_disperse == "Y")

#Bud model
bud_model2 <- lm(jdate  ~ march_temperature + april_temperature + may_temperature, data = bud_data)
summary(bud_model2)

#Flower model
fl_model2 <- lm(jdate  ~ march_temperature + april_temperature + may_temperature + july_temperature, data = flower_data)
summary(fl_model2)

#Fruit model
fr_model2 <- lm(jdate  ~ april_temperature + july_temperature, data = fruit_data)
summary(fr_model2)

#Seed disperse model
sd_model2 <- lm(jdate ~ may_temperature + june_temperature + august_temperature, 
                data = seed_disperse_data)
summary(sd_model2)

