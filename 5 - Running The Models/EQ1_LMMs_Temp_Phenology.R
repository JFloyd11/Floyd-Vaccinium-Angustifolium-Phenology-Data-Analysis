#LMMs for temperature phenology relationships

#Set working directory
setwd("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/5 - Running The Models")

#Load packages
library(dplyr)
library(lme4)
library(lmerTest)

#Import data and select important variables
vaccinium <- read.csv("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/5 - Running The Models/vaccinium_with_zones_weather_clean.csv")
vaccinium <- vaccinium %>% 
  select(!(X))

#Separate data into phenology stages
bud<- vaccinium %>% 
  filter(bud == "Y")
flower<- vaccinium %>% 
  filter(flower == "Y")
fruit<- vaccinium %>% 
  filter(fruit == "Y")
sd<- vaccinium %>% 
  filter(seed_disperse == "Y")

#Bud model
table(bud$month)
bud_model <- lmer(jdate  ~ march_temperature + april_temperature + 
                    may_temperature + june_temperature + july_temperature + (1 | NA_L2NAME), data = bud)
summary(bud_model)

#Flower model
table(flower$month)
flower_model <- lmer(jdate  ~ march_temperature + april_temperature + 
                       may_temperature + june_temperature + july_temperature + (1 | NA_L2NAME), data = flower)
summary(flower_model)

#Fruit model
table(fruit$month)
fruit_model <- lmer(jdate  ~ april_temperature + may_temperature + 
                      june_temperature + july_temperature + august_temperature + (1 | NA_L2NAME), data = fruit)
summary(fruit_model)

#Seed disperse model
table(sd$month)
sd_model <- lmer(jdate  ~ may_temperature + june_temperature + july_temperature + 
                   august_temperature + sept_temperature + (1 | NA_L2NAME), data = sd)
summary(sd_model)
