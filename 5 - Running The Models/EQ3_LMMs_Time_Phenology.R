#LMMs for time phenology models

#Load packages
library(dplyr)
library(lme4)
library(lmerTest)

#Import data and select import variables 
vaccinium <- read.csv("./5 - Running The Models/vaccinium_with_zones_weather_clean.csv")
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
bud_model <- lmer(jdate  ~ year + (1 | NA_L2NAME), data = bud)
summary(bud_model)

#Flower model
flower_model <- lmer(jdate  ~ year + (1 | NA_L2NAME), data = flower)
summary(flower_model)

#Fruit model
fruit_model <- lmer(jdate  ~ year + (1 | NA_L2NAME), data = fruit)
summary(fruit_model)

#Seed disperse model
sd_model <- lmer(jdate  ~ year + (1 | NA_L2NAME), data = sd)
summary(sd_model)


# -------------------------------------------------------------------------
# LRTs
# -------------------------------------------------------------------------
# Bud
bud_lm_year <- lm(jdate ~ year, data = bud)
lrtest(bud_lm_year, bud_model)
# Flower
flower_lm_year <- lm(jdate ~ year, data = flower)
lrtest(flower_lm_year, flower_model)
# Fruit
fruit_lm_year <- lm(jdate ~ year, data = fruit)
lrtest(fruit_lm_year, fruit_model)
# Seed disperse
sd_lm_year <- lm(jdate ~ year, data = sd)
lrtest(sd_lm_year, sd_model)

