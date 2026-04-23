#Getting numbers for table 1

#Load packages
library(dplyr)

#Import data and select import variables 
vaccinium <- read.csv("./6 - Creating Figures And Tables/vaccinium_with_zones_weather_clean.csv")
vaccinium <- vaccinium %>% 
  select(!(X))

#Separate data into different phenological stages
bud<- vaccinium %>% 
  filter(bud == "Y") #N = 300
flower<- vaccinium %>% 
  filter(flower == "Y") #N = 403
fruit<- vaccinium %>% 
  filter(fruit == "Y")#N = 472
sd<- vaccinium %>% 
  filter(seed_disperse == "Y")#N = 369

#Find means
mean(bud$jdate) #138.0167
mean(flower$jdate) #142.6526
mean(fruit$jdate) #184.5403
mean(sd$jdate) #201.2656

#Find standard deviation
sd(bud$jdate) #15.5818
sd(flower$jdate) #17.27718
sd(fruit$jdate) #23.30255
sd(sd$jdate) #21.02599

#Find Min DOY
min(bud$jdate) #99
min(flower$jdate) #102
min(fruit$jdate) #139
min(sd$jdate) #157

#Find Max DOY
max(bud$jdate) #206
max(flower$jdate) #206
max(fruit$jdate) #233
max(sd$jdate) #247

#Find range
206 - 99 #107
206 - 102 #104
233 - 139 #94
247 - 157 #90
