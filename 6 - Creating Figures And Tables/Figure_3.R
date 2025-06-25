#Creating Figure 3

#Set working directory
setwd("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/6 - Creating Figures And Tables")

#Load packages
library(dplyr)
library(ggplot2)

#Import data and select import variables 
vaccinium <- read.csv("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/6 - Creating Figures And Tables/vaccinium_with_zones_weather_clean.csv")
vaccinium <- vaccinium %>% 
  select(!(X))

#Separate data
bud<- vaccinium %>% 
  filter(bud == "Y")
flower<- vaccinium %>% 
  filter(flower == "Y")
fruit<- vaccinium %>% 
  filter(fruit == "Y")
sd<- vaccinium %>% 
  filter(seed_disperse == "Y")


#Modify bud data for facet wrap
bud_march<- bud 
bud_april<- bud
bud_may<- bud

bud_march$month<- "March"
bud_march$temp<- bud_march$march_temperature
bud_april$month<- "April"
bud_april$temp<- bud_april$april_temperature
bud_may$month<- "May"
bud_may$temp<- bud_may$may_temperature
new_bud<- rbind(bud_march, bud_april, bud_may)
new_bud$pheno<- "Bud"

#Modify fruit data for facet wrap
fruit_april<- fruit
fruit_july<- fruit

fruit_april$month<- "April"
fruit_april$temp<- fruit_april$april_temperature
fruit_july$month<- "July"
fruit_july$temp<- fruit_july$july_temperature
new_fruit<- rbind(fruit_april, fruit_july)
new_fruit$pheno<- "Fruit pre-veraison"

#Modify flower data for facet wrap
flower_march<- flower
flower_april<- flower
flower_may<- flower
flower_july<- flower

flower_march$month<- "March"
flower_april$month<- "April"
flower_may$month<- "May"
flower_july$month<- "July"
flower_march$temp<- flower_march$march_temperature
flower_april$temp<- flower_april$april_temperature
flower_may$temp<- flower_may$may_temperature
flower_july$temp<- flower_july$july_temperature
new_flower<- rbind(flower_march, flower_april, flower_may, flower_july)
new_flower$pheno<- "Flower"

#Modify seed disperse data for facet wrap
sd_may<- sd
sd_august<- sd

sd_may$month<- "May"
sd_august$month<- "August"
sd_may$temp<- sd_may$may_temperature
sd_august$temp<- sd_august$august_temperature
new_sd<- rbind(sd_may, sd_august)
new_sd$pheno<- "Fruit post-veraison"

#Recombine data
all<- rbind(new_bud, new_flower, new_fruit, new_sd)

#Order months through factoring
all$month <- factor(all$month, levels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

#Order phenology stages through factoring
all$pheno <- factor(all$pheno, levels = c(
  "Bud", "Flower", "Fruit pre-veraison", "Fruit post-veraison"))

#Change date column to a date data type
all$date<- as.Date(all$date)

#Generate figure 3
ggplot(all, aes(x = temp, y = jdate)) +
  geom_point(aes(color = date)) +
  geom_smooth(method=lm, se=FALSE, color = 'red') +
  theme_bw() +
  facet_grid(pheno ~ month, scales = "free") +
  scale_color_viridis_c(option = "mako") +
  scale_y_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                     labels = month.abb) +
  guides(color = "none") +
  labs(
    x = "Temperature (°C)",
    y = "Day of Year (DOY)"
  ) +
  theme(
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 14),  # Facet column labels
    strip.text.y = element_text(size = 14) 
  )
