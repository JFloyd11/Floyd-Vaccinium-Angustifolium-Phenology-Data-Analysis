#Creating Figure 2

#Set working directory
setwd("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/6 - Creating Figures And Tables")

#Load packages
library(dplyr)
library(ggridges)
library(ggplot2)

#Import data and select import variables 
vaccinium <- read.csv("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/6 - Creating Figures And Tables/vaccinium_with_zones_weather_clean.csv")
vaccinium <- vaccinium %>% 
  select(!(X))

#Change date column to date data type
vaccinium$date<- as.Date(vaccinium$date)

#Separate data into phenology stages
bud<- vaccinium %>% 
  filter(bud == "Y")
flower<- vaccinium %>% 
  filter(flower == "Y")
fruit<- vaccinium %>% 
  filter(fruit == "Y")
sd<- vaccinium %>% 
  filter(seed_disperse == "Y")

#Add pheno column
bud$pheno<- "Bud"
flower$pheno<- "Flower"
fruit$pheno<- "Fruit pre-veraison"
sd$pheno<- "Fruit post-veraison"

#Remerge data
pheno_added<- rbind(bud, flower, fruit, sd)

#Order phenology stages through factoring
pheno_added$pheno<- factor(pheno_added$pheno, levels = c("Fruit post-veraison",
                                                         "Fruit pre-veraison",
                                                         "Flower",
                                                         "Bud"))

#Get mean DOY for each phenoly stage
mean_dates <- pheno_added %>%
  group_by(pheno) %>%
  summarize(mean_date = mean(date)) %>% 
  mutate(pheno_num = as.numeric(pheno))

#Create ridge plot
ggplot(pheno_added, aes(x = date, y = pheno, group = pheno, fill = ..x..)) +
  geom_density_ridges_gradient(alpha = 0.7, scale = 1.0) +
  geom_segment(data = mean_dates,
               aes(x = mean_date, xend = mean_date,
                   y = pheno_num, yend = pheno_num + 1),
               linetype = "dashed", color = "red", size = 0.8,
               inherit.aes = FALSE) +
  theme_ridges() +
  theme_bw() +
  theme(
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title.y = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank()) +
  labs(x = "Date",
       y = "Phenological Stage") +
  scale_fill_viridis_c(option = "mako", direction = 1, guide = "none")
