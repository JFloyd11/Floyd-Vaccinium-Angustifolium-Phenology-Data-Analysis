#Creating Figure 4

#Load packages
library(dplyr)
library(lme4)
library(lmerTest)
library(ggeffects)
library(cowplot)
library(ggplot2)

#Import data and select import variables 
vaccinium <- read.csv("./6 - Creating Figures And Tables/vaccinium_with_zones_weather_clean.csv")
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

#Predict fitted values
predicted_bud<- ggpredict(bud_model, terms = "year")
predicted_flower<- ggpredict(flower_model, terms = "year")
predicted_fruit<- ggpredict(fruit_model, terms = "year")
predicted_sd<- ggpredict(sd_model, terms = "year")

#Create bud plot
bud$predicted_jdate<- predict(bud_model)
range_b<- range(bud$predicted_jdate)
breaks_b <- seq(from = range_b[1], to = range_b[2], length.out = 6)
labels_b <- format(as.Date(breaks_b - 1, origin = "1901-01-01"), "%b %d")

b<-ggplot(bud, aes(x = year, y = predicted_jdate, color = NA_L2NAME)) +
  geom_point(alpha = 0.8) +
  geom_line(data = predicted_bud, aes(x = x, y = predicted), color = "red", linewidth = 1.2) +
  #scale_color_manual(name = "Ecozone", values = ecozone_colors) +
  scale_color_manual(values = c(
    "ATLANTIC HIGHLANDS" = "blueviolet",
    "CENTRAL USA PLAINS" = "deeppink",
    "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" = "cyan",
    "MIXED WOOD PLAINS" = "blue",
    "MIXED WOOD SHIELD" = "darkorange",
    "OZARK/OUACHITA-APPALACHIAN FORESTS" = "darkgoldenrod2",
    "SOFTWOOD SHIELD" = "darkgreen",
    "SOUTHEASTERN USA PLAINS" = "darkolivegreen4"
  ), labels = c(
    "Atlantic highlands",
    "Central USA plains",
    "Mississippi",
    "Mixed wood plains",
    "Mixed wood shield",
    "Ozark",
    "Softwood shield",
    "Southeastern USA plains"
  ), name = "Ecozone") +
  scale_y_continuous(breaks = breaks_b,
                     labels = labels_b) +
  labs(
    x = "Year",
    y = "Budding DOY") +
  guides(color = guide_legend(
    override.aes = list(size = 5)  # Makes legend dots/lines larger
  )) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),  # Title size
    legend.text = element_text(size = 12),    # Label size
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

#Generate flower plot
flower$predicted_jdate<- predict(flower_model)
range_fl<- range(flower$predicted_jdate)
breaks_fl <- seq(from = range_fl[1], to = range_fl[2], length.out = 6)
labels_fl <- format(as.Date(breaks_fl - 1, origin = "1901-01-01"), "%b %d")

fl<-ggplot(flower, aes(x = year, y = predicted_jdate, color = NA_L2NAME)) +
  geom_point(alpha = 0.8) +
  geom_line(data = predicted_flower, aes(x = x, y = predicted), color = "red", linewidth = 1.2) +
  #scale_color_manual(values = ecozone_colors) +
  scale_color_manual(values = c(
    "ATLANTIC HIGHLANDS" = "blueviolet",
    "CENTRAL USA PLAINS" = "deeppink",
    "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" = "cyan",
    "MIXED WOOD PLAINS" = "blue",
    "MIXED WOOD SHIELD" = "darkorange",
    "OZARK/OUACHITA-APPALACHIAN FORESTS" = "darkgoldenrod2",
    "SOFTWOOD SHIELD" = "darkgreen",
    "SOUTHEASTERN USA PLAINS" = "darkolivegreen4"
  ), labels = c(
    "Atlantic highlands",
    "Central USA plains",
    "Mississippi",
    "Mixed wood plains",
    "Mixed wood shield",
    "Ozark",
    "Softwood shield",
    "Southeastern USA plains"
  ), name = "Ecozone") +
  scale_y_continuous(breaks = breaks_fl,
                     labels = labels_fl) +
  labs(
    x = "Year",
    y = "Flowering DOY") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

#Generate fruit plot
fruit$predicted_jdate<- predict(fruit_model)
range_fr<- range(fruit$predicted_jdate)
breaks_fr <- seq(from = range_fr[1], to = range_fr[2], length.out = 6)
labels_fr <- format(as.Date(breaks_fr - 1, origin = "1901-01-01"), "%b %d")

fr<-ggplot(fruit, aes(x = year, y = predicted_jdate, color = NA_L2NAME)) +
  geom_point(alpha = 0.8) +
  geom_line(data = predicted_fruit, aes(x = x, y = predicted), color = "red", linewidth = 1.2) +
  #scale_color_manual(values = ecozone_colors) +
  scale_color_manual(values = c(
    "ATLANTIC HIGHLANDS" = "blueviolet",
    "CENTRAL USA PLAINS" = "deeppink",
    "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" = "cyan",
    "MIXED WOOD PLAINS" = "blue",
    "MIXED WOOD SHIELD" = "darkorange",
    "OZARK/OUACHITA-APPALACHIAN FORESTS" = "darkgoldenrod2",
    "SOFTWOOD SHIELD" = "darkgreen",
    "SOUTHEASTERN USA PLAINS" = "darkolivegreen4"
  ), labels = c(
    "Atlantic highlands",
    "Central USA plains",
    "Mississippi",
    "Mixed wood plains",
    "Mixed wood shield",
    "Ozark",
    "Softwood shield",
    "Southeastern USA plains"
  ), name = "Ecozone") +
  scale_y_continuous(breaks = breaks_fr,
                     labels = labels_fr) +
  labs(
    x = "Year",
    y = "Fruit pre-veraison DOY") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

#Generate seed disperse plot
sd$predicted_jdate<- predict(sd_model)
range_sd<- range(sd$predicted_jdate)
breaks_sd <- seq(from = range_sd[1], to = range_sd[2], length.out = 6)
labels_sd <- format(as.Date(breaks_sd - 1, origin = "1901-01-01"), "%b %d")

sd2<-ggplot(sd, aes(x = year, y = predicted_jdate, color = NA_L2NAME)) +
  geom_point(alpha = 0.8) +
  geom_line(data = predicted_sd, aes(x = x, y = predicted), color = "red", linewidth = 1.2) +
  #scale_color_manual(values = ecozone_colors) +
  scale_color_manual(values = c(
    "ATLANTIC HIGHLANDS" = "blueviolet",
    "CENTRAL USA PLAINS" = "deeppink",
    "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" = "cyan",
    "MIXED WOOD PLAINS" = "blue",
    "MIXED WOOD SHIELD" = "darkorange",
    "OZARK/OUACHITA-APPALACHIAN FORESTS" = "darkgoldenrod2",
    "SOFTWOOD SHIELD" = "darkgreen",
    "SOUTHEASTERN USA PLAINS" = "darkolivegreen4"
  ), labels = c(
    "Atlantic highlands",
    "Central USA plains",
    "Mississippi",
    "Mixed wood plains",
    "Mixed wood shield",
    "Ozark",
    "Softwood shield",
    "Southeastern USA plains"
  ), name = "Ecozone") +
  scale_y_continuous(breaks = breaks_sd,
                     labels = labels_sd) +
  labs(
    x = "Year",
    y = "Fruit post-veraison DOY") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

#Get legend
legend<- get_legend(b)
b<- b + theme(legend.position = "none")

#Plot figure 4
plots_combined<- plot_grid(b, fl, fr, sd2, labels = "AUTO", ncol = 2)
plot_grid(plots_combined, legend, rel_widths = c(1, 0.25))
