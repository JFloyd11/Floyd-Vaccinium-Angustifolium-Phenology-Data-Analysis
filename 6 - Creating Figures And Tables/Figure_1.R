#Creating Figure 1

#Set working directory
setwd("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/6 - Creating Figures And Tables")

#Load packages
library(dplyr)
library(ggplot2)
library(sf)
library(patchwork)  
library(scales)
library(geodata)
library(cowplot)

#Import data and select import variables 
vaccinium <- read.csv("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/6 - Creating Figures And Tables/vaccinium_with_zones_weather_clean.csv")
vaccinium <- vaccinium %>% 
  select(!(X))

#Import base map 
world<- world(path = "/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/6 - Creating Figures And Tables")
us_map <- gadm(country = 'USA', level = 1, resolution = 2,
               path = "/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/6 - Creating Figures And Tables") #USA basemap w. States
ca_map <- gadm(country = 'CA', level = 1, resolution = 2,
               path = "/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/6 - Creating Figures And Tables") #Canada basemap w. Provinces
canUS_map <- rbind(us_map, ca_map) #combine US and Canada vector map
great_lakes<- vect("/Users/migicovskylab/Documents/Vaccinium Paper GitHub Files/6 - Creating Figures And Tables/Great Lakes")

#Convert maps to spatial objects
canUS_sf <- st_as_sf(canUS_map)
lakes_sf <- st_as_sf(great_lakes)

#Create base map layers
base_layers <- list(
  geom_sf(data = canUS_sf, fill = "white", color = "grey"),
  geom_sf(data = lakes_sf, fill = "lightblue", color = "grey"),
  coord_sf(xlim = c(-94, -54), ylim = c(37, 51)),
  theme_minimal()
)

#Create dataset map
map_a<- ggplot() +
  base_layers +
  geom_point(data = vaccinium,
             aes(x = decimalLongitude, y = decimalLatitude, color = dataset),
             size = 1, alpha = 0.8) +
  scale_color_manual(values = c(
    "Online Herbarium" = "forestgreen",
    "iNaturalist" = "darkorange1",
    "ECSmith" = "blue"
  )) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(color = "Source") +
  annotate("text", x = -95, y = 50, label = "A", size = 6, fontface = "bold") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    legend.position = c(0.8, 0.25),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "lightblue", color = NA),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.background = element_rect(color = "black"))

#Create ecoregion map
map_b<- ggplot() +
  base_layers +
  geom_point(data = vaccinium,
             aes(x = decimalLongitude, y = decimalLatitude, color = NA_L2NAME), 
             size = 1, alpha = 0.8) +
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
    "Ozark/Ouachita-appalachian",
    "Softwood shield",
    "Southeastern USA plains"
  )) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(color = "Ecozone") +
  annotate("text", x = -95, y = 50, label = "B", size = 6, fontface = "bold") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    legend.position = c(0.8, 0.25),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "lightblue", color = NA),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.background = element_rect(color = "black"),
    legend.key.height = unit(0.3, "cm")
  )

#Add a black border to each map
map_a <- map_a + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.2))
map_b <- map_b + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.2))

#Plot maps one on top of the other
plot_grid(map_a, map_b, labels = NULL, ncol = 1, align = "v", axis = "lr")
