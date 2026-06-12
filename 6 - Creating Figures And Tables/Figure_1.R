#Creating Figure 1

#Load packages
library(dplyr)
library(ggplot2)
library(sf)
library(patchwork)  
library(scales)
library(geodata)
library(cowplot)

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

#Import base map 
world<- world(path = "./6 - Creating Figures And Tables")
us_map <- gadm(country = 'USA', level = 1, resolution = 2,
               path = "./6 - Creating Figures And Tables") #USA basemap w. States
ca_map <- gadm(country = 'CA', level = 1, resolution = 2,
               path = "./6 - Creating Figures And Tables") #Canada basemap w. Provinces
canUS_map <- rbind(us_map, ca_map) #combine US and Canada vector map
great_lakes<- vect("./6 - Creating Figures And Tables/Great Lakes")

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
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
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
    "ATLANTIC HIGHLANDS" = "#A95FBF",
    "CENTRAL USA PLAINS" = "black",
    "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" = "#009E73",
    "MIXED WOOD PLAINS" = "#F0E442",
    "MIXED WOOD SHIELD" = "royalblue",
    "OZARK/OUACHITA-APPALACHIAN FORESTS" = "#D55E00",
    "SOFTWOOD SHIELD" = "#CC79A7",
    "SOUTHEASTERN USA PLAINS" = "#00BFC4"
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
    legend.position = c(0.8, 0.23),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
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
pdf("C:/Users/terre/Documents/Acadia/V_angustifolium_paper/Paper/Paper/June 11th Resubmission/Figure1.pdf", width=7.25, height=7)
plot_grid(map_a, map_b, labels = NULL, ncol = 1, align = "v", axis = "lr")
dev.off()


# Appendix: ecoregion by phenophase ---------------------------------------
map_bud <- ggplot() +
  base_layers +
  geom_point(data = bud,
             aes(x = decimalLongitude, y = decimalLatitude, color = NA_L2NAME), 
             size = 1, alpha = 0.8) +
  scale_color_manual(values = c(
    "ATLANTIC HIGHLANDS" = "#A95FBF",
    "CENTRAL USA PLAINS" = "black",
    "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" = "#009E73",
    "MIXED WOOD PLAINS" = "#F0E442",
    "MIXED WOOD SHIELD" = "royalblue",
    "OZARK/OUACHITA-APPALACHIAN FORESTS" = "#D55E00",
    "SOFTWOOD SHIELD" = "#CC79A7",
    "SOUTHEASTERN USA PLAINS" = "#00BFC4"
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
    legend.box.background = element_rect(color = "black"),
    legend.key.height = unit(0.3, "cm")
  )

map_flower <- ggplot() +
  base_layers +
  geom_point(data = flower,
             aes(x = decimalLongitude, y = decimalLatitude, color = NA_L2NAME), 
             size = 1, alpha = 0.8) +
  scale_color_manual(values = c(
    "ATLANTIC HIGHLANDS" = "#A95FBF",
    "CENTRAL USA PLAINS" = "black",
    "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" = "#009E73",
    "MIXED WOOD PLAINS" = "#F0E442",
    "MIXED WOOD SHIELD" = "royalblue",
    "OZARK/OUACHITA-APPALACHIAN FORESTS" = "#D55E00",
    "SOFTWOOD SHIELD" = "#CC79A7",
    "SOUTHEASTERN USA PLAINS" = "#00BFC4"
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

map_fruit <- ggplot() +
  base_layers +
  geom_point(data = fruit,
             aes(x = decimalLongitude, y = decimalLatitude, color = NA_L2NAME), 
             size = 1, alpha = 0.8) +
  scale_color_manual(values = c(
    "ATLANTIC HIGHLANDS" = "#A95FBF",
    "CENTRAL USA PLAINS" = "black",
    "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" = "#009E73",
    "MIXED WOOD PLAINS" = "#F0E442",
    "MIXED WOOD SHIELD" = "royalblue",
    "OZARK/OUACHITA-APPALACHIAN FORESTS" = "#D55E00",
    "SOFTWOOD SHIELD" = "#CC79A7",
    "SOUTHEASTERN USA PLAINS" = "#00BFC4"
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
  annotate("text", x = -95, y = 50, label = "C", size = 6, fontface = "bold") +
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

map_sd <- ggplot() +
  base_layers +
  geom_point(data = sd,
             aes(x = decimalLongitude, y = decimalLatitude, color = NA_L2NAME), 
             size = 1, alpha = 0.8) +
  scale_color_manual(values = c(
    "ATLANTIC HIGHLANDS" = "#A95FBF",
    "CENTRAL USA PLAINS" = "black",
    "MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS" = "#009E73",
    "MIXED WOOD PLAINS" = "#F0E442",
    "MIXED WOOD SHIELD" = "royalblue",
    "OZARK/OUACHITA-APPALACHIAN FORESTS" = "#D55E00",
    "SOFTWOOD SHIELD" = "#CC79A7",
    "SOUTHEASTERN USA PLAINS" = "#00BFC4"
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
  annotate("text", x = -95, y = 50, label = "D", size = 6, fontface = "bold") +
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
map_bud <- map_bud + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.2))
map_flower <- map_flower + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.2))
map_fruit <- map_fruit + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.2))
map_sd <- map_sd + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.2))

#Plot maps one on top of the other
pdf("./6 - Creating Figures And Tables/ecoregion_phenophase_v2.pdf", width=10, height=6)
plot_grid(map_bud, map_flower, map_fruit, map_sd, labels = NULL, ncol = 2, nrow = 2, align = "v", axis = "lr")
dev.off()
