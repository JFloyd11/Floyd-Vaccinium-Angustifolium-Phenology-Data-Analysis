#Cleaning the original datasets 

#Load packages
library(readxl)
library(dplyr)
library(tibble)
library(lubridate)

#-------------------------------------------------------------------------------
#Import and clean the E. C. Smith herbarium data 
#-------------------------------------------------------------------------------
ECSmithHerbariumAccessions_PhenologyScoring <- read_excel("./1 - Initial Data Cleaning/ECSmithHerbariumAccessions_PhenologyScoring2.xlsx", 
                                                          col_types = c("text", "text", "text", 
                                                                        "date", "text", "numeric", "text", 
                                                                        "numeric", "text", "text", 
                                                                        "text", "text", "text", "text", "text", 
                                                                        "text", "text", "text", "text", "text", 
                                                                        "text", "text", "text", "text", "text", 
                                                                        "text"))

#Select columns of interest
ECSmith_Herbarium<- ECSmithHerbariumAccessions_PhenologyScoring %>% 
  select(catalogueNumber, decimalLatitude, decimalLongitude, eventDate, Bud, Flower, 
         Fruit, Seed_disperse, Only_Vegetative)

#Select only entries that have been scored
ECSmith_Herbarium<- ECSmith_Herbarium %>% 
  filter(Bud != "NA" | Flower != "NA"| Fruit != "NA"| Seed_disperse != "NA"| Only_Vegetative != "NA")

#Replace NA values
ECSmith_Herbarium$decimalLatitude[is.na(ECSmith_Herbarium$decimalLatitude)]<- 0
ECSmith_Herbarium$decimalLongitude[is.na(ECSmith_Herbarium$decimalLongitude)]<- 0
ECSmith_Herbarium$eventDate[is.na(ECSmith_Herbarium$eventDate)]<- 0
ECSmith_Herbarium$Bud[is.na(ECSmith_Herbarium$Bud)]<- "NA"
ECSmith_Herbarium$Flower[is.na(ECSmith_Herbarium$Flower)]<- "NA"
ECSmith_Herbarium$Fruit[is.na(ECSmith_Herbarium$Fruit)]<- "NA"
ECSmith_Herbarium$Seed_disperse[is.na(ECSmith_Herbarium$Seed_disperse)]<- "NA"
ECSmith_Herbarium$Only_Vegetative[is.na(ECSmith_Herbarium$Only_Vegetative)]<- "NA"

#Filter for ones without coordinates
#Removed 56 observations that didn't have latitude and longitude coordinates 
ECSmith_clean<- ECSmith_Herbarium %>% 
  filter(decimalLongitude != 0 | decimalLatitude != 0)

#Add a column to signify which dataset the data came from
ECSmith_clean <- ECSmith_clean %>% 
  add_column(dataset = "ECSmith")

#Filter for ones without an eventDate
#Removed one observation without an eventDate
ECSmith_clean<- ECSmith_clean %>% 
  filter(eventDate != 0)

#Add a column signifying the month of each entry
ECSmith_clean<- ECSmith_clean %>% 
  mutate(month = month(eventDate))

#Add a column signifying the day of each entry
ECSmith_clean<- ECSmith_clean %>% 
  mutate(day = day(eventDate))

#Add a column signifying the year of each entry
ECSmith_clean<- ECSmith_clean %>% 
  mutate(year = year(eventDate))

#Rename columns 
ECSmith_clean<- ECSmith_clean %>% 
  rename("bud" = "Bud",
         "flower" = "Flower",
         "fruit" = "Fruit",
         "seed_disperse" = "Seed_disperse",
         "veg_only" = "Only_Vegetative",
         "reference" = "catalogueNumber")
#-------------------------------------------------------------------------------
#Import and clean the iNaturalist data
#-------------------------------------------------------------------------------
angustifolium_photodata <- read_excel("./1 - Initial Data Cleaning/angustifolium_photodata.xlsx")

#Selecting important variables from iNaturalist data
iNaturalist_data<- angustifolium_photodata %>% 
  select(occurrenceID, decimalLatitude, decimalLongitude, eventDate, bud, flower, 
         fruit, seed_disperse, veg_only, comments, skip, month, day, year)

#Replace NA values
#Because this ran means there aren't any missing values in the coordinates (still need to check for zeros)
iNaturalist_data[is.na(iNaturalist_data)]<- "NA"

#Get rid of skipped entries
#Removed 9 skipped entries
iNaturalist_data<- iNaturalist_data %>% 
  filter(skip == "NA")

#Select only entries that have been scored
iNaturalist_data<- iNaturalist_data %>% 
  filter(bud != "NA" | flower != "NA"| fruit != "NA"| seed_disperse != "NA"| veg_only != "NA")

#Removing ones marked as 'full date not given' and 'unsure' and 'Looked diseased'
#Ones marked as 'full date not given' where checked manually (full date could not be found)
#Removed 15 "full date not given"
#Removed 22 "Unsure"
#Removed 2 "Looked dead"
iNaturalist_data<- iNaturalist_data %>% 
  filter(comments != "Full date not given" & comments != "Unsure" & comments != "Plant looked diseased")
#There are still two comments left. The "full location not given" comment doesn't
#apply as the latitude and longitude are given. The "duplicate with entry 563" comment
#can be ignored as entry 563 was one of the nine observations removed when we got rid 
#of skipped entries

#Add a dataset column 
iNaturalist_data <- iNaturalist_data %>% 
  add_column(dataset = "iNaturalist")

#Select new variables of interest now that cleaning is complete
iNaturalist_clean <- iNaturalist_data %>% 
  select(occurrenceID, decimalLatitude, decimalLongitude, eventDate, bud, flower, 
         fruit, seed_disperse, veg_only, month, day, year, dataset )

#Remove observations without coordinates (didn't remove anything)
iNaturalist_clean <- iNaturalist_clean %>% 
  filter(decimalLatitude != 0 | decimalLongitude != 0)

#Convert date column to a date data type 
iNaturalist_clean$eventDate<- as.Date(iNaturalist_clean$eventDate, format = "%Y-%m-%d")

#Check that there are no missing dates
#nothing removed
iNaturalist_clean$eventDate[is.na(iNaturalist_clean$eventDate)]<- 0
iNaturalist_clean<- iNaturalist_clean %>% 
  filter(eventDate != 0)

#Rename columns 
iNaturalist_clean<- iNaturalist_clean %>% 
  rename("reference" = "occurrenceID")
#-------------------------------------------------------------------------------
#Import and clean online herbarium data
#-------------------------------------------------------------------------------
filtered_blueberry_data_1_<- read_excel("./1 - Initial Data Cleaning/filtered_blueberry_data (1).xlsx")

#Replace NA values
#Since this runs means there weren't any missing values in the coordinates (still need to check for zeros)
filtered_blueberry_data_1_[is.na(filtered_blueberry_data_1_)]<- "NA"

#Remove skipped values
#Removed 38 "Skip"
herbarium_data<- filtered_blueberry_data_1_ %>% 
  filter(skip == "NA" )

#Remove ones marked as unsure
#Removed 42 "unsure"
herbarium_data<- herbarium_data %>% 
  filter(comment != "Unsure")

#Remove ones with an incorrect date. Incorrect dates were recorded on the specimen
#as 1800s but in the spread sheet as 1900s. All entries marked with incorrect dates 
#were manually checked. The best decision is to remove them. There is six in total.
herbarium_data<- herbarium_data %>% 
  filter(comment != "Incorrect date")

#Add a column that clarifies which dataset this data came from
online_herbarium_data <- herbarium_data %>% 
  add_column(dataset = "Online Herbarium")

#Select for important variables
online_herbarium_data<- online_herbarium_data %>% 
  select(`dcterms:references`,`dwc:decimalLatitude`, `dwc:decimalLongitude`, `dwc:eventDate`,
         bud, flower, fruit, Seed_disperse, veg_only, dataset)

#Rename variables
online_herbarium_data<- online_herbarium_data %>% 
  rename("decimalLatitude"= "dwc:decimalLatitude",
         "decimalLongitude" = "dwc:decimalLongitude",
         "eventDate"= "dwc:eventDate",
         "reference" = "dcterms:references",
         "seed_disperse" = "Seed_disperse")

#Select only entries that have been scored
online_herbarium_data<- online_herbarium_data %>% 
  filter(bud != "NA" | flower != "NA"| fruit != "NA"| seed_disperse != "NA"| veg_only != "NA")

#Convert eventDate to date data type
online_herbarium_data$eventDate<- as.Date(online_herbarium_data$eventDate, format = "%Y-%m-%d")

#Add a column signifying the month of each entry
online_herbarium_data<- online_herbarium_data %>% 
  mutate(month = month(eventDate))

#Add a column signifying the day of each entry
online_herbarium_data<- online_herbarium_data %>% 
  mutate(day = day(eventDate))

#Add a column signifying the year of each entry
online_herbarium_data<- online_herbarium_data %>% 
  mutate(year = year(eventDate))

#Check that every observation has coordinates
#Didn't remove anything
online_herbarium_data <- online_herbarium_data %>% 
  filter(decimalLatitude != 0 | decimalLongitude != 0)

#Check that every observation has an event date
#Didn't remove anything
online_herbarium_data$eventDate[is.na(online_herbarium_data$eventDate)]<- 0
online_herbarium_data <- online_herbarium_data %>% 
  filter(eventDate != 0)
#-------------------------------------------------------------------------------
#Merge the datasets
#-------------------------------------------------------------------------------
merged_vaccinium<- rbind(online_herbarium_data, iNaturalist_clean, ECSmith_clean)

#Add julian date column
dates<-merged_vaccinium$eventDate
julian_dates<- numeric(length(dates))
for (index in 1:length(dates))
{
  julian_date<-yday(dates[index])
  julian_dates[index]<- julian_date
}

merged_vaccinium$jdate<- julian_dates

#Save merged dataset
write.csv(merged_vaccinium, "./1 - Initial Data Cleaning/initial_data.csv")
#-------------------------------------------------------------------------------