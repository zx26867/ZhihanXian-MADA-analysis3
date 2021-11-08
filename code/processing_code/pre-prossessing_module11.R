###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
rawdata <- readRDS(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

processed_data = rawdata %>% select(-c(CoughYN,CoughYN2,MyalgiaYN,WeaknessYN))
dplyr::glimpse(processed_data)

processed_data$Myalgia <- factor(processed_data$Myalgia, ordered = TRUE, 
                                levels = c("Mild", "Moderate", "Severe"))
processed_data$Weakness <- factor(processed_data$Weakness, ordered = TRUE, 
                                 levels = c("Mild", "Moderate", "Severe"))
processed_data$CoughIntensity <- factor(processed_data$CoughIntensity, ordered = TRUE, 
                                  levels = c("Mild", "Moderate", "Severe"))

for(i in 1:ncol(processed_data)) {       # for-loop over columns
  print(i)
  print(table(processed_data[ , i]))
}

# the binary predictor with < 50 entries in one category is the 21th and 25th colomn, which are 

# remove these 2 col from the dataset
processed_data = processed_data %>% select(-c(21,25))
dplyr::glimpse(processed_data)

# location to save file
save_data_location <- here::here("data","processed_data","processeddata_m11.rds")

saveRDS(processed_data, file = save_data_location)


