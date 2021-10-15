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

summary(rawdata$BodyTemp)
# The values are all falling with reasonable ranges

ggplot(rawdata, aes(x=BodyTemp)) + geom_histogram()
# Histogram of body temperature

ggplot(rawdata, aes(x=RunnyNose, y=BodyTemp)) + geom_boxplot()
# boxplot to for runnynose and body temp
# body temp is my outcome of interest, I plan to use some selected synptoms to predict body temp 


# All these variables selected below are my predictor variables
summary(rawdata$RunnyNose)
# Looking for if any entry other than yes and no

summary(rawdata$Headache)
# Looking for if any entry other than yes and no

summary(rawdata$Sneeze)
# Looking for if any entry other than yes and no

summary(rawdata$CoughYN)
# Looking for if any entry other than yes and no

summary(rawdata$Fatigue)
# Looking for if any entry other than yes and no

summary(rawdata$ChestCongestion)
# Looking for if any entry other than yes and no

summary(rawdata$Weakness)
# Looking for if any entry other than None, Mild, Moderate and Severe

# Since everything looks good, no change was made so no need to save data


# location to save file
# save_data_location <- here::here("data","processed_data","processeddata2.rds")

#saveRDS(processeddata, file = save_data_location)


