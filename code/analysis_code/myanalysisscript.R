###############################

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidymodels)

#path to data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)
mydata

# this is the linear model with ordinary least square method
lm_mod <- linear_reg() %>% set_engine("lm")

# fitting model with 1 continuous outcome and 1 major predictor
lm_fit <- lm_mod %>% fit(BodyTemp ~ RunnyNose, data = mydata)
tidy(lm_fit)

# fitting model with 1 continuous outcome and all predictors I chose
lm_fit <- lm_mod %>% fit(BodyTemp ~ RunnyNose + Headache + Sneeze + CoughYN + Fatigue + ChestCongestion + Weakness, data = mydata)
tidy(lm_fit)

# this is the logistic model with ordinary least square method
glm_mod <- logistic_reg() %>% set_engine("glm")

# fitting model with 1 continuous outcome and 1 major predictor
lm_fit <- glm_mod %>% fit(Weakness ~ RunnyNose, data = mydata)
tidy(lm_fit)

# fitting model with 1 continuous outcome and all predictors I chose
lm_fit <- glm_mod %>% fit(Weakness ~ RunnyNose + Headache + Sneeze + CoughYN + Fatigue + ChestCongestion + BodyTemp, data = mydata)
tidy(lm_fit)

# save fit results table  
#table_file = here("results", "resulttable.rds")
#saveRDS(lmtable, file = table_file)

  