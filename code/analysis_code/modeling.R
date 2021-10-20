###############################

#load needed packages
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidymodels)

#path to data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

# This enables the analysis to be reproducible when random numbers are used 
set.seed(111)
# Put 3/4 of the data into the training set
data_split <- initial_split(mydata, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# receipe for model with all predictors
data_rec <- recipe(CoughYN ~ ., data = train_data) 

# this is the logistic model with ordinary least square method
glm_mod <- logistic_reg() %>% set_engine("glm")

# workflow object combining model and receipe
wflow <- workflow() %>% add_model(glm_mod) %>% add_recipe(data_rec)

# model fitting
data_fit <- wflow %>% fit(data = train_data)
tidy(data_fit)

# prediction for model with all predictors
predict(data_fit, test_data)

# check ROC
aug <- augment(data_fit, test_data)
aug %>% select(CoughYN,.pred_class,.pred_Yes)
aug %>% roc_curve(truth = CoughYN, .pred_Yes) %>% autoplot()
aug %>% roc_auc(truth = CoughYN, .pred_Yes)
# ROC = 0.1 means the model is not useful at all

##################################################################################
# receipe for using one single predictor RunnyNose
data_single_predictor <- recipe(CoughYN ~ RunnyNose, data = train_data) 

# workflow object combining model and receipe
wflow_sp <- workflow() %>% add_model(glm_mod) %>% add_recipe(data_single_predictor)

# model fitting
data_fit_sp <- wflow_sp %>% fit(data = train_data)
tidy(data_fit_sp)

# prediction for model one predictor
predict(data_fit_sp, test_data)

# check ROC
aug_sp <- augment(data_fit_sp, test_data)
aug_sp %>% select(CoughYN,.pred_class,.pred_Yes)
aug_sp %>% roc_curve(truth = CoughYN, .pred_Yes) %>% autoplot()
aug_sp %>% roc_auc(truth = CoughYN, .pred_Yes)
# ROC = 0.358, not a useful model but performs much better than the previous one

# save fit results table  
#table_file = here("results", "resulttable.rds")
#saveRDS(lmtable, file = table_file)

  