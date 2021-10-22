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



#---------------------------
#---------------------------
#Ehsan starts here:


#Create recipe
Recipe_BT <- recipe(BodyTemp ~ ., data = train_data)

#Define model 
linear_mod <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")

#Create workflow that adds recipe and model
BT_lm_wflow <- 
  workflow() %>% 
  add_model(linear_mod) %>% 
  add_recipe(Recipe_BT)

# Using the Workflow, model fitting to Recipe_BT
BT_fit <- 
  BT_lm_wflow %>% 
  fit(data = train_data)

#See
BT_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

### Using Workflows to Make Predictions

#predict values in test data
predict(BT_fit, test_data)

#predictions with augment
BT_aug_test <- 
  augment(BT_fit, test_data)

### Using RMSE to Assess Model Fit

#Calculate RMSE for test data
BT_aug_test %>%
  rmse(truth = BodyTemp, .pred)

#Predict outcomes using augment in training data
BT_aug_train <- 
  augment(BT_fit, train_data)

#Calculate RSME for training data
BT_aug_train %>%
  rmse(truth = BodyTemp, .pred)



#Creates recipe
Recipe_BT2 <- recipe(BodyTemp ~ RunnyNose, data = train_data)

#Define model 
linear_mod <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")

#Create workflow that adds our recipe and model
BT2_wflow <- 
  workflow() %>% 
  add_model(linear_mod) %>% 
  add_recipe(Recipe_BT2)

#model fitting to workflow
BT2_fit <- 
  BT2_wflow %>% 
  fit(data = train_data)

#see
BT2_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

### Using Workflows to Make Predictions

#predict values in test data
predict(BT2_fit, test_data)


#Use the augment() function to predict outcomes in the test data.

#Predict outcomes using augment in test data
BT2_aug_test <- 
  augment(BT2_fit, test_data)

### Using RMSE to Assess Model Fit


#### Assessing fit for test data predictions

#Make RMSE for test data predictions.
BT2_aug_test %>%
  rmse(truth = BodyTemp, .pred)

#RMSE = 1.19

####RMSE for Body Temperature as a function of Runny Nose in Train Data.

#Predict outcomes using augment in training data.
BT2_aug_train <- 
  augment(BT2_fit, train_data)

#Calculate RMSE for training data.
BT2_aug_train %>%
  rmse(truth = BodyTemp, .pred)

#RMSE = 1.19


  