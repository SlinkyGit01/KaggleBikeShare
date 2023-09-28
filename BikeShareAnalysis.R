library(tidyverse)
library(tidymodels)
library(vroom)

## Change working directory

setwd(dir = "~/School/F2023/STAT348/STAT348/KaggleBikeShare")



########################### Cleaning step using dplyr ##########################



bt <- vroom("train.csv")

head(bt)

bt <- bt %>%
         mutate(weather = ifelse(weather == 4, 3, weather)) 
        
bt %>% filter(weather == 4)

bt <- select(bt, -c(casual, registered))

## Feature engineering at least 2 times with recipes

my_recipe <- recipe(count~., data=bt) %>% # Set model formula and d2
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_time(datetime, features=c("hour")) %>% # Creating time of day variable
  step_mutate(weather = as.factor(weather)) # %>% # Converting to factor
 # step_scale(temp, humidity) # Standardize 'temp' and 'humidity'

prepped_recipe <- prep(my_recipe, training = bt) # Sets up the preprocessing using myDataS12
bt_processed <- bake(prepped_recipe, new_data=bt)

head(bt_processed, 10) # first ten lines for my baked data



################################# LR ###########################################



library(tidymodels)

my_mod <- linear_reg() %>% #Type of model3
  set_engine("lm") # Engine = What R function to use4

bike_workflow <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(my_mod) %>% 
fit(data = bt) # Fit the workflow9

btest <- vroom("test.csv")

bike_predictions <- predict(bike_workflow, new_data=btest) # Use fit to predict

bike_predictions[bike_predictions < 0] <- 0
bike_predictions <- cbind(btest$datetime, bike_predictions) %>% 
  rename(datetime = "btest$datetime", count = ".pred")

#vroom_write(bike_predictions, file = "bikePred.csv", delim = ",")

#write.csv(bike_predictions, file = "BikePred2.csv", quote = F, row.names = F)

library(lubridate)

bike_predictions <- data.frame(
  datetime = btest$datetime,
  count = bike_predictions
)

# Format the datetime column to include the time portion
bike_predictions$datetime <- format(bike_predictions$datetime, "%Y-%m-%d %H:%M:%S")

bike_predictions <- data.frame(
  datetime = format(btest$datetime, "%Y-%m-%d %H:%M:%S"),
  count = bike_predictions
)

bike_predictions <- select(bike_predictions, c(datetime, count.count.count))

colnames(bike_predictions) <- c("datetime", "count")

# Write the data to a CSV file
write.csv(bike_predictions, file = "BikePred2.csv", quote = FALSE, row.names = FALSE)



############################ Poisson LR ########################################



library(tidymodels)

library(poissonreg)

pois_mod <- poisson_reg() %>% #Type of model3
  set_engine("glm") # GLM = generalized linear model45

bike_pois_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(pois_mod) %>% fit(data = bt) # Fit the workflow910

btest <- vroom("test.csv")

bike_predictions <- predict(bike_pois_workflow, new_data=btest) # Use fit to predict

bike_predictions <- cbind(btest$datetime, bike_predictions) %>% 
  rename(datetime = "btest$datetime", count = ".pred")

#vroom_write(bike_predictions, file = "bikePred.csv", delim = ",")

#write.csv(bike_predictions, file = "BikePred2.csv", quote = F, row.names = F)

library(lubridate)

bike_predictions <- data.frame(
  datetime = btest$datetime,
  count = bike_predictions
)

# Format the datetime column to include the time portion
bike_predictions$datetime <- format(bike_predictions$datetime, "%Y-%m-%d %H:%M:%S")

bike_predictions <- data.frame(
  datetime = format(btest$datetime, "%Y-%m-%d %H:%M:%S"),
  count = bike_predictions
)

bike_predictions <- select(bike_predictions, c(datetime, count.count.count))

colnames(bike_predictions) <- c("datetime", "count")

# Write the data to a CSV file
write.csv(bike_predictions, file = "BikePredP.csv", quote = FALSE, row.names = FALSE)



############################# DR.H CODE FOR POISSON #####################



my_recipe <- recipe(count~., data=bt) %>% # Set model formula and d2
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime)

prepped_recipe <- prep(my_recipe, training = bt) # Sets up the preprocessing using myDataS12
bt_processed <- bake(prepped_recipe, new_data=bt)

library(tidymodels)

library(poissonreg)

pois_mod <- poisson_reg() %>% #Type of model3
  set_engine("glm") # GLM = generalized linear model45

bike_pois_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(pois_mod) %>% fit(data = bt) # Fit the workflow910

btest <- vroom("test.csv")

bike_predictions <- predict(bike_pois_workflow, new_data=btest) # Use fit to predict

bike_predictions <- cbind(btest$datetime, bike_predictions) %>% 
  rename(datetime = "btest$datetime", count = ".pred")

#vroom_write(bike_predictions, file = "bikePred.csv", delim = ",")

#write.csv(bike_predictions, file = "BikePred2.csv", quote = F, row.names = F)

library(lubridate)

bike_predictions <- data.frame(
  datetime = btest$datetime,
  count = bike_predictions
)

# Format the datetime column to include the time portion
bike_predictions$datetime <- format(bike_predictions$datetime, "%Y-%m-%d %H:%M:%S")

bike_predictions <- data.frame(
  datetime = format(btest$datetime, "%Y-%m-%d %H:%M:%S"),
  count = bike_predictions
)

bike_predictions <- select(bike_predictions, c(datetime, count.count.count))

colnames(bike_predictions) <- c("datetime", "count")

# Write the data to a CSV file
write.csv(bike_predictions, file = "BikePredP2.csv", quote = FALSE, row.names = FALSE)



############################### Penalized Regression ###########################



bt <- vroom("train.csv")

head(bt)

bt <- bt %>%
  mutate(weather = ifelse(weather == 4, 3, weather))

bt <- select(bt, -c(casual, registered))

## Feature engineering at least 2 times with recipes

my_recipe <- recipe(count~., data=bt) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_mutate(datetime=factor(datetime, levels=1:24)) %>% 
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())


library(tidymodels)

library(poissonreg)

logTrainSet <- bt %>%
  mutate(count=log(count))

preg_model <- linear_reg(penalty=0, mixture=1) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data=logTrainSet)

btest <- vroom("test.csv")

## Get Predictions for test set AND format for Kaggle
log_lin_preds <- predict(preg_wf, new_data = btest) %>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., btest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle## Write predictions to CSV
  
vroom_write(x=log_lin_preds, file="bikePredPenalized1.csv", delim=",")



############################## Tuning Models ###################################



library(tidymodels)
library(poissonreg) #if you want to do penalized, poisson regression23 ## Penalized regression model

bt <- vroom("train.csv")

head(bt)

bt <- bt %>%
  mutate(weather = ifelse(weather == 4, 3, weather))

bt <- select(bt, -c(casual, registered))

## Feature engineering at least 2 times with recipes

my_recipe <- recipe(count~., data=bt) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

logTrainSet <- bt %>%
  mutate(count=log(count))

preg_model <- linear_reg(penalty=tune(),mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R ## Set Workflow

btest <- vroom("test.csv")

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) #%>%
  #fit(data=logTrainSet)

tuning_grid <- grid_regular(penalty(),mixture(),levels = 5) ## L^2 total tuning possibilities, you choose L
## Split data for CV
folds <- vfold_cv(logTrainSet, v = 5, repeats=1) # Choose what value v is

## Run the CV1
CV_results <- preg_wf %>% tune_grid(resamples=folds, grid=tuning_grid, metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL56
## Plot Results (example)7
collect_metrics(CV_results) %>% # Gathers metrics into DF8
  filter(.metric=="rmse") %>% ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) + geom_line()
## Find Best Tuning Parameters13
bestTune <- CV_results %>% select_best("rmse")

## Finalize the Workflow & fit it1
final_wf <- preg_wf %>% finalize_workflow(bestTune) %>% fit(data=logTrainSet)## Predict7
final_wf %>% predict(new_data = btest)

## Get Predictions for test set AND format for Kaggle
log_lin_preds <- predict(final_wf, new_data = btest) %>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., btest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle## Write predictions to CSV

vroom_write(x=log_lin_preds, file="bikePredPenalized2.csv", delim=",")



########################### Regression Trees ##################################



#install.packages("rpart")

library(tidymodels)

bt <- vroom("train.csv")

head(bt)

bt <- bt %>%
  mutate(weather = ifelse(weather == 4, 3, weather))

bt <- select(bt, -c(casual, registered))

library(tidymodels)
my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>% #Type of model6
  set_engine("rpart") %>% # Engine = What R function to use7
  set_mode("regression")

## Create a workflow with model & recipe10

my_recipe <- recipe(count~., data=bt) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

logTrainSet <- bt %>%
  mutate(count=log(count))

mod_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

## Set up grid of tuning values

tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n(),
                            levels = 5) ## L^2 total tuning possibilities, you choose L

## Set up K-fold CV

folds <- vfold_cv(logTrainSet, v = 5, repeats=1)

## Find best tuning parameters

CV_results <- mod_wf %>% tune_grid(resamples=folds, grid=tuning_grid, metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL56

bestTune <- CV_results %>% select_best("rmse")

## Finalize the Workflow & fit it1
final_wf <- mod_wf %>% finalize_workflow(bestTune) %>% fit(data=logTrainSet)## Predict7
final_wf %>% predict(new_data = btest)

## Get Predictions for test set AND format for Kaggle
log_lin_preds <- predict(final_wf, new_data = btest) %>% #This predicts log(count)
  mutate(.pred=exp(.pred)) %>% # Back-transform the log to original scale
  bind_cols(., btest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle## Write predictions to CSV

vroom_write(x=log_lin_preds, file="bikePredRegressionTree.csv", delim=",")

## Finalize workflow and predict




















