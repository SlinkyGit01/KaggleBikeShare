library(tidyverse)
library(tidymodels)
library(vroom)

## Change working directory

setwd(dir = "~/School/F2023/STAT348/STAT348/KaggleBikeShare")



########################### Cleaning step using dplyr ##########################



bt <- vroom("train.csv")

head(bt)

bt <- bt %>%
         mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
        
bt %>% 
  filter(weather == 4)

bt <- select(bt, -c(casual, registered))

## Feature engineering at least 2 times with recipes

my_recipe <- recipe(count~., data=bt) %>% # Set model formula and d2
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_time(datetime, features=c("hour")) %>% # Creating time of day variable
  step_mutate(weather = as.factor(weather))# %>% # Converting to factor
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

