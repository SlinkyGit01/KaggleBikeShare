library(tidyverse)
library(tidymodels)
library(vroom)

## Change working directory

setwd(dir = "~/School/F2023/STAT348/STAT348/KaggleBikeShare")

## Cleaning step using dplyr

bt <- vroom("train.csv")

head(bt)

bt <- bt %>%
         mutate(weather = ifelse(weather == 4, 3, weather))

bt %>% 
  filter(weather == 4)

## Feature engineering at least 2 times with recipes

my_recipe <- recipe(count~., data=bt) %>% # Set model formula and d2
  step_time(datetime, features=c("hour", "minute")) %>% # Creating time of day variable
  step_mutate(weather = as.factor(weather)) %>% # Converting to factor
  step_scale(temp, humidity) # Standardize 'temp' and 'humidity'

prepped_recipe <- prep(my_recipe, training = bt) # Sets up the preprocessing using myDataS12
bt_processed <- bake(prepped_recipe, new_data=bt)

head(bt_processed, 10) # first ten lines for my new data
