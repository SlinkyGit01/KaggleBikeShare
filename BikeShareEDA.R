##
## Bike Share EDA Code
##

## Set working directory

setwd(dir = "~/School/F2023/STAT348/STAT348/KaggleBikeShare")

## Libraries
library(tidyverse)
library(vroom)

## Read in the Data
bike <- vroom("train.csv")
