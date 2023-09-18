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

##EDA

dplyr::glimpse(bike)
skimr::skim(bike)
DataExplorer::plot_intro(bike)
DataExplorer::plot_correlation(bike)
DataExplorer::plot_bar(bike)
DataExplorer::plot_histogram(bike)
DataExplorer::plot_missing(bike)
#GGally::ggpairs(bike)

##GGPlot

plot3 <- ggplot(data=bike, aes(x=temp, y=count, color = as.factor(season))) + geom_point() + geom_smooth(se=F) + theme(aspect.ratio = 1)

plot4 <- ggplot(data = bike, aes(x=windspeed, y=count, color = as.factor(season))) + geom_boxplot()

library(patchwork)
plot1 <- DataExplorer::plot_intro(bike)
plot2 <- DataExplorer::plot_correlation(bike)
plot1 + plot2 #side by side4
plot1 / plot2 #top and bottom5
(plot1 + plot3) / (plot2 + plot4) #4 panel plot

library(tidymodels)