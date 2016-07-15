library(ggplot2)
library(reshape)
library(plyr)
library(dplyr)


# Labels
labels <- c('Food','Nutrition','WASH','Agriculture','Health','Education',
            'Shelter','Protection','Other')

setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")
source('EN_load.R')
setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")

# Compare total humanitarian/dev budgets by country

