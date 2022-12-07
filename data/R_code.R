library(tidyverse)
library(scales)

#### IMPORT DATA

# Specify working directory7
setwd("C:/Users/leabo/Desktop/Project")

# Assign working directory as path to object
wd <- "C:/Users/leabo/Desktop/Project"

# list file names in working directory
list.files(wd, all.files=FALSE, full.names=FALSE, pattern=".csv")

# import/read in data files
real.estate.prices.CH <- read.csv("real_estate_prices_CH.csv")
View(real.estate.prices.CH)

# add new row for averages
# privately owned apartments
private.apartements.average <- c(rowMeans(real.estate.prices.CH[,2:6], na.rm = TRUE))
real.estate.prices.CH <- cbind(real.estate.prices.CH, private.apartements = private.apartements.average)

# single family houses
single.family.houses.average <- c(rowMeans(real.estate.prices.CH[,7:11], na.rm = TRUE))
real.estate.prices.CH <- cbind(real.estate.prices.CH, single.family.houses = single.family.houses.average)

# apartment buildings (residential investment property)
apartment.buildings.average <- c(rowMeans(real.estate.prices.CH[,12:14], na.rm = TRUE))
real.estate.prices.CH <- cbind(real.estate.prices.CH, apartment.buildings = apartment.buildings.average)

# delete rows from different sources, only keep averages
real.estate.prices.CH <- real.estate.prices.CH[,-2:-14]
View(real.estate.prices.CH)
