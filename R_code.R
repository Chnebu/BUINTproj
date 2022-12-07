library(tidyverse)
library(scales)
library(ggplot2)

#### IMPORT DATA

# Specify working directory7
#install.packages("here") #<- install once for location of the files!
#library(here) #<- for Location of the files!

#getwd() <-always starts at this level here!
#here("data", "real_estate_prices_CH.csv") #<- example for subfolder data, file quarterly-data-us-mortgage.csv


#setwd("C:/Users/leabo/Desktop/Project")

# Assign working directory as path to object
#wd <- "C:/Users/leabo/Desktop/Project"

# list file names in working directory
#list.files(wd, all.files=FALSE, full.names=FALSE, pattern=".csv")

# import/read in data files
real.estate.prices.CH <- read.csv(here("data", "real_estate_prices_CH.csv"))
View(real.estate.prices.CH)

#hehe

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

#convert to date
date.real.estate.prices.CH <- as.character(real.estate.prices.CH$Property.type)
real.estate.prices.CH$Property.type <- as.Date(date.real.estate.prices.CH, formats = "%Y/%m/%d")

# plot
ggplot(real.estate.prices.CH  %>% filter(real.estate.prices.CH$Property.type > '2009-01-01'), aes(x = Property.type))+
  geom_point(aes(y = private.apartements), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = single.family.houses), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = apartment.buildings), na.rm = TRUE, size = 2, color = "green")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("real estate prices CH")
