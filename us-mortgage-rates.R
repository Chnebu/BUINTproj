library(tidyverse)
library(dplyr)
#install.packages("pastecs")
#(pastecs)

DS <- read.table("OneDrive - Hochschule Luzern/BUINT Data/r Proj/data/timeseries-data-mortgage.csv", sep=",", quote=",", comment.char="", na.strings = "NA", colClasses = NA)

View(DS)

#let's rename the columns from V1, V2,... to the names (they were manually extracted form the "auto-mpg.name" file
names(DS) <- c("Date", "15-Year Fixed Rate Avg","30-Year Fixed Rate Avg","5/1-Year Adjustable Rate","Margin for 5/1-Year Adj","Origination Fees and Discount Points for 15-Y","Origination Fees and Discount Points for 30-Y","5/1-Year Adjustable")

#for better manipulation and readibilitty, let's transform the data.frame into a "tibble" object
DStibble <- as_tibble(DS)
View(DStibble)

monthly <- ts(DStibble, start = c(2009, 1), frequency = 12)
quarterly <- aggregate(monthly, mean)
View(quarterly)

