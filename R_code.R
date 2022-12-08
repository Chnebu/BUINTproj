library(tidyverse)
library(scales)
library(ggplot2)

#### IMPORT DATA

# Specify working directory7
#install.packages("here") #<- install once for location of the files!
library(here) #<- for Location of the files!

#getwd() <-always starts at this level here!
#here("data", "real_estate_prices_CH.csv") #<- example for subfolder data, file quarterly-data-us-mortgage.csv


#setwd("C:/Users/leabo/Desktop/Project")

# Assign working directory as path to object
#wd <- "C:/Users/leabo/Desktop/Project"

# list file names in working directory
#list.files(wd, all.files=FALSE, full.names=FALSE, pattern=".csv")

# import/read in data files
ch.house.prices <- read.csv(here("data", "ch-house-prices.csv"))
ch.mortgage.rates <- read.csv(here("data", "ch-mortgage-rates.csv"))
us.mortgage.rates <- read.csv(here("data", "us-mortgage-rates.csv"))
us.house.prices <- read.csv(here("data", "us-house-prices.csv"))
View(ch.house.prices)
View(ch.mortgage.rates)
View(us.mortgage.rates)
View(us.house.prices)

################## CH House Prices #######################################################################

# add new row for averages
# privately owned apartments
private.apartements.average <- c(rowMeans(ch.house.prices[,2:6], na.rm = TRUE))
ch.house.prices <- cbind(ch.house.prices, private.apartements = private.apartements.average)

#view(real.estate.prices.CH[,2:6])

# single family houses
single.family.houses.average <- c(rowMeans(ch.house.prices[,7:11], na.rm = TRUE))
ch.house.prices <- cbind(ch.house.prices, single.family.houses = single.family.houses.average)

# apartment buildings (residential investment property)
apartment.buildings.average <- c(rowMeans(ch.house.prices[,12:14], na.rm = TRUE))
ch.house.prices <- cbind(ch.house.prices, apartment.buildings = apartment.buildings.average)

# delete rows from different sources, only keep averages
ch.house.prices <- ch.house.prices[,-2:-14]
View(ch.house.prices)

#convert to date
date.ch.house.prices <- as.character(ch.house.prices$Date)
ch.house.prices$Date <- as.Date(date.ch.house.prices, formats = "%Y/%m/%d")

# plot
ggplot(ch.house.prices  %>% filter(ch.house.prices$Date > '2009/01/01'), aes(x = Date))+
  geom_point(aes(y = private.apartements), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = single.family.houses), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = apartment.buildings), na.rm = TRUE, size = 2, color = "green")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH House prices")
          
#################### CH Mortgage Rates ###################################################

#convert to date
date.ch.mortgage.rates <- as.character(ch.mortgage.rates$Date)
ch.mortgage.rates$Date <- as.Date(date.ch.mortgage.rates, formats = "%Y/%m/%d")

names(ch.mortgage.rates)


# plot Average -> variable red, fixed blue
ggplot((ch.mortgage.rates), aes(x = Date))+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of..M..Mortgages.Var.Int.Rates...NL.BR..CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of..M..Mortgages.Var.Int.Rates...NL.BR..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "green")+
  
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "blue")+
#  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "green")+
  
#  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "blue")+

  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "black")+
  
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH Mortgage Rates")

# plot Sum -> variable red, fixed blue
ggplot((ch.mortgage.rates), aes(x = Date))+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...NL.BR..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...NL.BR..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...NL.BR..CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...NL.BR..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...NL.BR..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "red")+
  
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...Linked.BR..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...Linked.BR..CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...Linked.BR..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...Linked.BR..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "red")+
  
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates...CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "blue")+
  
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH Mortgage Rates")
          

#################### US Mortgage Rates ###################################################


#let's rename the columns from V1, V2,... to the names (they were manually extracted form the "auto-mpg.name" file
names(us.mortgage.rates) <- c("Date", "fixed.Rate.Avg.15.Year","fixed.Rate.Avg.30.Year","fixed.Rate.Avg.5.Year",
                              "Margin.for.5.1.Year.Adj","Origination.Fees.and.Discount.Points.for.15.Y",
                              "Origination.Fees.and.Discount.Points.for.30.Y","Origination.Fees.and.Discount.Points.for.5.Y")

#for better manipulation and readibilitty, let's transform the data.frame into a "tibble" object
us.mortgage.rates <- as_tibble(us.mortgage.rates)
View(us.mortgage.rates)

#     Jan's Code :)

#monthly <- ts(us.mortgage.rates, start = c(2009, 1), frequency = 12)
#quarterly <- aggregate(monthly, mean)
#View(quarterly)

#plot1 <- ggplot(data = us.mortgage.rates, mapping = aes(x = "30-Year Fixed Rate Avg", y="Date"))

# plot2 <- ggplot(data = us.mortgage.rates, mapping = aes(x = "Date", y = "30-Year Fixed Rate Avg"))+
#  geom_point(color = "#00AFBB", size = 5) +
#  geom_line(color = "#00AFBB", size = 2)
# print(plot2)

#convert to date
date.us.mortgage.rates <- as.character(us.mortgage.rates$Date)
us.mortgage.rates$Date <- as.Date(date.us.mortgage.rates, formats = "%Y/%m/%d")

# plot
ggplot((us.mortgage.rates), aes(x = Date, colour = Year))+
  geom_point(aes(y = fixed.Rate.Avg.15.Year), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = fixed.Rate.Avg.30.Year), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = fixed.Rate.Avg.5.Year), na.rm = TRUE, size = 2, color = "blue")+
#  geom_point(aes(y = Margin.for.5.1.Year.Adj), na.rm = TRUE, size = 2, color = "yellow")+
# margin thing excluded due to little change over the years (betw. 2.74, 2.76 only).
  geom_point(aes(y = Origination.Fees.and.Discount.Points.for.15.Y), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Origination.Fees.and.Discount.Points.for.30.Y), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Origination.Fees.and.Discount.Points.for.5.Y), na.rm = TRUE, size = 2, color = "blue")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  labs(caption = "Green: 30y, Red: 15y, Blue: 5y, Mortgages & Fees, Discount points")
  ggtitle("US Mortgage Rates")
  
  # what can we see: Banks somehow wanted to sell more short-term loans (probably less 
  # risk for them during rising rates....)
  
################### US House Prices ###################################

#convert to date
date.us.house.prices <- as.character(us.house.prices$Date)
us.house.prices$Date <- as.Date(date.us.house.prices, formats = "%Y/%m/%d")

# plot
ggplot((us.house.prices), aes(x = Date))+
  geom_point(aes(y = Real.Estate.Prices), na.rm = TRUE, size = 2, color = "red")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("US House Prices")


