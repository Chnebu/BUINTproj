library(tidyverse)
library(scales)

#### IMPORT DATA

# Specify working directory7
setwd("C:/Users/leabo/Desktop/data")

# Assign working directory as path to object
wd <- "C:/Users/leabo/Desktop/data"

# list file names in working directory
list.files(wd, all.files=FALSE, full.names=FALSE, pattern=".csv")

# import/read in data files
interest.rates.US <- read.csv("interest_rates_US.csv")
interest.rates.CH <- read.csv("interest_rates_CH.csv")
real.estate.prices.US <- read.csv("real_estate_prices_US.csv")
real.estate.prices.CH <- read.csv("real_estate_prices_CH.csv")


#### Steps to be performed 

##### DATA CLEANING
# Step 1 - Handle missing values in data
# Step 2 - Identify and reduce noise in the data 
#          statistical techniques: outliers, averages, st. dev.
#          cluster analysis -> remove outliers by binning, regression or simple averages
# Step 3 - Find and eliminate erroneous data (odd values, other than outliers)

#### DATA TRANSFORMATION
# Step 1 - Normalize the data
# Step 2 - Discretize or aggregate the data
# Step 3 - Construct new attributes

#### DATA REDUCTION
# Step 1 - reduce number of attributes
# Step 2 - reduce number of records
# Step 3 - Balance skewed data



####################### interest rates CH ################################

# to check data type (we want Date)
str(interest.rates.CH)

#convert to date
dates.interest.rates.CH <- as.character(interest.rates.CH$Month)
interest.rates.CH$Month <- as.Date(dates.interest.rates.CH, formats = "%b/%Y",
                                  optional = FALSE, tz = "UTC")

# plot all years
ggplot(data = interest.rates.CH,
       mapping = aes(x = Month,
                     y = Variable.mortgages))+
  geom_point(size = 1)+
  geom_line(color = "red")+
  ggtitle("interest rates CH")


# plot filtered data (later than 2000)
filtered.interest.rates.CH <- interest.rates.CH %>% filter(interest.rates.CH$Month > '2000-01-01')

ggplot(data = filtered.interest.rates.CH,
       mapping = aes(x = Month,
                     y = Variable.mortgages))+
  geom_point(size = 1)+
  geom_line(color = "red")+
  ggtitle("interest rates CH after 2000")




####################### real estate prices CH ################################

# to check data type (we want Date)
str(real.estate.prices.CH)

#convert to date
dates.real.estate.CH <- as.character(real.estate.prices.CH$Month)
real.estate.prices.CH$Month <- as.Date(dates.real.estate.CH, formats = "%Y/%b",
                                               optional = FALSE, tz = "UTC")

# calculate mean of all the columns
data.without.date <- real.estate.prices.CH[,2:17]
row.average <- rowMeans(data.without.date, na.rm = TRUE)

real.estate.prices.CH <- cbind(real.estate.prices.CH, average = row.average)

# plot all years
ggplot(data = real.estate.prices.CH,
       mapping = aes(x = Month,
                     y = average))+
  geom_point(size = 1)+
  geom_line(color = "red")+
  ggtitle("real estate prices CH")



# plot filtered data
filtered.real.estate.CH <- real.estate.prices.CH %>% filter(real.estate.prices.CH$Month > '2000-01-01')

ggplot(data = filtered.real.estate.CH,
       mapping = aes(x = Month,
                     y = average))+
  geom_point(size = 1)+
  geom_line(color = "red")+
  ggtitle("real estate prices CH after 2000")




############# combine the two CH datasets #######################################

#join the two tables    <-------- what to do with the NA, missing data, 
#                                one table has all the months the other one only quarters

CH <- full_join(interest.rates.CH, real.estate.prices.CH, by = "Month")
CH <- CH[,-3:-246]

# functions to view the tables
View(CH)
View(interest.rates.CH)
View(real.estate.prices.CH)
View(interest.rates.US)
View(real.estate.prices.US)

# CORRELATION <------ Measures the relative strength of a linear relationship btw. 2 variables
# calculate correlation ("complete.obs" to make it ignore the NA values)
CH.correlation <- cor(CH$Variable.mortgages, CH$average, use = "complete.obs", method = "pearson")
### CORRELATION IS -0.49361  
### 1 or -1 would be a perfect correlation - 0 would be no correlation at all


## interest rates are plotted, real estate prices are colored
ggplot(data = CH,
       mapping = aes(x = Month,
                     y = average))+
  geom_point(aes(color = Variable.mortgages))+
  labs(x = "time period", y = "interest rates")+
  ggtitle("interest rates CH (color = real estate prices)")

# only data later than 2000
filtered.data.CH <- CH %>% filter(CH$Month > '2000-01-01')
View(filtered.data.CH)
                
ggplot(data = filtered.data.CH,
       mapping = aes(x = Month,
                     y = average))+
  geom_point(aes(color = Variable.mortgages))+
  labs(x = "time period", y = "interest rates")+
  ggtitle("interest rates CH after 2000 (color = real estate prices)")

## real estate prices are plotted, interest rates are colored
ggplot(data = CH,
       mapping = aes(x = Month,
                     y = average))+
  geom_point(aes(color = Variable.mortgages))+
  labs(x = "time period", y = "real estate price")+
  ggtitle("real estate prices CH (color = interest rates)")

# only data later than 2000
ggplot(data = filtered.data.CH,
       mapping = aes(x = Month,
                     y = Variable.mortgages))+
  geom_point(aes(color = average))+
  labs(x = "time period", y = "real estate price")+
  ggtitle("real estate prices CH after 2000 (color = interest rates)")


# two plots combined
ggplot(CH, aes(x = Month))+
  geom_point(aes(y = Variable.mortgages*15), na.rm = TRUE, size = 1)+
  geom_point(aes(y = average), na.rm = TRUE, color = CH$Variable.mortgages)+
  scale_y_continuous(sec.axis = sec_axis(trans=~./15, name= "interest rate"))+
  labs(x = "time period", y = "real estate price")+
  ggtitle("real estate prices and interest rates CH")

# two plots combined with data after 2000
ggplot(filtered.data.CH, aes(x = Month))+
  geom_point(aes(y = Variable.mortgages*15), na.rm = TRUE, size = 1)+
  geom_point(aes(y = average), na.rm = TRUE, color = filtered.data.CH$Variable.mortgages)+
  scale_y_continuous(sec.axis = sec_axis(trans=~./15, name= "interest rate"))+
  labs(x = "time period", y = "real estate price")+
  ggtitle("real estate prices and interest rates CH after 2000")


####################### interest rates US ################################

####### EXPLENATIO PTS:
# Mortgage points are the fees a borrower pays a mortgage lender in order to trim the interest rate on the loan. 
# Each point the borrower buys costs 1 percent of the mortgage amount. So, one point on a $300,000 mortgage would cost $3,000.
# Each point typically lowers the rate by 0.25 percent, 
# so one point would lower a mortgage rate of 4 percent to 3.75 percent for the life of the loan


# to check data type (we want Date)
str(interest.rates.US)

#convert to date
dates.interest.rates.US <- as.character(interest.rates.US$Month)
interest.rates.US$Month <- as.Date(dates.interest.rates.US, formats = "%Y/%b",
                                   optional = FALSE, tz = "UTC")

# plot all years -> Pts
ggplot(data = interest.rates.US,
       mapping = aes(x = Month,
                     y = Pts))+
  geom_smooth(span = 0.2)+
  geom_point(size = 1)+
  ggtitle("mortgage points US")

# plot filtered data -> Pts
filtered.interest.rates.US <- interest.rates.US %>% filter(interest.rates.US$Month > '2000-01-01')

ggplot(data = filtered.interest.rates.US,
       mapping = aes(x = Month,
                     y = Pts))+
  geom_smooth(span = 0.2)+
  geom_point(size = 1)+
  ggtitle("mortgage points US after 2000")

# plot all years -> Rate
ggplot(data = interest.rates.US,
       mapping = aes(x = Month,
                     y = Rate))+
  geom_smooth(span = 0.3)+
  geom_point(size = 1)+
  ggtitle("interest rate US")

# plot filtered data -> Rate
filtered.interest.rates.US <- interest.rates.US %>% filter(interest.rates.US$Month > '2000-01-01')

ggplot(data = filtered.interest.rates.US,
       mapping = aes(x = Month,
                     y = Rate))+
  geom_point(size = 1)+
  ggtitle("interest rate US after 2000")

####################### real estate prices US ################################

# to check data type (we want Date)
str(interest.rates.US)

#convert to date
dates.real.estate.prices.US <- as.character(real.estate.prices.US$Month)
real.estate.prices.US$Month <- as.Date(dates.real.estate.prices.US, formats = "%Y/%b",
                                   optional = FALSE, tz = "UTC")

# plot all years
ggplot(data = real.estate.prices.US,
       mapping = aes(x = Month,
                     y = ASPUS))+
  geom_point(size = 1)+
  geom_line(color = "red")+
  ggtitle("real estate prices US")


# plot filtered data
filtered.real.estate.US <- real.estate.prices.US %>% filter(real.estate.prices.US$Month > '2000-01-01')

ggplot(data = filtered.real.estate.US,
       mapping = aes(x = Month,
                     y = ASPUS))+
  geom_point(size = 1)+
  geom_line(color = "red")+
  ggtitle("real estate prices US 2000")

############# combine the two US datasets #######################################

US <- full_join(interest.rates.US, real.estate.prices.US, by = "Month")
US[,3:6]
View(US)

# CORRELATION <------ Measures the relative strength of a linear relationship btw. 2 variables
# calculate correlation ("complete.obs" to make it ignore the NA values)
US.correlation <- cor(US$Rate, US$ASPUS, use = "complete.obs", method = "pearson")
### CORRELATION IS -0.7613069 
### 1 or -1 would be a perfect correlation - 0 would be no correlation at all


## interest rates are plotted, real estate prices are colored
ggplot(data = US,
       mapping = aes(x = Month,
                     y = Rate))+
  geom_point(aes(color = ASPUS))+
  labs(x = "time period", y = "interest rates")+
  ggtitle("interest rates US, color = real estate prices")

# only data later than 2000
filtered.data.US <- US %>% filter(US$Month > '2000-01-01')
View(filtered.data.US)

ggplot(data = filtered.data.US,
       mapping = aes(x = Month,
                     y = Rate))+
  geom_point(aes(color = ASPUS))+
  geom_smooth(span = 0.3)+
  labs(x = "time period", y = "interest rates")+
  ggtitle("interest rates US after 2000, color = real estate prices")

## real estate prices are plotted, interest rates are colored
ggplot(data = US,
       mapping = aes(x = Month,
                     y = ASPUS))+
  geom_point(aes(color = Rate, size = Rate))+
  labs(x = "time period", y = "real estate price")+
  ggtitle("real estate prices, color = interest rates")

# only data later than 2000
ggplot(data = filtered.data.US,
       mapping = aes(x = Month,
                     y = ASPUS))+
  geom_point(aes(color = Rate, size = Rate))+
  labs(x = "time period", y = "real estate price")+
  ggtitle("interest rates US after 2000, color = real estate prices")


# two plots combined
ggplot(US, aes(x = Month))+
  geom_point(aes(y = Rate*10000), na.rm = TRUE, size = 1)+
  geom_point(aes(y = ASPUS), na.rm = TRUE, color = US$Rate)+
  scale_y_continuous(sec.axis = sec_axis(trans=~./10000, name= "interest rate"))+
  labs(x = "time period", y = "real estate price")+
  ggtitle("interest rates and real estate prices US")


# two plots combined with data after 2000
ggplot(filtered.data.US, aes(x = Month))+
  geom_point(aes(y = Rate*30000), na.rm = TRUE, size = 1)+
  geom_point(aes(y = ASPUS), na.rm = TRUE, color = filtered.data.US$Rate)+
  scale_y_continuous(sec.axis = sec_axis(trans=~./30000, name= "interest rate"))+
  labs(x = "time period", y = "real estate price")+
  ggtitle("interest rates and real estate prices US after 2000")
