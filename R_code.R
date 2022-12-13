library(tidyverse)
library(scales)
library(ggplot2)
library(reshape)
library(reshape2)
library(e1071)
library(here) #<- for Location of the files!
library(performance)
library(magrittr)
library(see)

##### DATA CLEANING
# Step 1 - Handle missing values in data
# Step 2 - Identify and reduce noise in the data 
#          statistical techniques: outliers, averages, st. dev.
#          cluster analysis -> remove outliers by binning, regression or simple averages
# Step 3 - Find and eliminate erroneous data (odd values, other than outliers)

#### DATA TRANSFORMATION
# Step 1 - Normalize the data -DONE
# Step 2 - Discretize or aggregate the data - DONE
# Step 3 - Construct new attributes - DONE

#### DATA REDUCTION
# Step 1 - reduce number of attributes - DONE 
# Step 2 - reduce number of records - DONE
# Step 3 - Balance skewed data - 

#### IMPORT DATA

# import/read in data files
ch.house.prices <- read.csv(here("data", "ch-house-prices.csv"))
ch.mortgage.rates <- read.csv(here("data", "ch-mortgage-rates.csv"))
us.mortgage.rates <- read.csv(here("data", "us-mortgage-rates.csv"))
us.house.prices <- read.csv(here("data", "us-house-prices.csv"))
us.house.prices.2nd.df <- read.csv(here("data", "zillowPriceIndex.csv"))
us.house.prices.1991 <- read.csv(here("data", "us-house-prices-1991.csv"))
us.mortgage.rates.1991 <- read.csv(here("data", "us-mortgage-rates-1991.csv"))
#View(ch.house.prices)
#View(ch.mortgage.rates)
#View(us.mortgage.rates)
#View(us.house.prices)
#View(us.house.prices.2nd.df)



################## CH House Prices #######################################################################



summary(ch.house.prices)
View(ch.house.prices)

ch.house.prices <- ch.house.prices[,-2]
ch.house.prices <- ch.house.prices[,-6]
# fill in missing values - same as next column, since it behaves very similar
ch.house.prices[1,10] <- ch.house.prices[1,11]
ch.house.prices[2,10] <- ch.house.prices[2,11]
ch.house.prices[3,10] <- ch.house.prices[3,11]
ch.house.prices[4,10] <- ch.house.prices[4,11]

ch.house.prices[55,12] <- ch.house.prices[54,12]

# add new column for averages
# privately owned apartments
private.apartements.average <- c(rowMeans(ch.house.prices[,2:5], na.rm = TRUE))
ch.house.prices <- cbind(ch.house.prices, private.apartements = private.apartements.average)

#view(real.estate.prices.CH[,2:6])

# single family houses
single.family.houses.average <- c(rowMeans(ch.house.prices[,6:9], na.rm = TRUE))
ch.house.prices <- cbind(ch.house.prices, single.family.houses = single.family.houses.average)

# apartment buildings (residential investment property)
apartment.buildings.average <- c(rowMeans(ch.house.prices[,10:12], na.rm = TRUE))
ch.house.prices <- cbind(ch.house.prices, apartment.buildings = apartment.buildings.average)

# create total average 
house.prices.average <- c(rowMeans(ch.house.prices[,12:14], na.rm = TRUE))
ch.house.prices <- cbind(ch.house.prices, total.house.prices.average = house.prices.average)

# for analysis purposes
# reshape table so that column name is an variable
ch.house.prices.for.boxplot <- melt(ch.house.prices, na.rm = FALSE, name = 'columns')
View(ch.house.prices.for.boxplot)

ch.house.prices.for.boxplot %>%
  ggplot(aes(x = variable, y = value), na.rm = TRUE)+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 60,vjust = 0.5,hjust = 1))+
  ggtitle("Boxplot CH House Prices - to find outliers")
# we can see that there are outliers in "privately.owned.appartments.3" (the outliers are too low)
# and "privately.owned.appartments.3" (the outliers are too high)


# delete rows from different sources, only keep averages
ch.house.prices <- ch.house.prices[,-2:-11]
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
  labs(caption = "Green: apartment.buildings, Red: private.apartments, Blue: single.family.houses")+
  ggtitle("CH House prices")




          
#################### CH Mortgage Rates ###################################################

#convert to date
date.ch.mortgage.rates <- as.character(ch.mortgage.rates$Date)
ch.mortgage.rates$Date <- as.Date(date.ch.mortgage.rates, formats = "%Y/%m/%d")

summary(ch.mortgage.rates)
View(ch.mortgage.rates)

# plot Average -> variable red, fixed blue
ggplot((ch.mortgage.rates), aes(x = Date))+
  #-> VARIABLE INTEREST RATES - NL.BR
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of..M..Mortgages.Var.Int.Rates...NL.BR..CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of..M..Mortgages.Var.Int.Rates...NL.BR..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "orange")+
  #geom_line(aes(y = average.int.rates.nl.br), na.rm = TRUE, size = 1, color = "red")+
  
  #VARIABLE INTEREST RATES - LINKED BR
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "purple")+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "green")+
  #geom_line(aes(y = average.int.rates.linked.br), na.rm = TRUE, size = 1, color = "red")+
  
  #FIXED INTEREST RATES
  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Average.of.M..Mortgages.with.fixed.interest.rates..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "black")+
  #geom_line(aes(y = average.fixed.int.rates), na.rm = TRUE, size = 1, color = "red")+
  
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  labs(caption = "variable NL BR: green, orange (5-15Mio), 
       variable Linked BR: lower green, purple(100k-500k), red(50k-100k), 
       fixed int rates: blue, black (5-15Mio)")+
  ggtitle("CH Mortgage Rates")


# INTERPRETATION AND CLUSTERING

# calculate mean of [2] Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.50k...100k.  
#                   [4] Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.100k...500k. 
#                   [6] Average.of..M..Mortgages.Var.Int.Rates...NL.BR..CHF.500k...1.Mio. 
#                   [8] Average.of..M..Mortgages.Var.Int.Rates...NL.BR..CHF.1.Mio...5.Mio. 
# -> these are the 4 upper green dots -> VARIABLE INTEREST RATES - NL.BR

# Orange belongs to the same category but is slightly off -> should it be included ??????????????
# ->               [10] Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.5.Mio...15.Mio.




# calculate mean of [16] Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.500k...1.Mio.  
#                   [18] Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.1.Mio...5.Mio.  
#                   [20] Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.5.Mio...15.Mio.  
# -> these are the 3 lower dark green dots -> VARIABLE INTEREST RATES - LINKED BR

# Red and Purple belong to the same category but are a bit off (should they be included ?????????)
# Red ->            [12] Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.50k...100k. 
# Purple ->         [14] Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.100k...500k. 



# calculate mean of [22] Average.of.M..Mortgages.with.fixed.interest.rates..CHF.50k...100k.
#                   [24] Average.of.M..Mortgages.with.fixed.interest.rates..CHF.100k...500k.
#                   [26] Average.of.M..Mortgages.with.fixed.interest.rates..CHF.500k...1.Mio.
#                   [28] Average.of.M..Mortgages.with.fixed.interest.rates..CHF.1.Mio...5.Mio.
# -> these are the blue dots -> FIXED INTEREST RATES

# the black dots are a little bit off, I don't know if they should be included ????????
# ->                [30] Average.of.M..Mortgages.with.fixed.interest.rates..CHF.5.Mio...15.Mio

### DECISION -> I included EVERYTHING, because they belong to the same category -> Jan you can change it if you want :)


# Add averages as a new column to the table "ch.mortgage.rates"
# VARIABLE INTEREST RATES - NL.BR
average.int.rates.nl.br <- rowMeans(select(ch.mortgage.rates,c("Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.50k...100k.",  
                                                           "Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.100k...500k.", 
                                                           "Average.of..M..Mortgages.Var.Int.Rates...NL.BR..CHF.500k...1.Mio.",
                                                           "Average.of..M..Mortgages.Var.Int.Rates...NL.BR..CHF.1.Mio...5.Mio.",
                                                           "Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.5.Mio...15.Mio.")),
                                                            na.rm = TRUE)
ch.mortgage.rates <- cbind(ch.mortgage.rates, ch.average.int.rates.nl.br = average.int.rates.nl.br)

# VARIABLE INTEREST RATES - LINKED BR
average.int.rates.linked.br <- rowMeans(select(ch.mortgage.rates,c("Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.500k...1.Mio.",  
                                                            "Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.1.Mio...5.Mio.", 
                                                            "Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.5.Mio...15.Mio.",
                                                            "Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.50k...100k.",
                                                            "Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.100k...500k.")),
                                                             na.rm = TRUE)
ch.mortgage.rates <- cbind(ch.mortgage.rates, ch.average.int.rates.linked.br = average.int.rates.linked.br)

# FIXED INTEREST RATES
average.fixed.int.rates <- rowMeans(select(ch.mortgage.rates,c("Average.of.M..Mortgages.with.fixed.interest.rates..CHF.50k...100k.",  
                                                              "Average.of.M..Mortgages.with.fixed.interest.rates..CHF.100k...500k.", 
                                                              "Average.of.M..Mortgages.with.fixed.interest.rates..CHF.500k...1.Mio.",
                                                              "Average.of.M..Mortgages.with.fixed.interest.rates..CHF.1.Mio...5.Mio.",
                                                              "Average.of.M..Mortgages.with.fixed.interest.rates..CHF.5.Mio...15.Mio.")),
                                                              na.rm = TRUE)
ch.mortgage.rates <- cbind(ch.mortgage.rates, ch.average.fixed.int.rates = average.fixed.int.rates)
View(ch.mortgage.rates)




# plot Sum -> NL BR variable = red, Linked BR variable = orange, fixed = blue
ggplot((ch.mortgage.rates), aes(x = Date))+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...NL.BR..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...NL.BR..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...NL.BR..CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...NL.BR..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...NL.BR..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "red")+
  
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...Linked.BR..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "orange")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...Linked.BR..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "orange")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...Linked.BR..CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "orange")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...Linked.BR..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "orange")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.Var.Int.Rates...Linked.BR..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "orange")+
  
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates..CHF.50k...100k.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates..CHF.100k...500k.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates...CHF.500k...1.Mio.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates..CHF.1.Mio...5.Mio.), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = Sum.of.N.Loan.Mortgages.with.fixed.interest.rates..CHF.5.Mio...15.Mio.), na.rm = TRUE, size = 2, color = "blue")+
  
  labs(caption = "variable NL BR: red, 
       variable Linked BR: orange, 
       fixed int rates: blue")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH Sum of Loan Mortgages")





          

#################### US Mortgage Rates ###################################################


# rename the colmns
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

# since the fixed rate seems to be somehow similar we create a new column for the average
fixed.rate.average <- c(rowMeans(us.mortgage.rates[,2:5], na.rm = TRUE))
us.mortgage.rates <- cbind(us.mortgage.rates, us.fixed.rate.average = fixed.rate.average)

# same for the origination fees and discount points
fees.and.discount.points.average <- c(rowMeans(us.mortgage.rates[,6:8], na.rm = TRUE))
us.mortgage.rates <- cbind(us.mortgage.rates, us.fees.and.discount.points.average = fees.and.discount.points.average)

# plot
ggplot((us.mortgage.rates), aes(x = Date, colour = Year))+
  geom_point(aes(y = fixed.Rate.Avg.15.Year), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = fixed.Rate.Avg.30.Year), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = fixed.Rate.Avg.5.Year), na.rm = TRUE, size = 2, color = "blue")+
# geom_line(aes(y = us.fixed.rate.average), na.rm = TRUE, size = 1, color = "red")+        
# geom_point(aes(y = Margin.for.5.1.Year.Adj), na.rm = TRUE, size = 2, color = "yellow")+
# margin thing excluded due to little change over the years (betw. 2.74, 2.76 only).
  geom_point(aes(y = Origination.Fees.and.Discount.Points.for.15.Y), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = Origination.Fees.and.Discount.Points.for.30.Y), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = Origination.Fees.and.Discount.Points.for.5.Y), na.rm = TRUE, size = 2, color = "blue")+
#  geom_line(aes(y = us.fees.and.discount.points.average), na.rm = TRUE, size = 1, color = "red")+  
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  labs(caption = "Green: 30y, Red: 15y, Blue: 5y, Mortgages & Fees, Discount points")+
  ggtitle("US Mortgage Rates")
  

  # what can we see: Banks somehow in 2020 wanted to sell more short-term loans (probably less 
  # risk for them during times of rising rates, bc. customers have to ajust in 5y.)


  
################### US House Prices ##############################################

#convert to date
date.us.house.prices <- as.character(us.house.prices$Date)
us.house.prices$Date <- as.Date(date.us.house.prices, formats = "%Y/%m/%d")

# plot
ggplot((us.house.prices), aes(x = Date))+
  geom_point(aes(y = Real.Estate.Prices), na.rm = TRUE, size = 2, color = "red")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("US House Prices")






################## Join the US data ###########################################
#us <- na.omit(us)
summary(us)

us <- full_join(us.mortgage.rates, us.house.prices, by = "Date")
View(us)

# There is no skewness to be corrected

# CORRELATION <------ Measures the relative strength of a linear relationship btw. 2 variables
# calculate correlation ("complete.obs" to make it ignore the NA values)
us.correlation <- cor(us$us.fixed.rate.average, us$Real.Estate.Prices, use = "complete.obs", method = "pearson")
us.correlation
#   CORRELATION IS -0.2065972   	
### 1 or -1 would be a perfect correlation - 0 would be no correlation at all


model.us <- lm(us$us.fixed.rate.average~ us$Real.Estate.Prices, data=us)
summary(model.us)
# p-value: 0.1339 <- we cannot reject H0 - the model has no relevance


#### SOMETHING IS WRONG, correlation is  very little, can this be ? :(

### We are gonna add another data set just to make sure we have accurate and treatable data



####### US data 1991 ####################################

#us <- na.omit(us)
summary(us.house.prices.1991)
summary(us.mortgage.rates.1991)

us.1991 <- full_join(us.house.prices.1991, us.mortgage.rates.1991, by = "Date")
View(us.1991)

# There is no skewness to be corrected

# CORRELATION <------ Measures the relative strength of a linear relationship btw. 2 variables
# calculate correlation ("complete.obs" to make it ignore the NA values)
us.correlation.1991.30y <- cor(us.1991$house.prices.1991, us.1991$mortgage.30y, use = "complete.obs", method = "pearson")
us.correlation.1991.30y
#   CORRELATION IS -0.8325875  	
### 1 or -1 would be a perfect correlation - 0 would be no correlation at all


model.us.1991.30y <- lm(us.1991$house.prices.1991~ us.1991$mortgage.30y, data=us.1991 )
summary(model.us.1991.30y)
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              533994      16202   32.96   <2e-16 ***
#  us.1991$mortgage.30y    -44938       2696  -16.67   <2e-16 ***

# Multiple R-squared:  0.6932,	Adjusted R-squared:  0.6907 
# F-statistic: 277.9 on 1 and 123 DF,  p-value: < 2.2e-16
# p-value < 0.05 model is relevant


us.correlation.1991.15y <- cor(us.1991$house.prices.1991, us.1991$mortgage.15y, use = "complete.obs", method = "pearson")
us.correlation.1991.15y
#   CORRELATION IS -0.8279419 	
### 1 or -1 would be a perfect correlation - 0 would be no correlation at all

model.us.1991.15y <- lm(us.1991$house.prices.1991~ us.1991$mortgage.15y, data=us.1991 )
summary(model.us.1991.15y)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             533994      16202   32.96   <2e-16 ***
#  us.1991$mortgage.30y   -44938       2696  -16.67   <2e-16 ***

# Multiple R-squared:  0.6932,	Adjusted R-squared:  0.6907 
# F-statistic: 277.9 on 1 and 123 DF,  p-value: < 2.2e-16
# p-value < 0.05 model is relevant

#convert to date
date.us.1991 <- as.character(us.1991$Date)
us.1991$Date <- as.Date(date.us.1991, formats = "%Y/%m/%d")

# two plots combined - interest rates and house prices
us.1991 %>%
ggplot(aes(x = Date))+
  geom_point(aes(y = mortgage.30y*50000), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = mortgage.15y*50000), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = house.prices.1991), na.rm = TRUE, color = "black", size = 2)+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  scale_y_continuous(sec.axis = sec_axis(trans=~./50000, name= "interest rates"))+
  labs(x = "time period", y = "house prices")+
  labs(caption = "US mortgage 30y: green, 
       US mortgage 15y: red, 
       house prices: black")+
  ggtitle("interest rates and house prices since 1991 US")

# two plots combined - interest rates and house prices - interest rates maped against house prices
ggplot(us.1991, aes(x = house.prices.1991))+
  geom_point(aes(y = mortgage.30y), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = mortgage.15y), na.rm = TRUE, size = 2, color = "red")+
  labs(caption = "US mortgage 30y: green, 
       US mortgage 15y: red")+
  labs(x = "house prices", y = "interest rates")+
  ggtitle("US CORRELATION since 1991 - interest rates and house prices")

  # two plots combined - interest rates and house prices - with a LINEAR MODEL
ggplot(us.1991, aes(x = house.prices.1991))+
  geom_point(aes(y = mortgage.30y), na.rm = TRUE, size = 2, color = "green")+
  geom_smooth(aes(y = mortgage.30y), na.rm = TRUE, size = 1, color = "green", method = lm)+
  geom_point(aes(y = mortgage.15y), na.rm = TRUE, size = 2, color = "red")+
  geom_smooth(aes(y = mortgage.15y), na.rm = TRUE, size = 1, color = "red", method = lm)+
  labs(caption = "US mortgage 30y: green, 
       US mortgage 15y: red")+
  labs(x = "house prices", y = "interest rates")+
  ggtitle("US CORRELATION since 1991 - interest rates and house prices")

################## Join the CH data ###########################################

ch <- full_join(ch.mortgage.rates, ch.house.prices, by = "Date")
# ch <- na.omit(ch)
View(ch)

ch$ch.average.int.rates.linked.br <- as.numeric(ch$ch.average.int.rates.linked.br)

# CORRELATION <------ Measures the relative strength of a linear relationship btw. 2 variables
# calculate correlation ("complete.obs" to make it ignore the NA values)
cor(ch$ch.average.fixed.int.rates, ch$total.house.prices.average, use = "complete.obs", method = "pearson")
#  CORRELATION IS -0.8785073

cor(ch$ch.average.int.rates.linked.br, ch$total.house.prices.average, use = "complete.obs", method = "pearson")
#  CORRELATION IS -0.6237408

cor(ch$ch.average.int.rates.nl.br, ch$total.house.prices.average, use = "complete.obs", method = "pearson")
#  CORRELATION IS -0.7066481
### 1 or -1 would be a perfect correlation - 0 would be no correlation at all


model.fixed.int.rates <- lm(ch$ch.average.fixed.int.rate~ ch$total.house.prices.average, data=ch)
summary(model.fixed.int.rates)
#Coefficients:
#                                   Estimate  Std. Error t value Pr(>|t|)    
#(Intercept)                         0.52824    0.10060   5.251  2.84e-06 ***
#  ch$total.house.prices.average.ch  0.18025    0.01921   9.382  8.97e-13 ***
# Intercept: 0.52824
# Multiple R-squared:  0.6286,	Adjusted R-squared:  0.6215 
# p-value: 8.966e-13 <- reject Null Hypothesis, model is relevant!

model.nl.br <- lm(ch$ch.average.int.rates.nl.br~ ch$total.house.prices.average, data=ch)
summary(model.nl.br)
# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       2.497604   0.033543  74.460   <2e-16 ***
#  ch$total.house.prices.average.ch 0.047627   0.006406   7.435    1e-09 ***
# Intercept: 2.497604
# Multiple R-squared:  0.5153,	Adjusted R-squared:  0.5059 
# p-value: 1.004e-09 <- reject Null Hypothesis, model is relevant!

model.linked.br <- lm(ch$ch.average.int.rates.linked.br~ ch$total.house.prices.average, data=ch)
summary(model.linked.br)
# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                        0.83236    0.02681   31.046  < 2e-16 ***
#  ch$total.house.prices.average.ch  0.03003    0.00512   5.866  3.14e-07 ***
# Intercept: 0.83236
# Multiple R-squared:  0.3982,	Adjusted R-squared:  0.3866 
# p-value: 3.137e-07 <- reject Null Hypothesis, model is relevant!

# two plots combined - interest rates and house prices
ggplot(ch, aes(x = Date))+
  geom_point(aes(y = ch.average.fixed.int.rates*100), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = ch.average.int.rates.nl.br*100), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = ch.average.int.rates.linked.br*100), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = total.house.prices.average), na.rm = TRUE, color = "black", size = 2)+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  scale_y_continuous(sec.axis = sec_axis(trans=~./100, name= "interest rates"))+
  labs(x = "time period", y = "house prices")+
  labs(caption = "fixed interest rates: blue, 
       NL BR: green, 
       linked BR: red
       average house prices: black")+
  ggtitle("interest rates and house prices CH")

# to see CORRELATION
# two plots combined - interest rates and house prices - interest rates maped against house prices
ggplot(ch, aes(x = total.house.prices.average))+
  geom_point(aes(y = ch.average.fixed.int.rates), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = ch.average.int.rates.nl.br), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = ch.average.int.rates.linked.br), na.rm = TRUE, size = 2, color = "red")+
  labs(caption = "fixed interest rates: blue, 
       NL BR: green, 
       linked BR: red")+
  labs(x = "house prices", y = "interest rates")+
  ggtitle("CH CORRELATION - interest rates and house prices")

# two plots combined - interest rates and house prices  WITH A LINEAR MODEL
ggplot(ch, aes(x = total.house.prices.average))+
  geom_point(aes(y = ch.average.fixed.int.rates), na.rm = TRUE, size = 2, color = "blue")+
  geom_smooth(aes(y = ch.average.fixed.int.rates), na.rm = TRUE, size = 1, color = "blue", method = lm)+
  geom_point(aes(y = ch.average.int.rates.nl.br), na.rm = TRUE, size = 2, color = "green")+
  geom_smooth(aes(y = ch.average.int.rates.nl.br), na.rm = TRUE, size = 1, color = "green", method = lm)+
  geom_point(aes(y = ch.average.int.rates.linked.br), na.rm = TRUE, size = 2, color = "red")+
  geom_smooth(aes(y = ch.average.int.rates.linked.br), na.rm = TRUE, size = 1, color = "red", method = lm)+
  labs(caption = "fixed interest rates: blue, 
       NL BR: green, 
       linked BR: red")+
  labs(x = "house prices", y = "interest rates")+
  ggtitle("CH CORRELATION - Linear model - Interest rates & house prices")


############ reorder 2nd data set - US house prices #############################

# create empty table (this is where the numbers are put in the loop)
us.house.prices.second.try = data.frame(Date=character(0),all.homes=numeric(0), single.family=numeric(0), 
                 condos=numeric(0), one.room=numeric(0), five.rooms=numeric(0))
print(us.house.prices.second.try)

## CONVERT TO Quarters
# matrix starts at 1
i <- 1
# 2 because 1 is the date
j <- 1 
# loop through numbers and add them to new table
for(i in 1: nrow(us.house.prices.2nd.df))
{
  for (j in 1: ncol(us.house.prices.2nd.df))
  {
    if(i %% 3 == 0) # one quarter <- 3 months/rows
    {
      if(j == 1) # if column is 1 <- the loop is on a Date
        {        # then add date of the first month of the quarter (2 rows above the current row)
          us.house.prices.second.try[nrow(us.house.prices.second.try) + 1,] <- c(Date=us.house.prices.2nd.df[i-2,1], 
                                                                                 NA, NA, NA, NA, NA)
        }                 
      else
        {       # if the loop is not on a date, then calculate the mean of this row and the last 2 and insert the mean 
                # in the new table (the new table is 3 times smaller than the original one)
          us.house.prices.second.try[i/3,j] = mean(us.house.prices.2nd.df[i-2,j], 
                                            us.house.prices.2nd.df[i-1,j], 
                                            us.house.prices.2nd.df[i,j]) 
        }             
    }
  }
}
#View(us.house.prices.second.try)

# for analysis purposes
# reshape table so that column name is an variable
us.house.prices.second.try.for.boxplot <-melt(us.house.prices.second.try, id = c("Date"))
View(us.house.prices.second.try.for.boxplot)
us.house.prices.second.try.for.boxplot$value <- as.numeric(us.house.prices.second.try.for.boxplot$value)

us.house.prices.second.try.for.boxplot %>%
  ggplot(aes(x = variable, y = value), na.rm = TRUE)+
  geom_boxplot()+
  ggtitle("Boxplot US House Prices, 2nd data set - to find outliers")
# we can see that there are outliers in "privately.owned.appartments.3" (the outliers are too low)
# and "privately.owned.appartments.3" (the outliers are too high)

us.house.prices.second.try.for.boxplot %>%
  ggplot(aes(x = Date, y = value))+
  geom_point(aes(na.rm = TRUE, color = variable))+
  # scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("US House Prices")



###### Join with other us data set ##################################################

#convert to date
date.us.house.prices.second.try <- as.character(us.house.prices.second.try$Date)
us.house.prices.second.try$Date <- as.Date(date.us.house.prices.second.try, formats = "%Y/%m/%d")

us <- full_join(us.house.prices.second.try, us, by = "Date")
View(us)


# transform data to numeric data type
us$single.family <- as.numeric(us$single.family)
us$all.homes <- as.numeric(us$all.homes)
us$condos <- as.numeric(us$condos)
us$one.room <- as.numeric(us$one.room)
us$five.rooms <- as.numeric(us$five.rooms)


# Jans code
#delete old Data from pre 2009.
# usfiltered2009 <- us[!(us$Date < '2009/01/01'),]
# chfiltered2009 <- ch[!(ch$Date < '2009/01/01'),]
#View(chfiltered2009)

# compare new data set to old one in order to see if it is accurate
us %>%
  ggplot(aes(x = Date))+
  geom_point(aes(y = single.family), na.rm = TRUE, colour = "light green")+
  geom_point(aes(y = all.homes), na.rm = TRUE, colour = "orange")+
  geom_point(aes(y = condos), na.rm = TRUE, colour = "dark green")+
  geom_point(aes(y = one.room), na.rm = TRUE, colour = "blue")+
  geom_point(aes(y = five.rooms), na.rm = TRUE, colour = "purple")+
  geom_point(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")+
  geom_smooth(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")+
  labs(caption = "2nd Data Set: 
  single family: light green, all homes: orange, condos: dark green, one room: blue, five rooms: purple
  1st Data Set: Real.Estate.Prices: red
  We can see: the 1st data set is similar to the data from the 2nd data set for five.rooms")+
  labs(x = "house prices", y = "Time Period")+
  ggtitle("US House Prices with new Data")

# seems pretty acurrate -> very similar to five room data from new dataset

# safe unfiltered data, just in case we need it
us.2000 <- us
# filter us data so it only shows values from 2009 till now
us <-  filter(us, us$Date > '2009/01/01') 

us %>%
  ggplot(aes(x = Date))+
  geom_point(aes(y = single.family), na.rm = TRUE, colour = "light green")+
  geom_point(aes(y = all.homes), na.rm = TRUE, colour = "orange")+
  geom_point(aes(y = condos), na.rm = TRUE, colour = "dark green")+
  geom_point(aes(y = one.room), na.rm = TRUE, colour = "blue")+
  geom_point(aes(y = five.rooms), na.rm = TRUE, colour = "purple")+
  geom_point(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")+
  geom_smooth(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")+
  labs(caption = "2nd Data Set: 
  single family: light green, all homes: orange, condos: dark green, one room: blue, five rooms: purple
  1st Data Set: Real.Estate.Prices: red
  We can see: the 1st data set is similar to the data from the 2nd data set for five.rooms")+
  labs(x = "house prices", y = "Time Period")+
  ggtitle("US House Prices with new Data 2009")
  
# two plots combined - interest rates and house prices
ggplot(us, aes(x = Date))+
  geom_point(aes(y = us.fixed.rate.average*60000), na.rm = TRUE, size = 2, color = "black")+
  geom_line(aes(y = us.fixed.rate.average*60000), na.rm = TRUE, size = 0.5, color = "black")+
  geom_point(aes(y = single.family), na.rm = TRUE, colour = "light green")+
  geom_point(aes(y = all.homes), na.rm = TRUE, colour = "orange")+
  geom_point(aes(y = condos), na.rm = TRUE, colour = "dark green")+
  geom_point(aes(y = one.room), na.rm = TRUE, colour = "blue")+
  geom_point(aes(y = five.rooms), na.rm = TRUE, colour = "purple")+
  geom_point(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  scale_y_continuous(sec.axis = sec_axis(trans=~./60000, name= "interest rates"))+
  labs(caption = "single family: light green, all homes: orange, condos: dark green, one room: blue, five rooms: purple, Real.Estate.Prices: red
  fixed interest rate average: black")+
  labs(x = "time period", y = "house prices")+
  ggtitle("Comparing interest rates and house prices US")

# two plots combined - interest rates and house prices
ggplot(us, aes(x = us.fixed.rate.average))+
  geom_point(aes(y = single.family), na.rm = TRUE, size = 2, color = "light green")+
  geom_point(aes(y = all.homes), na.rm = TRUE, size = 2, color = "orange")+
  geom_point(aes(y = condos), na.rm = TRUE, size = 2, color = "dark green")+
  geom_point(aes(y = one.room), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = five.rooms), na.rm = TRUE, size = 2, color = "purple")+
  geom_point(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")+
  labs(x = "house prices", y = "interest rates")+
  labs(caption = "single family: light green, all homes: orange, condos: dark green, one room: blue, five rooms: purple, Real.Estate.Prices: red
  We can see that there is probably not much correlation since the dots are almost scattered vertically")+
  ggtitle("US CORRELATION - Interest rates & house prices")

# two plots combined - interest rates and house prices  WITH A LINEAR MODEL
ggplot(us, aes(x = us.fixed.rate.average))+
  geom_point(aes(y = single.family), na.rm = TRUE, size = 2, color = "light green")+
  geom_smooth(aes(y = single.family), na.rm = TRUE, size = 1, color = "light green", method = lm)+
  geom_point(aes(y = all.homes), na.rm = TRUE, size = 2, color = "orange")+
  geom_smooth(aes(y = all.homes), na.rm = TRUE, size = 1, color = "orange", method = lm)+
  geom_point(aes(y = condos), na.rm = TRUE, size = 2, color = "dark green")+
  geom_smooth(aes(y = condos), na.rm = TRUE, size = 1, color = "dark green", method = lm)+
  geom_point(aes(y = one.room), na.rm = TRUE, size = 2, color = "blue")+
  geom_smooth(aes(y = one.room), na.rm = TRUE, size = 1, color = "blue", method = lm)+
  geom_point(aes(y = five.rooms), na.rm = TRUE, size = 2, color = "purple")+
  geom_smooth(aes(y = five.rooms), na.rm = TRUE, size = 1, color = "purple", method = lm)+
  geom_point(aes(y = Real.Estate.Prices), na.rm = TRUE, size = 2, color = "red")+
  geom_smooth(aes(y = Real.Estate.Prices), na.rm = TRUE, size = 1, color = "red", method = lm)+
  labs(x = "house prices", y = "interest rates")+
  labs(caption = "single family: light green, all homes: orange, condos: dark green, one room: blue, five rooms: purple, Real.Estate.Prices: red
  We can see that there is almost no correlation, the line is almost vertical and the confidence band is really big")+
  ggtitle("US CORRELATION - Linear model - Interest rates & house prices")
# There is almost no correlation, and the confidence band is VERY big :(

  
  
  ################### Merge US and CH data ######################

# before merging
# add suffix us to all the column names US
colnames(us) <- paste(colnames(us), "us", sep = ".")
colnames(us)[1] <- "Date"  # rename first column, which is now "Date.us" to Date, in order to be able to merge
View(us)

# add suffix us to all the column names CH
colnames(ch) <- paste(colnames(ch), "ch", sep = ".")
colnames(ch)[1] <- "Date" # rename first column, which is now "Date.ch" to Date, in order to be able to merge
View(ch)

final.data <- full_join(ch, us, by = "Date")
View(final.data)
summary(final.data)
# colnames(final.data)

# Too many column names, it's a bit confusing - let's remove rows 2-31 (CH interest rates, which are not averaged)
final.data <- final.data[,-2:-31]
# let's remove rows 17 (US Margin.for.5.1.Year.Adj.us) this is not very important for our research
final.data <- final.data[,-18]
View(final.data)
colnames(final.data)

# Overview over columns IN FINAL DATA

# CH INTEREST RATES
# "ch.average.int.rates.nl.br.ch" 
# "ch.average.int.rates.linked.br.ch"                
# "ch.average.fixed.int.rates.ch"

# CH HOUSE PRICES
# "private.apartements.ch"                           
# "single.family.houses.ch"                         
# "apartment.buildings.ch"                           
# "total.house.prices.average.ch" 

# US HOUSE PRICES
# "all.homes.us"        2nd data set                             
# "single.family.us"    lowest                            
# "condos.us"           almost the same as one.room
# "one.room.us"         almost the same as condos                          
# "five.rooms.us"       highest, similar to first data set (Real.Estate.Prices)
# "Real.Estate.Prices.us"   <--- 1st data set

# US INTEREST RATES
# "fixed.Rate.Avg.15.Year.us"     highest   <- but all the interest rates behave very similar               
# "fixed.Rate.Avg.30.Year.us"     middle       for the purpose to find out correlation it is okay to use average                
# "fixed.Rate.Avg.5.Year.us"      lowest                  
# "Origination.Fees.and.Discount.Points.for.15.Y.us"    POINTS / DISCOUNTS: not that important
# "Origination.Fees.and.Discount.Points.for.30.Y.us" 
# "Origination.Fees.and.Discount.Points.for.5.Y.us" 
# "us.fixed.rate.average.us"                         
# "us.fees.and.discount.points.average.us"



### Comparing house prices 
ggplot(final.data, aes(x = Date))+
  geom_point(aes(y = ch.average.int.rates.nl.br.ch), na.rm = TRUE, size = 2, color = "orange")+
  geom_point(aes(y = ch.average.int.rates.linked.br.ch), na.rm = TRUE, size = 2, color = "orange")+
  geom_point(aes(y = ch.average.fixed.int.rates.ch), na.rm = TRUE, size = 2, color = "orange")+
  geom_point(aes(y = one.room.us), na.rm = TRUE, size = 2, color = "orange")+
  geom_point(aes(y = five.rooms.us), na.rm = TRUE, size = 2, color = "orange")+
  geom_point(aes(y = Real.Estate.Prices.us), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = private.apartements.ch*2000), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = single.family.houses.ch*2000), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = apartment.buildings.ch*2000), na.rm = TRUE, size = 2, color = "blue")+
  labs(x = "Date", y = "house price US")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  scale_y_continuous(sec.axis = sec_axis(trans=~./2000, name= "house prices points CH"))+
  labs(caption = "CH house rices: blue, US house prices: orange/red")
# The data in from CH is in points :(  ????


### Comparing interest rates 
ggplot(final.data, aes(x = Date))+
  geom_point(aes(y = ch.average.int.rates.nl.br.ch), na.rm = TRUE, size = 2, color = "blue")+
  geom_line(aes(y = ch.average.int.rates.nl.br.ch), na.rm = TRUE, size = 1, color = "blue")+
  geom_point(aes(y = ch.average.int.rates.linked.br.ch), na.rm = TRUE, size = 2, color = "blue")+
  geom_line(aes(y = ch.average.int.rates.linked.br.ch), na.rm = TRUE, size = 1, color = "blue")+
  geom_point(aes(y = ch.average.fixed.int.rates.ch), na.rm = TRUE, size = 2, color = "blue")+
  geom_line(aes(y = ch.average.fixed.int.rates.ch), na.rm = TRUE, size = 1, color = "blue")+
  geom_point(aes(y = fixed.Rate.Avg.15.Year.us), na.rm = TRUE, size = 2, color = "orange")+
  geom_line(aes(y = fixed.Rate.Avg.15.Year.us), na.rm = TRUE, size = 1, color = "orange")+
  geom_point(aes(y = fixed.Rate.Avg.30.Year.us), na.rm = TRUE, size = 2, color = "orange")+
  geom_line(aes(y = fixed.Rate.Avg.30.Year.us), na.rm = TRUE, size = 1, color = "orange")+
  geom_point(aes(y = fixed.Rate.Avg.5.Year.us), na.rm = TRUE, size = 2, color = "orange")+
  geom_line(aes(y = fixed.Rate.Avg.5.Year.us), na.rm = TRUE, size = 1, color = "orange")+
  labs(x = "Date", y = "interest rate")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  labs(caption = "CH: blue, US: orange")


#totalPlot: All Data from both df - Baustjielle Jan
# need:
  # house prices US; 2 lines.
  # house prices CH
  
# allFiltered <- try(chfiltered2009[, "total.house.prices.average"])
# allFiltered <- chfiltered2009$total.house.prices.average
# colnames(allFiltered)[colnames(allFiltered) == "total.house.prices.average"] ="ch.average.house.price"
# View(allFiltered)
# 
# ch.average.house.price
# allFiltered$us.average.interest.rate <- rowMeans(usfiltered2009[c(2, 3)], na.rm=TRUE)
# View(usfiltered2009)
# colMeans(df, na.rm=TRUE)
# 
# allFiltered %>%
#   ggplot(aes(x = Date))+
#   #  geom_point(aes(y = single.family), na.rm = TRUE, colour = "light green")+
#   geom_point(aes(y = all.homes), na.rm = TRUE, colour = "orange")+
#   #  geom_point(aes(y = condos), na.rm = TRUE, colour = "dark green")+
#   #  geom_point(aes(y = one.room), na.rm = TRUE, colour = "blue")+
#   #  geom_point(aes(y = five.rooms), na.rm = TRUE, colour = "purple")+
#   geom_point(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")
# #  geom_smooth(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")

View(ch)
ch <- ch[,-2:-31]
write.csv(ch, file = "ch.csv")

# 
# ####### PREDICTIVE ANALYTICS #####################################################
# ####### trying to make a linear model ###########################################
# 
# #THE FOLLOWING CODE IS BASED ON THE "LINEAR REGRESSION AND EXTENSIONS SW08" FROM ILIAS 
# 
# # Too many column names, it's a bit confusing - let's remove rows 2-31 (CH interest rates, which are not averaged)
# 
# ch <- na.omit(ch)
# summary(ch)
# skewness(ch$ch.average.int.rates.linked.br.ch) # for the linear model we're going to take the "linked br" variables 
#                                                # since the correlation between "linked br" and house prices was biggest
# # output: -0.114246  <----- very little skewness, we don't need to correct with "mirror x square root"
# 
# skewness(ch$total.house.prices.average.ch)
# # output: -0.6129048  <----- this is is low/medium skewness, so we need to correct with "mirror natural log"
# ch$total.house.prices.average.ch <- sqrt(max(ch$total.house.prices.average.ch+1) - ch$total.house.prices.average.ch)
# skewness(ch$total.house.prices.average.ch)
# # output: 0.005890946  <------ much better :)
# 
# lm.ch <- select(ch, c("Date", "ch.average.int.rates.linked.br.ch", "total.house.prices.average.ch"))
# 
# names(lm.ch) <- c("Date", "interest.rates", "house.prices")
# lm.ch <- as.tibble(lm.ch)
# lm.ch$Date <- lubridate::year(lm.ch$Date)
# lm.ch$Date <- as.numeric(lm.ch$Date)
# lm.ch$interest.rates <- as.numeric(lm.ch$interest.rates)
# lm.ch$house.prices <- as.numeric(lm.ch$house.prices)
# View(lm.ch)
# 
# # #regression
# regression <- lm(house.prices~., data=lm.ch)
# summary(regression)
# 
# drop1(regression, test = "F") 
# 
# regression.red <- lm(house.prices~interest.rates+Date, data=lm.ch)
# summary(regression.red)
# 
# regression.red2 <- lm(house.prices~interest.rates, data=lm.ch)
# summary(regression.red2)
#  
# regression.red3 <- lm(house.prices~Date, data=lm.ch)
# summary(regression.red3)
# 
# anova(regression.red3,regression.red2,regression.red,regression)
# 
# 
# (comparison <- compare_performance(regression.red3,regression.red2,regression.red,regression,rank=TRUE))
# plot(comparison)
# 
# 
# predicted <- predict(regression.red2, lm.ch)
# 
# residuals <- lm.ch$house.prices - predicted  # substract the predicted values from house.prices
# residuals <- as.tibble(predicted) %>% mutate(real = lm.ch$house.prices, n=row_number()) %>% mutate(error= value-real, ratio=error/real)
# 
# 
# ggplot(data=residuals) + geom_point(aes(x=n,y=real),color="blue") +
#    geom_point(aes(x=n,y=value),color="red") +
#    geom_segment(aes(x=n,xend=n,y=real,yend=value), color="yellow", size=1, alpha=0.5)+
#    geom_line(aes(x=n,y=(value+real)/2),color="black")
# 
# residuals <- residuals %>% arrange(real)  %>% mutate(n = row_number())    
# 
# ggplot(data=residuals) + geom_point(aes(x=n,y=real),color="blue") +
#   geom_point(aes(x=n,y=value),color="green", alpha=0.25) +
#   geom_segment(aes(x=n,xend=n,y=real,yend=value, color=factor(sign(value-real),levels=c(-1,1))), size=1, alpha=0.5)+
#   geom_line(aes(x=n,y=(value+real)/2),color="black") +
#   theme(legend.position = "none")
# 
# # #another more compact way of seeing the "errors" --> called residuals
# ggplot() + geom_point(data=residuals,aes(x=n,y=ratio)) + geom_abline(slope=0,color="red", alpha=0.5,size=3)
# 
# 
# 
# # ################## PERFORMACES estimation
# # 
# # #let's see how to compute performances: the most used approach is k-fold CrossValudation (CV):
# # #  randomly divide the dataset into k subset and use k-1 of them for train the model and the remaining one
# # #    for testing the performance: repeat till every combination is covered and then average them
# # #ISSUE: with complex models and datasets not-small, computationally expensive
# # 
# # #Anyway, we will take the "shotcut" to this: we separate 30% of the point as test set and use
# # #  the remaining 70% as train set, to create the model.
# # 
# # #let's use the model from the previous exercise: Attrition_DS
# # 
# # library(readr)
# # 
# # #separating test and training set
# # set.seed(56)
# # index_train <- sample(1:nrow(lm.ch),0.7*nrow(lm.ch))
# # 
# # lm.ch.train <- lm.ch[index_train,]
# # dim(lm.ch.train)
# # lm.ch.test  <- lm.ch[-index_train,]
# # dim(lm.ch.test)
# # 
# # #let's try to have an estimator for the "DailyRate"
# # #
# # regression.2 <- lm(house.prices~., data=lm.ch.train)
# # #equivalent will be: regression.2 <- lm(DailyRate~., data=lm.ch, subset = index_train)
# # summary(regression.2)
# # 
# # drop1(regression.2, test="F")
# # 
# # drop1(update(regression.2, ~ . -Age ), test="F")
# # 
# # drop1(update(regression.2, ~ . -Age -EmployeeNumber), test="F")
# # 
# # drop1(update(regression.2, ~ . -Age -EmployeeNumber -EnvironmentSatisfaction), test="F")
# # 
# # regression.2.1 <- lm(DailyRate~
# #                        Attrition+MaritalStatus+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+
# #                        Education+YearsAtCompany+EmployeeSource+
# #                        HourlyRate+JobRole+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager+
# #                        JobLevel+MonthlyRate+PercentSalaryHike+StockOptionLevel
# #                      , data=lm.ch.train)
# # 
# # summary(regression.2.1)
# # 
# # regression.2.2 <- update(regression.2, ~ . -Age -EmployeeNumber -EnvironmentSatisfaction)
# # 
# # summary(regression.2.2)
# # 
# # 
# # 
# # anova(regression.2.1, regression.2.2, regression.2)
# # 
# # (comparison.2 <- compare_performance(regression.2,regression.2.2,regression.2.1,rank=TRUE))
# # plot(comparison.2)
# # 
# # testcaseID=101
# # test <- lm.ch[testcaseID,]
# # test$DailyRate
# # predict(regression.2.2, test)
# # 
# # 
# # 
