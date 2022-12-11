library(tidyverse)
library(scales)
library(ggplot2)
library(reshape)
library(reshape2)
library(here) #<- for Location of the files!
#install.packages("reshape2")

#### IMPORT DATA

# Specify working directory7
#install.packages("here") #<- install once for location of the files!


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
us.house.prices.2nd.df <- read.csv(here("data", "zillowPriceIndex.csv"))
#View(ch.house.prices)
#View(ch.mortgage.rates)
#View(us.mortgage.rates)
#View(us.house.prices)
#View(us.house.prices.2nd.df)

################## CH House Prices #######################################################################

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

# let's correct this and filter these out in the original dataset "ch.house.prices"
summary(ch.house.prices$Privately.owned.apartments.3)
#    Min.  1st Qu.  Median    Mean   3rd Qu.    Max. 
#   142.3   169.5   178.1    173.4   181.2     185.1 

summary(ch.house.prices$Privately.owned.apartments.4)
#   Min.   1st Qu.  Median    Mean   3rd Qu.    Max. 
#   140.9   174.8   189.1    186.0   197.9     234.1 
# this means all values above 234.1 should be removed

# should we remove the outliers? if so how???

# add new column for averages
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

# create total average 
house.prices.average <- c(rowMeans(ch.house.prices[,14:16], na.rm = TRUE))
ch.house.prices <- cbind(ch.house.prices, total.house.prices.average = house.prices.average)

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
# -> these are the 3 lower green dots -> VARIABLE INTEREST RATES - LINKED BR

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
# VARIABLE INTEREST RATES - LINKED BR
average.int.rates.linked.br <- rowMeans(select(ch.mortgage.rates,c("Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.500k...1.Mio.",  
                                                            "Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.1.Mio...5.Mio.", 
                                                            "Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.5.Mio...15.Mio.",
                                                            "Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.50k...100k.",
                                                            "Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.100k...500k.")),
                                                             na.rm = TRUE)
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
  
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH Sum of Loan Mortgages")





          

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

us <- full_join(us.mortgage.rates, us.house.prices, by = "Date")
View(us)

# CORRELATION <------ Measures the relative strength of a linear relationship btw. 2 variables
# calculate correlation ("complete.obs" to make it ignore the NA values)
us.correlation <- cor(us$us.fixed.rate.average, us$Real.Estate.Prices, use = "complete.obs", method = "pearson")
view(us.correlation)
#   CORRELATION IS -0.2065972
### 1 or -1 would be a perfect correlation - 0 would be no correlation at all


#### SOMETHING IS WRONG, correlation is  very little, can this be ? :(

### We are gonna add another data set just to make sure we have accurate and treatable data




################## Join the CH data ###########################################

ch <- full_join(ch.mortgage.rates, ch.house.prices, by = "Date")
View(ch)

# CORRELATION <------ Measures the relative strength of a linear relationship btw. 2 variables
# calculate correlation ("complete.obs" to make it ignore the NA values)
ch.correlation.fixed.int.rates <- cor(ch$ch.average.fixed.int.rates, ch$total.house.prices.average, use = "complete.obs", method = "pearson")
#  CORRELATION IS -0.8785073
ch.correlation.linked.br <- cor(ch$ch.average.int.rates.linked.br, ch$total.house.prices.average, use = "complete.obs", method = "pearson")
#  CORRELATION IS -0.6237408
ch.correlation.nl.br <- cor(ch$ch.average.int.rates.nl.br, ch$total.house.prices.average, use = "complete.obs", method = "pearson")
#  CORRELATION IS -0.7066481
### 1 or -1 would be a perfect correlation - 0 would be no correlation at all

# two plots combined
ggplot(ch, aes(x = Date))+
  geom_point(aes(y = ch.average.fixed.int.rates*100), na.rm = TRUE, size = 2, color = "blue")+
  geom_point(aes(y = ch.average.int.rates.nl.br*100), na.rm = TRUE, size = 2, color = "green")+
  geom_point(aes(y = ch.average.int.rates.linked.br*100), na.rm = TRUE, size = 2, color = "red")+
  geom_point(aes(y = total.house.prices.average), na.rm = TRUE, color = "black", size = 2)+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
scale_y_continuous(sec.axis = sec_axis(trans=~./100, name= "interest rates"))+
  labs(x = "time period", y = "house prices")+
  ggtitle("interest rates and house prices CH")


############ reorder 2nd data set - US house prices #############################

# create empty table (this is where the numbers are put in the loop)
us.house.prices.second.try = data.frame(Date=character(0),all.homes=numeric(0), single.family=numeric(0), 
                 condos=numeric(0), one.room=numeric(0), five.rooms=numeric(0))
print(us.house.prices.second.try)

## CONVERT TO QUARTILES -> Quarters?
# matrix starts at 1
i <- 1
# 2 because 1 is the date
j <- 1 
# loop through NL BR numbers and add them to new table
for(i in 1: nrow(us.house.prices.2nd.df))
{
  for (j in 1: ncol(us.house.prices.2nd.df))
  {
    if(i %% 3 == 0)
    {
      if(j == 1)
        {
          us.house.prices.second.try[nrow(us.house.prices.second.try) + 1,] <- c(Date=us.house.prices.2nd.df[i-2,1], 
                                                                                 NA, NA, NA, NA, NA)
        }                 
      else
        {
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

###### Join with other us data set ###################

#convert to date
date.us.house.prices.second.try <- as.character(us.house.prices.second.try$Date)
us.house.prices.second.try$Date <- as.Date(date.us.house.prices.second.try, formats = "%Y/%m/%d")

us <- full_join(us.house.prices.second.try, us, by = "Date")
View(us)

# transform data to numeric data type
us$us.single.family <- as.numeric(us$single.family)
us$us.all.homes <- as.numeric(us$all.homes)
us$us.condos <- as.numeric(us$condos)
us$us.one.room <- as.numeric(us$one.room)
us$us.five.rooms <- as.numeric(us$five.rooms)

#delete old Data from pre 2009.
usfiltered2009 <- us[!(us$Date < '2009/01/01'),]
chfiltered2009 <- ch[!(ch$Date < '2009/01/01'),]
#View(chfiltered2009)

# compare new data set to old one in order to see if it is accurate
usfiltered2009 %>%
  ggplot(aes(x = Date))+
  geom_point(aes(y = us.single.family), na.rm = TRUE, colour = "light green")+
  geom_point(aes(y = us.all.homes), na.rm = TRUE, colour = "orange")+
  geom_point(aes(y = us.condos), na.rm = TRUE, colour = "dark green")+
  geom_point(aes(y = us.one.room), na.rm = TRUE, colour = "blue")+
  geom_point(aes(y = us.five.rooms), na.rm = TRUE, colour = "purple")+
  geom_point(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")+
  geom_smooth(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")
  
# seems pretty acurrate -> very similar to five room data from new dataset

  
  
  
  
#totalPlot: All Data from both df - Baustjielle Jan
# need:
  # house prices US; 2 lines.
  # house prices CH
  
allFiltered <- try(chfiltered2009[, "total.house.prices.average"])
allFiltered <- chfiltered2009$total.house.prices.average
colnames(allFiltered)[colnames(allFiltered) == "total.house.prices.average"] ="ch.average.house.price"
View(allFiltered)

ch.average.house.price
allFiltered$us.average.interest.rate <- rowMeans(usfiltered2009[c(2, 3)], na.rm=TRUE)
View(usfiltered2009)
colMeans(df, na.rm=TRUE)

allFiltered %>%
  ggplot(aes(x = Date))+
  #  geom_point(aes(y = single.family), na.rm = TRUE, colour = "light green")+
  geom_point(aes(y = all.homes), na.rm = TRUE, colour = "orange")+
  #  geom_point(aes(y = condos), na.rm = TRUE, colour = "dark green")+
  #  geom_point(aes(y = one.room), na.rm = TRUE, colour = "blue")+
  #  geom_point(aes(y = five.rooms), na.rm = TRUE, colour = "purple")+
  geom_point(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")
#  geom_smooth(aes(y = Real.Estate.Prices), na.rm = TRUE, color = "red")
