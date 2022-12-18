library(tidyverse)
library(scales)
library(ggplot2)
library(tree)
library(here)
#install.packages("tree")

ch.mortgage.rates <- read.csv(here("data", "ch-mortgage-rates.csv"))
View(ch.mortgage.rates)

#convert to date
date.ch.mortgage.rates <- as.character(ch.mortgage.rates$Date)
ch.mortgage.rates$Date <- as.Date(date.ch.mortgage.rates, formats = "%Y/%m/%d")

names(ch.mortgage.rates)

#table without the Sums
average.mortgage <- select(ch.mortgage.rates, Date, Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.50k...100k.,  
                            Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.100k...500k., 
                            Average.of..M..Mortgages.Var.Int.Rates...NL.BR..CHF.500k...1.Mio.,
                            Average.of..M..Mortgages.Var.Int.Rates...NL.BR..CHF.1.Mio...5.Mio.,
                            Average.of.M..Mortgages.Var.Int.Rates...NL.BR..CHF.5.Mio...15.Mio.,
                            Average.of.M..Mortgages.with.fixed.interest.rates..CHF.50k...100k.,  
                            Average.of.M..Mortgages.with.fixed.interest.rates..CHF.100k...500k., 
                            Average.of.M..Mortgages.with.fixed.interest.rates..CHF.500k...1.Mio.,
                            Average.of.M..Mortgages.with.fixed.interest.rates..CHF.1.Mio...5.Mio.,
                            Average.of.M..Mortgages.with.fixed.interest.rates..CHF.5.Mio...15.Mio.,
                            Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.500k...1.Mio.,  
                            Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.1.Mio...5.Mio.,
                            Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.5.Mio...15.Mio.,
                            Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.50k...100k.,
                            Average.of.M..Mortgages.Var.Int.Rates...Linked.BR..CHF.100k...500k.)
View(average.mortgage)

# create empty table (this is where the numbers are put in the loop)
data <- data.frame(Date = character(0), interest.rate = numeric(0), category = character(0), 
                 amount = character(0))

# preparations for For-Loop
counter.categories <- 1     #this counter should increase every 5 columns
counter.amount <- 1   # this counter should increase in every column, should be reset after 5.
counter <- 1 # counter for the date, index of row in new table

categories <- c("NL BR", "Fixed", "Linked BR")    # these are the different categories in the columns (5:5:5)
different.amounts <- c("50k - 100k", "100k-500k", "500k-1 Mio", "1 Mio-5 Mio", "5 Mio-15 Mio") # different amount within each category

# loop through NL BR numbers and add them to new table
for(i in 1: nrow(average.mortgage))
{
  for (j in 2: ncol(average.mortgage))  #2 because 1 is the date
  {   # for every value we create a new row in the new table "data"
    data[nrow(data) + 1,] <- c("NA", average.mortgage[i,j], categories[counter.categories], different.amounts[counter.amount])
    data[counter, 1] <- as.character(average.mortgage[i,1],  formats = "%Y/%m/%d")
    counter <- counter + 1  # increase counter to get to the next tow in the new table (data)
    if(counter.amount == 5) {
      counter.amount <- 1   # after 5 columns (when a new category begins),we start again with "50k-100k"
      if(counter.categories != 3) {
        counter.categories <- counter.categories + 1    # after category count has reached "Linked BR" reset to
      }                                                 # 1, which is "NL BR"
      else {
        counter.categories <- 1  # the category only changes after 5 columns
      }
    }
    else {
      counter.amount <- counter.amount + 1    # the amount increases every column (5 times) until the 
    }                                         # category changes 
  }
}
#convert to date
date.average.mortgage <- as.character(average.mortgage$Date)
average.mortgage$Date <- as.Date(date.average.mortgage, formats = "%Y/%m/%d")
View(data)

data <- drop_na(data)


####### Decision Tree #############################################################

data$Date <- lubridate::year(data$Date)
data$Date <- as.numeric(data$Date)
data$interest.rate <- as.numeric(data$interest.rate)
data$category <- as.factor(data$category)
data$amount <- as.factor(data$amount)

data$interest.rate = as.numeric(data$interest.rate)

tree.regression.data <- tree(interest.rate ~ Date + category + amount, 
                           data = data, subset = train)
summary(tree.regression.data)
# output: Number of terminal nodes:  5
# Residual mean deviance:  0.02711 = 14.34 / 529 

plot(tree.regression.data)
text(tree.regression.data, pretty = 0, cex = 0.75)



########### Boxplots ################################################

# boxplot sorted by category
data %>%
ggplot(aes(x = category, y = interest.rate))+
  geom_boxplot()+
  geom_point(aes(colour = amount, size = Date, alpha = 0.2))

# boxplot sorted by category -> facet wrap amount
data %>%
  ggplot(aes(x = category, y = interest.rate))+
  geom_boxplot()+
  geom_point(aes(colour = amount, size = Date, alpha = 0.2))+
  facet_wrap(~amount)

# boxplot sorted by amount -> facet wrap category
data %>%
  ggplot(aes(x = amount, y = interest.rate))+
  geom_boxplot()+
  geom_point(aes(colour = category, size = Date, alpha = 0.2))+
  theme(axis.text.x = element_text(angle = 60,vjust = 0.5,hjust = 1))+
  facet_wrap(~category)

###### other plots #########################

# plot
data %>%
  ggplot(aes(x = Date, y =interest.rate))+
  geom_point(aes(size = amount, color = category, alpha = 0.2))+
 # scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH mortgage rates")

data %>%
  ggplot(aes(x = Date, y =interest.rate))+
  geom_point(aes(size = amount, color = category, alpha = 0.2))+
  geom_smooth(method = lm)+
  # scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH mortgage rates")+
  facet_wrap(~category)
             
#linear model for FIXED INTEREST RATES
data %>% filter(category == "Fixed") %>%
  ggplot(aes(x = Date, y =interest.rate))+
  geom_smooth(method = lm)+
  geom_point(aes(size = 1, color = amount, alpha = 0.2))+
  # scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH mortgage rates - Fixed interest rates")

# after 2020 it rises a lot - let's create a smooth model, to see the st. error in the confindece interval
#linear model for FIXED INTEREST RATES
data %>% filter(category == "Fixed") %>%
  ggplot(aes(x = Date, y =interest.rate))+
  geom_smooth()+
  geom_point(aes(size = 1, color = amount, alpha = 0.2))+
  # scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH mortgage rates - Fixed interest rates")

#linear model for LINKED BR
data %>% filter(category == "Linked BR") %>%
  ggplot(aes(x = Date, y =interest.rate))+
  geom_smooth(method = lm)+
  geom_point(aes(size = 1, color = amount, alpha = 0.2))+
  # scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH mortgage rates - Linked BR")

#linear model for NL BR
data %>% filter(category == "NL BR") %>%
  ggplot(aes(x = Date, y =interest.rate))+
  geom_smooth(method = lm)+
  geom_point(aes(size = 1, color = amount, alpha = 0.2))+
  # scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  ggtitle("CH mortgage rates - NL BR")
