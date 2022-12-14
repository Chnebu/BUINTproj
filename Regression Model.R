#install.packages("devtools")
#install.packages("ggiraphExtra")
#install.packages("ggiraph")
#install.packages("moonBook")

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
library(ggiraph)
library(ggiraphExtra)
library(plyr)
library(moonBook)

dataset <- read.csv(here("data", "ch.csv"))

head(dataset)

###################################JUST A PLOT#########################################################

ggplot(dataset, aes(x= ch.average.int.rates.linked.br.ch, y= total.house.prices.average.ch)) +
  geom_point() +
  stat_smooth()

######################################CORRELATION######################################################

cor(dataset$ch.average.int.rates.linked.br.ch, dataset$total.house.prices.average.ch, use = "complete.obs", method = "pearson")
#[1] -0.7436318

######################################REG MODEL TEST 1######################################################

#y = mx + b

model <- lm(total.house.prices.average.ch ~ ch.average.int.rates.linked.br.ch, data = dataset)
model
print(summary(model))
#Coefficients:
#(Intercept)  ch.average.int.rates.linked.br.ch  
#345.1                             -189.6  

#Model = estimated regression line equation: House Prices = -189.6*interest rates + 345.1
#x = if you define interest rate it will give predictive result of average house price
newIntRate <- data.frame(ch.average.int.rates.linked.br.ch = 1.22)
result <- predict(model, newIntRate)
result
#1 
#0.9788451 

#Regression Model
ggplot(dataset,aes(y=total.house.prices.average.ch, x=ch.average.int.rates.linked.br.ch))+geom_point()+geom_smooth(method="lm")

#Regression Model with Interaction - hover over graph to see Linear Model Equation, can turn it off by putting "interactive" on False
#Not sure what "se" is.. could be confidence band but not sure.
ggPredict(model,se=FALSE,interactive=TRUE)

######################################REG MODEL TEST 2######################################################

#y = mx + b

model1 <- lm(ch.average.int.rates.linked.br.ch ~  total.house.prices.average.ch, data = dataset)
model1
print(summary(model1))
#Coefficients:
#(Intercept)  total.house.prices.average.ch  
#1.445623                      -0.002917  

#Model1 = estimated regression line equation: Interest Rates = 0*house prices + 1.45
#x = if you define house price it will give predictive result of average interest rate
newHousePrice <- data.frame(total.house.prices.average.ch = 160)
result1 <- predict(model1, newHousePrice)
result1
#1 
#0.9788451 

#Regression Model
ggplot(dataset,aes(y=ch.average.int.rates.linked.br.ch, x=total.house.prices.average.ch))+geom_point()+geom_smooth(method="lm")

#Regression Model with Interaction - hover over graph to see Linear Model Equation, can turn it off by putting "interactive" on FALSE
#Not sure what "se" is.. could be confidence band but not sure.
ggPredict(model1,se=FALSE,interactive=TRUE)


##################################WEIGHT & HEIGHT TEST 1 (FOR TRAINGING PURPOSE)#################################

#test1
peoples <- data.frame(height=c(155,162,170,188,209), weight=c(54, 72, 88, 89, 95))

##test1
model2 <- lm(weight ~ height, data = peoples)
model2

#test1
print(summary(model2))
newHeight <- data.frame(height = 180)
result2 <- predict(model2, newHeight)
result2

#test1
ggplot(peoples,aes(y=weight,x=height))+geom_point()+geom_smooth(method="lm")
ggPredict(model2,se=FALSE,interactive=TRUE)

#test2
height <- c(155, 162, 170, 188, 209)
weight <- c(54, 72, 88, 89, 95)

#test2
model3 <- lm(weight ~ height)
model3

#test2
print(summary(model3))
newHeight <- data.frame(height = 180) 
result3 <- predict(model3, newHeight)
result3

#test2
plot(height, weight, col = "blue", main = "Height & Weight Regression", abline(lm(weight ~ height)), 
     cex = 1.3, pch = 16, xlab = "Height in cm", ylab = "Weight in kg")


########################################SKEWNESS (LEA)####################################################

#ch <- dataset[,-2:-31]
#ch <- na.omit(dataset)
#skewness(ch$ch.average.int.rates.linked.br.ch) # for the linear model we're going to take the "linked br" variables 
#                                                # since the correlation between "linked br" and house prices was biggest
# # output: -0.114246  <----- very little skewness, we don't need to correct with "mirror x square root"
# 
# skewness(ch$total.house.prices.average.ch)
# # output: -0.6129048  <----- this is is low/medium skewness, so we need to correct with "mirror natural log"
# ch$total.house.prices.average.ch <- sqrt(max(ch$total.house.prices.average.ch+1) - ch$total.house.prices.average.ch)
# skewness(ch$total.house.prices.average.ch)
# # output: 0.005890946  <------ much better :)

