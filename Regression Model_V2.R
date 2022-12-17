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
library(moonBook)

dataset <- read.csv(here("data", "ch.csv"))

View(dataset)
head(dataset)

################### skewness Lea ############################

skewness(dataset$ch.average.int.rates.linked.br.ch)
# output: -0.114246 
skewness(dataset$total.house.prices.average.ch)
# output: -0.2759832  
skewness(dataset$ch.average.int.rates.nl.br.ch)
# output:  -0.4354741
skewness(dataset$ch.average.fixed.int.rates.ch)
# output:  0.8640249

# skewness lies between +1 and -1 so no need to correct


###################################JUST A PLOT#########################################################

ggplot(dataset, aes(x= ch.average.int.rates.linked.br.ch, y= total.house.prices.average.ch)) +
  geom_point() +
  stat_smooth()

######################################CORRELATION######################################################

# Linked BR
cor(dataset$ch.average.int.rates.linked.br.ch, dataset$total.house.prices.average.ch, use = "complete.obs", method = "pearson")
#  -0.7436318  <---- has the least correlation, so the model should be the least accurate

# NL BR
cor(dataset$total.house.prices.average.ch, dataset$ch.average.int.rates.nl.br.ch, use = "complete.obs", method = "pearson")
#  -0.9296063  <---- has the most correlation, so the model should be the most accurate

# Fixed int rates
cor(dataset$total.house.prices.average.ch, dataset$ch.average.fixed.int.rates.ch, use = "complete.obs", method = "pearson")
#  -0.884212

######################################REG MODEL TEST 1######################################################

# linked BR
 
#y = mx + b

model.linked.br <- lm(total.house.prices.average.ch ~ ch.average.int.rates.linked.br.ch, data = dataset)
model.linked.br
print(summary(model.linked.br))
#Coefficients:
#(Intercept)  ch.average.int.rates.linked.br.ch  
#345.1                             -189.6  

#Model = estimated regression line equation: House Prices = -189.6*interest rates + 345.1
#x = if you define interest rate it will give predictive result of average house price
newIntRate <- data.frame(ch.average.int.rates.linked.br.ch = 1.22)
result <- predict(model.linked.br, newIntRate)
result
#1 
#0.9788451 

#Regression Model
ggplot(dataset,aes(y=total.house.prices.average.ch, x=ch.average.int.rates.linked.br.ch))+geom_point()+geom_smooth(method="lm")

#Regression Model with Interaction - hover over graph to see Linear Model Equation, can turn it off by putting "interactive" on False
#Not sure what "se" is.. could be confidence band but not sure.
ggPredict(model.linked.br,se=FALSE,interactive=TRUE)



###################################### copied Dylan's code to make more models ######################################################

# NL BR

#y = mx + b

model.nl.br <- lm(total.house.prices.average.ch ~ ch.average.int.rates.nl.br.ch, data = dataset)
model.nl.br
print(summary(model.nl.br))
#Coefficients:
# (Intercept)  ch.average.int.rates.nl.br.ch  
# 729.2                         -207.6 

#Model = estimated regression line equation: House Prices = -189.6*interest rates + 345.1
#x = if you define interest rate it will give predictive result of average house price
newIntRate <- data.frame(ch.average.int.rates.nl.br.ch = 1.22)
result <- predict(model.nl.br, newIntRate)
result
# 1 
# 475.8969 

#Regression Model
ggplot(dataset,aes(y=total.house.prices.average.ch, x=ch.average.int.rates.nl.br.ch))+geom_point()+geom_smooth(method="lm")

#Regression Model with Interaction - hover over graph to see Linear Model Equation, can turn it off by putting "interactive" on False
#Not sure what "se" is.. could be confidence band but not sure.
ggPredict(model.nl.br,se=FALSE,interactive=TRUE)







# fixed int rates

model.fixed.int.rates <- lm(total.house.prices.average.ch ~ ch.average.fixed.int.rates.ch, data = dataset)
model.fixed.int.rates
print(summary(model.fixed.int.rates))
#Coefficients:
# (Intercept)  ch.average.fixed.int.rates.ch  
# 226.23                         -47.19  

#Model = estimated regression line equation: House Prices = -189.6*interest rates + 345.1
#x = if you define interest rate it will give predictive result of average house price
newIntRate <- data.frame(ch.average.fixed.int.rates.ch = 1.22)
result <- predict(model.fixed.int.rates, newIntRate)
result
# 1 
# 168.6609 

#Regression Model
ggplot(dataset,aes(y=total.house.prices.average.ch, x=ch.average.fixed.int.rates.ch))+geom_point()+geom_smooth(method="lm")

#Regression Model with Interaction - hover over graph to see Linear Model Equation, can turn it off by putting "interactive" on False
#Not sure what "se" is.. could be confidence band but not sure.
ggPredict(model.fixed.int.rates,se=FALSE,interactive=TRUE)




# linked BR, nl br and fixed int rates

#y = mx + b

model <- lm(total.house.prices.average.ch ~ ch.average.int.rates.linked.br.ch + ch.average.int.rates.nl.br.ch + ch.average.fixed.int.rates.ch, data = dataset)
model
print(summary(model))
#Coefficients:
# (Intercept)  ch.average.int.rates.linked.br.ch      ch.average.int.rates.nl.br.ch  ch.average.fixed.int.rates.ch  
# 582.98                              22.85                            -151.25                -21.80 


#Model = estimated regression line equation: House Prices = -189.6*interest rates + 345.1
#x = if you define interest rate it will give predictive result of average house price
newIntRate <- data.frame(ch.average.int.rates.linked.br.ch = 1.22, ch.average.fixed.int.rates.ch = 0.52, 
                         ch.average.int.rates.nl.br.ch = 0.12)
result <- predict(model, newIntRate)
result
# 1 
# 581.3789 


# don't know how to do this with multiple predictors

# #Regression Model
# ggplot(dataset,aes(y=total.house.prices.average.ch, x=ch.average.int.rates.linked.br.ch))+geom_point()+geom_smooth(method="lm")+
# 
# #Regression Model with Interaction - hover over graph to see Linear Model Equation, can turn it off by putting "interactive" on False
# #Not sure what "se" is.. could be confidence band but not sure.
# ggPredict(model.linked.br,se=FALSE,interactive=TRUE)



######################################## Lea is doing some tests ####################################################


# we separate 30% of the point as test set and use
# the remaining 70% as train set, to create the model.

library(readr)

# separating test and training set
set.seed(56)

# create index where data is splitted
index_train <- sample(1:nrow(dataset),0.7*nrow(dataset))  

# create a new dataset with the training data
dataset.train <- dataset[index_train,]    # <---- all the data after the splitting index
dim(dataset.train)  # <----- Retrieve or set the dimension of an object

# create dataset with the test data
dataset.test  <- dataset[-index_train,]    # <---- all the data before the splitting index
dim(dataset.test)



################ Test - Linked BR Model ################################################################

# let's try to have an estimate house prices with the linked br int rates (just like before)
regression.linked.br <- lm(total.house.prices.average.ch~ ch.average.int.rates.linked.br.ch, data=dataset.train)
summary(regression.linked.br)

# predict the test data
result.linked.br <- predict(regression.linked.br, dataset.test)

# put test data and prediction into a table to compare if prediction was accurate
real.house.prices <- dataset.test$total.house.prices.average.ch
compare.result.linked.br.to.real.house.prices = data.frame(index=1:nrow(dataset.test), 
                                                 real.house.price=real.house.prices, 
                                                 result.linked.br=result.linked.br)
View(compare.result.linked.br.to.real.house.prices)

ggplot(compare.result.linked.br.to.real.house.prices, aes(x = index))+
  geom_point(aes(y = real.house.prices), size = 2, color = "blue")+
  geom_line(aes(y = real.house.prices), size = 1, color = "blue")+
  geom_point(aes(y = result.linked.br), size = 2, color = "orange")+
  geom_segment(aes(x=index,xend=index,y=real.house.prices,yend=result.linked.br, 
                   color=factor(sign(real.house.prices-result.linked.br))))+
  theme(legend.position = "none")+
  labs(x = "nr", y = "house prices")+
  ggtitle("real house prices vs predicted result - Linked BR Model")





################ Test - NL BR Model ################################################################

# let's try to have an estimate house prices with the nl br int rates (just like before)
regression.nl.br <- lm(total.house.prices.average.ch~ ch.average.int.rates.nl.br.ch, data=dataset.train)
summary(regression.nl.br)

# predict the test data
result.nl.br <- predict(regression.nl.br, dataset.test)

# put test data and prediction into a table to compare if prediction was accurate
compare.result.nl.br.to.real.house.prices = data.frame(index=1:nrow(dataset.test), 
                                                 real.house.price=real.house.prices, 
                                                 result.nl.br=result.nl.br)
View(compare.result.nl.br.to.real.house.prices)

ggplot(compare.result.nl.br.to.real.house.prices, aes(x = index))+
  geom_point(aes(y = real.house.prices), size = 2, color = "blue")+
  geom_line(aes(y = real.house.prices), size = 1, color = "blue")+
  geom_point(aes(y = result.nl.br), size = 2, color = "red")+
  geom_segment(aes(x=index,xend=index,y=real.house.prices,yend=result.nl.br, color=factor(sign(real.house.prices-result.nl.br))))+
  labs(x = "nr", y = "house prices")+
  theme(legend.position = "none")+
  ggtitle("real house prices vs predicted result - NL BR Model")




################ Test - Fixed Int Rates Model ################################################################
  

# let's try to have an estimate house prices with the fixed int rates (just like before)
regression.fixed.int.rates <- lm(total.house.prices.average.ch~ ch.average.fixed.int.rates.ch, data=dataset.train)
summary(regression.fixed.int.rates)

# predict the test data
result.fixed.int.rates <- predict(regression.fixed.int.rates, dataset.test)

# put test data and prediction into a table to compare if prediction was accurate
compare.result.fixed.int.rates.to.real.house.prices = data.frame(index=1:nrow(dataset.test), 
                                                           real.house.price=real.house.prices, 
                                                           result.fixed.int.rates=result.fixed.int.rates)
View(compare.result.fixed.int.rates.to.real.house.prices)

ggplot(compare.result.fixed.int.rates.to.real.house.prices, aes(x = index))+
  geom_point(aes(y = real.house.prices), size = 2, color = "blue")+
  geom_line(aes(y = real.house.prices), size = 1, color = "blue")+
  geom_point(aes(y = result.fixed.int.rates), size = 2, color = "dark green")+
  geom_segment(aes(x=index,xend=index,y=real.house.prices,yend=result.fixed.int.rates, color=factor(sign(real.house.prices-result.fixed.int.rates))))+
  labs(x = "nr", y = "house prices")+
  theme(legend.position = "none")+
  ggtitle("real house prices vs predicted result - Fixed Intrest Rates Model")


################ Test - Model with all int rates attributes ################################################################


# let's try to have an estimate house prices with the fixed int rates (just like before)
regression.model <- lm(total.house.prices.average.ch ~ ch.average.int.rates.linked.br.ch 
                       + ch.average.int.rates.nl.br.ch 
                       + ch.average.fixed.int.rates.ch, data=dataset.train)
summary(regression.model)

# predict the test data
result.model <- predict(regression.model, dataset.test)

# put test data and prediction into a table to compare if prediction was accurate
compare.result.model.to.real.house.prices = data.frame(index=1:nrow(dataset.test), 
                                                                 real.house.price=real.house.prices, 
                                                                 result.model=result.model)
View(compare.result.model.to.real.house.prices)

ggplot(compare.result.model.to.real.house.prices, aes(x = index))+
  geom_point(aes(y = real.house.prices), size = 2, color = "blue")+
  geom_line(aes(y = real.house.prices), size = 1, color = "blue")+
  geom_point(aes(y = result.nl.br), size = 2, color = "purple")+
  geom_segment(aes(x=index,xend=index,y=real.house.prices,yend=result.nl.br, color=factor(sign(real.house.prices-result.nl.br))))+
  labs(x = "nr", y = "house prices")+
  theme(legend.position = "none")+
  ggtitle("real house prices vs predicted result - All int rates rates")



####### Show different results in order to compare different models

results <- cbind(compare.result.model.to.real.house.prices, 
                 linked.br=  compare.result.linked.br.to.real.house.prices$result.linked.br,
                 nl.br= compare.result.nl.br.to.real.house.prices$result.nl.br,
                 fixed.int.rates= compare.result.fixed.int.rates.to.real.house.prices$result.fixed.int.rates)
View(results)

ggplot(results, aes(x = index))+
  geom_point(aes(y = real.house.prices), size = 2, color = "blue")+
  geom_line(aes(y = real.house.prices), size = 1, color = "blue")+
  geom_point(aes(y = nl.br), size = 3, color = "red", alpha =0.5)+
  geom_point(aes(y = linked.br), size = 3, color = "orange", alpha =0.5)+
  geom_point(aes(y = fixed.int.rates), size = 3, color = "dark green", alpha =0.5)+
  geom_point(aes(y = result.model), size = 3, color = "purple", alpha =0.5)+
  labs(x = "nr", y = "house prices")+
  theme(legend.position = "none")+
  labs(caption = "nl br: red, 
       linked br: orange,
       fixed int rates: green,
       all int rates: purple")+
  ggtitle("Plot with different results from different models")

