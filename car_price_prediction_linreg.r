#################################################################
# Econometrics and Statistical Models project - Linear regression
#################################################################

#-----------------------------------
# Import data
#-----------------------------------

library(corrplot)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(boot)

data <- read.csv('CarPrice_Assignment.csv', stringsAsFactors = TRUE)

# Exploratory analysis and data structure
str(data)
dim(data)
View(data)
summary(data)

# Check for missing values
sum(is.na(data))
# No missing values

# Output variable Y = price

#-----------------------------------
# Cleaning the data
#-----------------------------------

# Removing the ID column
data$car_ID<-NULL

# We store the CarName attribute as a character
data$CarName<-as.character(data$CarName)
str(data)

# Convert some variable types to Numeric from Integer
data$symboling = as.character(data$symboling)
data$enginesize = as.numeric(data$enginesize)
data$horsepower = as.numeric(data$horsepower)
data$peakrpm = as.numeric(data$peakrpm)
data$citympg = as.numeric(data$citympg)
data$highwaympg = as.numeric(data$highwaympg)
data$curbweight = as.numeric(data$curbweight)

str(data)

#Exclude all non-numeric columns 
num.cols <- sapply(data, is.numeric)
data2 <- data[,num.cols]

#Corrplot
corrplot(cor(data2), order = "hclust", method = "number")

# price has a high positive correlation with engine size, curb weight and horsepower
# This give us an idea of which variables will have a significant relationship with price

#-----------------------------------
# #Visualizing the Data
#-----------------------------------

hist(data$price)
boxplot(data$price)
# Heavy tail on the right-hand side of the distribution
#Most of the cars in our dataset cost less than 10,000

data$logprice <- log(data$price)

boxplot(data$logprice)
hist(data$logprice)
# The log variable seems closer to a normal distribution.


par(mfrow = c(1, 4))
ggplot(data, aes(x=wheelbase))+
  geom_boxplot() 
ggplot(data, aes(x=carlength))+
  geom_boxplot() 
ggplot(data, aes(x=carwidth))+
  geom_boxplot()
ggplot(data, aes(x=carheight))+
  geom_boxplot() 
ggplot(data, aes(x=boreratio))+
  geom_boxplot() 
ggplot(data, aes(x=curbweight))+
  geom_boxplot()
ggplot(data, aes(x=enginesize))+
  geom_boxplot() 
ggplot(data, aes(x=stroke))+
  geom_boxplot() 
ggplot(data, aes(x=horsepower))+
  geom_boxplot() 


ggplot(data, aes(x=horsepower, y =price))+
  geom_point()+geom_smooth( colour="red") 
ggplot(data, aes(x=enginesize, y =price))+
  geom_point()+geom_smooth( colour="red") 
ggplot(data, aes(x=curbweight, y =price))+
  geom_point()+geom_smooth( colour="red") 

par(mfrow = c(1, 1))

###########################
# Simple linear regression
###########################

#-------------------------------
# Link between enginesize and price
#--------------------------------

# Graph
attach(data)  #to avoid reference to the dataset
plot(logprice~enginesize)

# Model estimation
lm.fit1 <- lm(logprice~enginesize) # logprice = b_0 + b_1 * enginesize
summary(lm.fit1)

# Interpretation:
# the pvalue (Pr(>|t|) = 2.2e-16) is very small (less than 5%)
# So we conclude that the coefficient beta_1 is significantly different from 0
# and we can interpret its value.
# beta_1 = 0.01 <0 so an increase by 1 unit of enginesize impacts negatively logprice 
# by an increase of 0.01

# R² = adjusted R² = 69%
# This model explains 69% of the log price variations

names(lm.fit1) # all the results provided by the function lm

plot(enginesize,logprice, pch="+")
abline(lm.fit1, col="red")

#--------------------
# Residuals analysis
#--------------------

# Normality of residuals
# QQplot
qqnorm(residuals(lm.fit1))
qqline(residuals(lm.fit1))
# most of the points fit on the line which indicates that the residuals distribution is not too far from a Normal distribution

#Shapiro-Wilks test
#Ho: the distribution is Normal
#H1: the distribution is not Normal

shapiro.test(residuals(lm.fit1))
# p-value = 4.585e-04 <<<5% so we reject Ho !
# The distribution cannot be considered as Normal
# There must be potential outliers....
hist(residuals(lm.fit1))

# Homoscedasticity    
par(mfrow=c(1,2))
plot(predict(lm.fit1),residuals(lm.fit1))
plot(predict(lm.fit1),rstudent(lm.fit1))
abline(h=0)

#----------------------------------
# Evaluate the quality of the model
#----------------------------------

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Fit on the training sample using the R² and adjusted R²
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# the closer R² to 1, the better 

#''''''''''''''''''''''''''''''''''''''
# Predictive power with Validation set 
#'''''''''''''''''''''''''''''''''''''
# Sample the dataset
set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]
dim(train)
dim(test)

# Estimate the linear fit on the training set
lm.fit0.8 <- lm(logprice~enginesize, data=train) # data=train to estimate the model
summary(lm.fit0.8)

# Compute RMSE, MAPE
pred0.8 <- predict(lm.fit0.8,newdata=test) # data=test to predict 
err0.8 <- pred0.8-test$logprice
rmse <- sqrt(mean(err0.8^2))
mape <- mean(abs(err0.8/test$logprice))
c(RMSE=rmse,mape=mape,R2=summary(lm.fit0.8)$r.squared) # to print the 3 parameters

plot(test$logprice,pred0.8) # plot of predicted values against test values


#'''''''''''''''''''''''''''''''''
# LOOCV method
#'''''''''''''''''''''''''''''''''

glm.fit <- glm(logprice~enginesize, data=data)
cv.err <- cv.glm(data,glm.fit)
cv.err$delta[1]  # to print the cross-validation statistics
# The best model is the one with the smallest CV statistics


#'''''''''''''''''''''''''''''''''''''''''''''
# Application of the predictive accuracy tool
# 2 models: logprice vs enginesize or horsepower
#'''''''''''''''''''''''''''''''''''''''''''''

# Accuracy on the sample with R² and adjusted R²
lm.fit2 <- lm(logprice~horsepower, data=data)
summary(lm.fit2)

# the fit on the sample is good
# R² = 68%

# validation set method
lm.fit2 <- lm(logprice~horsepower, data=train)
summary(lm.fit2)
names(lm.fit2)
pred2 <- predict(lm.fit2,newdata=test)
err2 <- pred2-test$logprice
rmse <- sqrt(mean(err2^2))
mape <- mean(abs(err2/test$logprice))
c(RMSE=rmse,mape=mape,R2=summary(lm.fit2)$r.squared) # to print the 3 parameters

# Results for the second model
# RMSE > the previous one # the predictive power with horsepower as predictor is better
# MAPE > the previous one



#######################################
# Multiple regression setting
#######################################

# Comments on the dataset: 
# 1) there are a lot of predictors...
# 2) There are qualitative variables, type factor: we can directly include it in the model
# 3) Particular attention to multicolinearity
# 4) Stepwise variable selection


lm.fit=lm(logprice~factor(fueltype)+factor(aspiration)+factor(doornumber)+factor(carbody)+factor(drivewheel)+factor(enginelocation)+wheelbase+carlength+carwidth+carheight+curbweight+factor(enginetype)+factor(cylindernumber)+enginesize+factor(fuelsystem)+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg,
          data=data) # all remaining variables will be used as predictors
summary(lm.fit)
contrasts(chas) 


#--------------------
# Multicolinearity
#------------------
# = too strong link between predictors
# multicolinearity introduces unstability in the model estimation
# and issues in the interpretation of model coefficients

#Check for correlation between quantitative variables
data2 <- data[,c('wheelbase', 'carlength', 'carwidth','carheight', 'curbweight','enginesize', 'boreratio', 'stroke', 'compressionratio', 'horsepower', 'peakrpm', 'citympg', 'highwaympg', 'price')]

cor(data2)
pairs(data2)

# Variance Inflation factor (VIF)
vif(lm.fit)
# there are aliased coefficients in the model


#------------------------------
# Variables stepwise selection
#------------------------------

# We remove first the least significant variable (with the largest pvalue) and re-run the regression
lm.fit1 <- update(lm.fit,~.-factor(drivewheel))
summary(lm.fit1)

lm.fit2 <- update(lm.fit1,~.-factor(cylindernumber))
summary(lm.fit2)

lm.fit3 <- update(lm.fit2,~.-carheight)
summary(lm.fit3)

lm.fit4 <- update(lm.fit3,~.-factor(fuelsystem))
summary(lm.fit4)

lm.fit5 <- update(lm.fit4,~.-factor(doornumber))
summary(lm.fit5)

lm.fit6 <- update(lm.fit5,~.-factor(enginetype))
summary(lm.fit6)

lm.fit7 <- update(lm.fit6,~.-carlength)
summary(lm.fit7)

lm.fit8 <- update(lm.fit7,~.-compressionratio)
summary(lm.fit8)

lm.fit9 <- update(lm.fit8,~.-factor(aspiration))
summary(lm.fit9)

lm.fit10 <- update(lm.fit9,~.-boreratio)
summary(lm.fit10)

lm.fit11 <- update(lm.fit10,~.-enginesize)
summary(lm.fit11)

lm.fit12 <- update(lm.fit11,~.-highwaympg)
summary(lm.fit12)

lm.fit13 <- update(lm.fit12,~.-stroke)
summary(lm.fit13)

lm.fit14 <- update(lm.fit13,~.-peakrpm)
summary(lm.fit14)

lm.fit15 <- update(lm.fit14,~.-wheelbase)
summary(lm.fit15)

lm.fit16 <- update(lm.fit15,~.-factor(fueltype))
summary(lm.fit16)

lm.fitvf <- update(lm.fit16,~.-citympg)
summary(lm.fitvf)

lm.fitvf

# All coefficients are significant in this final regression (carbody, enginelocation, carwidth, curbweight, horsepower).
# The regression equation is : logprice = 5.044 + 0.6088*enginelocation + 0.04461*carwidth + ...

# The R-squared of our model is 0.89. This means that 89% of the variation in price is explained by the variation in the variables above.

vif(lm.fitvf)
# The larger the VIF, the more correlated the variables

#--------------------
# Residuals analysis
#--------------------

# Linear regression analysis assumes that residuals are normally distributed, randomly spread out and not related to the independent variables. We check these assumptions by examining residuals.

residuals <- lm.fitvf$residuals

# Verify that residuals are normally distributed
qqnorm(residuals, main="Are residuals normally distributed?")
qqline (residuals)
# Verify the homoscedasticity and non autocorrelation of residuals
plot(residuals)
plot(residuals, data$carwidth)
plot(residuals, data$horsepower)
plot(residuals, data$curbweight)
plot(residuals, data$carbody)
plot(residuals, data$enginelocation)

# An inspection of the output allows us to confirm that all three assumptions concerning residuals have been respected.
# most of the points fit on the line (except for the extreme quantiles) which indicates that the residuals distribution is not too far from a Normal distribution

# Shapiro-Wilks test
#  Ho: the distribution is Normal
# H1: the distribution is not Normal

shapiro.test(residuals(lm.fitvf))
# p-value = 8e-08 <<<5% so we reject Ho !
# The distribution cannot be considered as Normal
# There must be potential outliers....
hist(residuals(lm.fitvf))


# Homoscedasticity
par(mfrow=c(1,2))
plot(predict(lm.fitvf),residuals(lm.fitvf))
plot(predict(lm.fitvf),rstudent(lm.fitvf))


#----------------------------------
# Evaluate the quality of the model
#----------------------------------

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Fit on the training sample using the R² and adjusted R²
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# the closer R² to 1, the better 

#''''''''''''''''''''''''''''''''''''''
# Predictive power with Validation set 
#'''''''''''''''''''''''''''''''''''''
# Sample the dataset
set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data))
train = data[row.number,]
test = data[-row.number,]
dim(train)
dim(test)

# Estimate the linear fit on the training set
linreg0.8 <- lm(logprice~factor(carbody)+factor(enginelocation)+carwidth+curbweight+horsepower, data=train)
summary(linreg0.8)

# Compute RMSE, MAPE
pred0.8 <- predict(linreg0.8,newdata=test) # data=test to predict 
err0.8 <- pred0.8-test$logprice
rmse <- sqrt(mean(err0.8^2))
mape <- mean(abs(err0.8/test$logprice))
c(RMSE=rmse,mape=mape,R2=summary(linreg0.8)$r.squared) # to print the 3 parameters

plot(test$logprice,pred0.8) # plot of predicted values against test values


# Now that we have a model, we can verify its predictive power. How well does it predict the values in our data set?

cor(lm.fitvf$fitted.values, data$logprice) # Computes the correlation between the fitted values and the actual ones
plot(data$logprice, lm.fitvf$fitted.values) # Plot the fitted values vs. the actual ones

# The correlation coefficient between predicted and actual values is strong and positive (r=0.946)

predictors <- data.frame(horsepower=180, carwidth=70, curbweight=2800, carbody="wagon",
                         enginelocation="front")
predict(lm.fitvf, predictors, interval = "prediction", level = 0.95)

exp(9.795627)
exp(9.441226)
exp(10.15003)

# Final results : fit = 17955.06 $, lwr = 12597.15 $ , upr = 25591.87 $

