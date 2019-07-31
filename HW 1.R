################################################################################################
#### Homework 1 Data Mining I
## Created: June 27, 2019
## Edited:
################################################################################################

rm(list = ls())

# set working directory
setwd("/Users/m/Desktop/ADS/Data Mining I/Week 1/HW 1")






################################################################################################
#### Problem 1
################################################################################################
library("ISLR")

# data <- read.csv("Auto.csv", sep=",", header=TRUE)
# head(data)

head(Auto)
names(Auto)

# take a look at all the data for any obvious correlation
pairs(Auto)

# start out by just looking at mpg
quartz()
par(mfrow=c(1,3))
hist(Auto$mpg)
boxplot(Auto$mpg, horizontal=TRUE, main="mpg")
plot(mpg~year, data=Auto, col=Auto$origin)
# looks like as time goes on, mpg steadily increases

# compare mpg by country of origin
quartz()
stripplot(origin~mpg, xlab="mpg by country", data=Auto)
# American made cars have noticeably lower mpg and a wider spread than either European or Japanese made cars
# confirmed by the density plot
quartz()
densityplot(~mpg | origin, data=Auto, auto.key=list(space="right"))


# start by looking at each quantitative variable and its effect on mpg
quartz()
par(mfrow=c(1,7))
plot(mpg ~ cylinders, data=Auto, main="Effect of Cylinders", pch=16)
plot(mpg ~ displacement, data=Auto, main="Effect of Engine Size", pch=16)
plot(mpg ~ horsepower, data=Auto, main="Effect of Horsepower", pch=16)
plot(mpg ~ weight, data=Auto, main="Effect of Car's Weight", pch=16)
plot(mpg ~ acceleration, data=Auto, main="Car's Acceleration", pch=16)
plot(mpg ~ year, data=Auto, main="Year Car was Built", pch=16)
plot(mpg ~ origin, data=Auto, main="Country of Origin", pch=16)

# take a closer look now at displacement, horsepower, weight, and acceleration
quartz()
par(mfrow=c(1,4))
plot(mpg ~ displacement, data=Auto, main="Effect of Engine Size", pch=16)
plot(mpg ~ horsepower, data=Auto, main="Effect of Horsepower", pch=16)
plot(mpg ~ weight, data=Auto, main="Effect of Car's Weight", pch=16)
plot(mpg ~ acceleration, data=Auto, main="Car's Acceleration", pch=16)

# displacement, horsepower, and weight seem to follow a similar pattern
# add a smooth curve and account for country of origin
quartz()
par(mfrow=c(1,3))
plot(mpg ~ displacement, data=Auto, main="Effect of Engine Size", pch=16, col=Auto$origin)
with(Auto, lines(lowess(displacement, mpg), lines=2))
plot(mpg ~ horsepower, data=Auto, main="Effect of Horsepower", pch=16, col=Auto$origin)
with(Auto, lines(lowess(horsepower, mpg), lines=2))
plot(mpg ~ weight, data=Auto, main="Effect of Car's Weight", pch=16, col=Auto$origin)
with(Auto, lines(lowess(weight, mpg), lines=2))
# can see distinct differences between the three chosen variables more clearly now
# consistently seeing that American cars have lower mpg, larger engines, more horsepower, and are the heaviest
  # would naturally assume that this factors into American cars having lower mpg than competitors
  # heavy cars with large engines and more horsepower will likely be less fuel efficient


# look at some boxplots to identify outliers
quartz()
par(mfrow=c(1,4))
boxplot(Auto$displacement, horizontal=TRUE, main="Engine Displacement")
boxplot(Auto$horsepower, horizontal=TRUE, main="Horsepower")
boxplot(Auto$weight, horizontal=TRUE, main="Car Weight")
boxplot(Auto$acceleration, horizontal=TRUE, main="0-60")
# see outliers in horsepower and acceleration
quartz()
par(mfrow=c(1,2))
boxplot(Auto$horsepower, horizontal=TRUE, main="Horsepower")
boxplot(Auto$acceleration, horizontal=TRUE, main="0-60")

# remove name from list of variables
auto <- Auto[,-9]
names(auto)
# would ideally like to break this into 2 variables "make" and "model" but am not quite sure how to accomplish this with the data as is

# further investigate correlation between variables seen previously
quartz()
par(mfrow=c(2,3))
plot(displacement~horsepower, data=Auto) # strong
plot(displacement~acceleration, data=Auto) # no correlation
plot(displacement~weight, data=Auto) # strong
plot(horsepower~acceleration, data=Auto) # moderate
plot(horsepower~weight, data=Auto) # strong
plot(acceleration~weight, data=Auto) # no correlation






################################################################################################
#### Problem 2
################################################################################################
library("leaps")

# start by looking at a model containing all of the variables
large_model <- lm(mpg ~., data=auto)
summary(large_model)
# see cylinders, horsepower, and acceleration can all be removed at the 0.05 level

# trim the large model
trim_large_model <- lm(mpg~.-cylinders -horsepower -acceleration, data=auto)
summary(trim_large_model)
# see a slight drop in adjusted r-squared and that displacement can also be dropped

# trim the model again
trim_again_large_model <- lm(mpg~.-cylinders -horsepower -acceleration -displacement, data=auto)
summary(trim_again_large_model)
# little to no change in adjusted r-squared and all variables are significant at the 0.05 level now
  # expected to see these variables removed, logically, since they would all contribute to a lower mpg

# best single model based on number of beta-terms specified
regfit.full_one <- regsubsets(mpg ~ ., data=auto, nvmax=8, method="exhaustive", really.big=T)
my_sum <- summary(regfit.full_one)
my_sum
# top two models based on number of beta-terms specified
regfit.full_two <- regsubsets(mpg ~ ., data=auto, nbest=2, nvmax=8, method="exhaustive", really.big=T)
summary(regfit.full_two)

# plot the results to visualize what we calculated
quartz()
par(mfrow=c(2,2))
plot(my_sum$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(my_sum$adjr2, xlab="Number of Variables", ylab="Adjusted R^2", type="l")
plot(my_sum$cp, xlab="Number of Variables", ylab="Cp", type="l")
plot(my_sum$bic, xlab="Number of Variables", ylab="BIC", type="l")
# can see that the model with 3 terms is ~the best option available
  # this is confirmed between the linear models I manually fit and the regsubsets performed
    # can see that the model that includes weight, year, and origin is the best predictor for mpg


# now want to look deeper into the country of origin and how that affects things
  # start by creating new variables "American", "European", and "Japanese"
auto$American <- NA
auto$American[auto$origin=="1"] <- "1"
auto$American[auto$origin=="2"] <- "0"
auto$American[auto$origin=="3"] <- "0"
auto$European <- NA
auto$European[auto$origin=="2"] <- "1"
auto$European[auto$origin=="1"] <- "0"
auto$European[auto$origin=="3"] <- "0"
auto$Japanese <- NA
auto$Japanese[auto$origin=="3"] <- "1"
auto$Japanese[auto$origin=="1"] <- "0"
auto$Japanese[auto$origin=="2"] <- "0"

# remove origin
new_auto <- auto[,-8]


# perform analysis again
new_model <- lm(mpg~., data=new_auto)
summary(new_model)

# create some (educated) interactions between variables
int_model_am_one <- lm(mpg~displacement+displacement*American+weight+year, data=new_auto)
summary(int_model_am_one)
int_model_am_two <- lm(mpg~displacement+weight+weight*American+year, data=new_auto)
summary(int_model_am_two)
# saw from the summary that displacement is not significant at the 0.05 level int he last model
two_two <- lm(mpg~weight+weight*American+year, data=new_auto)
summary(two_two)

int_model_am_three <- lm(mpg~displacement+weight+year+year*American, data=new_auto)
summary(int_model_am_three)


# look at the 
# best single model based on number of beta-terms specified
regfit.full_one <- regsubsets(mpg ~ displacement+displacement*American+weight+weight*American+year+year*American, data=new_auto, method="exhaustive", really.big=T)
my_sum_two <- summary(regfit.full_one)
my_sum_two


# plot the results to visualize what we calculated
quartz()
par(mfrow=c(2,2))
plot(my_sum_two$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(my_sum_two$adjr2, xlab="Number of Variables", ylab="Adjusted R^2", type="l")
plot(my_sum_two$cp, xlab="Number of Variables", ylab="Cp", type="l")
plot(my_sum_two$bic, xlab="Number of Variables", ylab="BIC", type="l")
# can see visually that the model with 5 terms is approximately the best fit for the data
  # interstingly, displacement is not included
  # two interaction terms (weight*American and year*American) are included
    # saw previously that all cars performed better over time and this data tells a similar story





################################################################################################
#### Problem 3
################################################################################################
library("ElemStatLearn")
library("ggplot2")
library("mclust")

#######################################################################################

# Thank you for your help professor!!!

### CORRECTION: you want to grab the 2's and the 3's
zip.train[,1] # the digits are in the first column of the train
zip.test[,1] # and the first column of the test

train_indis <- which((zip.train[,1] == 2 | zip.train[,1]== 3) == TRUE) #this logical argument takes 2's or 3's row indis
test_indis <- which((zip.test[,1] == 2 | zip.test[,1]== 3) == TRUE) #this logical argument takes 2's or 3's row indis

my_train <- data.frame(zip.train[train_indis, ]) #your "subset" training set, convert to data.frame (lm does not like matrix)
my_test <- data.frame(zip.test[test_indis, ]) #your "subset" test set

dim(my_train) # a check, it is smaller
dim(my_test)  # a check, it is smaller

colnames(my_train) # take a look at the variable names
colnames(my_test) # take a look at the variable names  -- they need to match, and they do

colnames(my_train)[1] <- "Y" # lets rename the first column "Y" so that we do not get confused.
colnames(my_test)[1] <- "Y"

## Linear Regression  --- as you had it!
reg <- lm(Y ~ ., data = my_train)
summary(reg)

?predict.lm # just to remind you
y_hat0 <- predict(reg, newdata = my_test) # predict a response value (y_hat) for the test set
y_hat0 # take a look, none are exactly 2 or 3, and some of these are out of range (e.g., obs 360)

# we are solving this problem as a regression, but it is a classification,
# we need to take one more step to assign "2" or "3" to the different predictions.

y_hat <- y_hat0 # assign y_hat to be what you got out of the regression, and you some "decision rules below" to assign the final class.
for (i in 1:length(y_hat0)){
  if (y_hat0[i] < 2.4999){ #what does the prediction look like, if it is less than or equal to 2.4999 , lets call it a 2
    y_hat[i] <- 2}
  else{
    y_hat[i] <- 3} # otherwise, lets call it a 3
}

mismatch <- length(which(y_hat != my_test$Y)) # 15 errors
lm_error_rate <- mismatch/length(y_hat)
lm_error_rate


## KNN
knn.train <- my_train[,2:257] 
knn.test <- my_test[,2:257]
knn.train.Y <- as.factor(my_train$Y) 
knn.test.Y <- as.factor(my_test$Y)

# KNN Predictions
predictions.knn.test <- sapply(1:15, function(k) { 
  knn(train = knn.train, test = knn.test, cl = knn.train.Y, k = k)
  })
predictions.knn.train <- sapply(1:15, function(k) {
  knn(train = knn.train, test = knn.train, cl = knn.train.Y, k = k)
  })


# Compute error rates
errors.xs <- 1:15

errors.knn.test <- apply(predictions.knn.test, 2, function(prediction) {
  classError(prediction , as.factor(my_test$Y))$errorRate})

errors.knn.train <- apply(predictions.knn.train, 2, function(prediction) {
  classError(prediction, as.factor(my_train$Y))$errorRate})

errors.lm.test <- sapply(errors.xs, function(k) {
  classError(y_hat, as.factor(my_test$Y))$errorRate})

errors.lm.train <- sapply(errors.xs, function(k) {
  classError(y_hat, as.factor(my_train$Y))$errorRate})

errors <- data.frame("K"=errors.xs, "KNN.Train"=errors.knn.train, "KNN.Test"=errors.knn.test, "LR.Train"=errors.lm.train ,"LR.Test"=errors.lm.test)








################################################################################################
#### Problem 4
################################################################################################
library(MASS)
?Boston
dim(Boston)
# 506 rows, 14 columns
# 14 features, 506 housing values in Boston suburbs


# (b)
pairs(Boston)
# X correlates with: a, b, c
# crim: age, dis, rad, tax, ptratio
# zn: indus, nox, age, lstat
# indus: age, dis
# nox: age, dis
# dis: lstat
# lstat: medv

# (c)
quartz()
par(mfrow=c(2,3))
plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)
plot(Boston$rad, Boston$crim)
plot(Boston$tax, Boston$crim)
plot(Boston$ptratio, Boston$crim)
# Older homes, more crime
# Closer to work-area, more crime
# Higher index of accessibility to radial highways, more crime
# Higher tax rate, more crime
# Higher pupil:teacher ratio, more crime

# (d)
par(mfrow=c(1,3))
hist(Boston$crim[Boston$crim>1], breaks=25)
# most cities have low crime rates, but there is a long tail: 18 suburbs appear to have a crime rate > 20, reaching to above 80
hist(Boston$tax, breaks=25)
# there is a large divide between suburbs with low tax rates and a peak at 660-680
hist(Boston$ptratio, breaks=25)
# a skew towards high ratios, but no particularly high ratios









