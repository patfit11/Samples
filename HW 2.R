################################################################################################
#### This code is for Data Mining I HW 2
## Created: July 16, 2019
## Edited:
################################################################################################

rm(list = ls())

# set working directory
setwd("/Users/m/Desktop/ADS/Data Mining I/Week 3/HW 2")

library(ISLR)
library(dplyr)
library(plyr)
library(DAAG)
library(car)
library(leaps)
library(glmnet)
library(pls)
library(caret)
library(ggthemes)
library(ggplot2)
library(corrplot)
# set the seed for reproducibility
set.seed(1234)


################################################################################################
#### Problem 1 Data Prep
################################################################################################
# convert categorical variables
College$Private <- revalue(College$Private, c("Yes"=1))
College$Private <- revalue(College$Private, c("No"=0))

# generate a random sample of 80% of the College data
training_row <- sample(1:nrow(College), 0.8*nrow(College))
train <- College[training_row, ]
test <- College[-training_row, ]

preObj <- preProcess(train, method=c("center", "scale"))

# create scaled versions of the data
training <- predict(preObj, train)
testing <- predict(preObj, test)

y_train <- training$Apps
y_test <- testing$Apps

one_hot_encoding <- dummyVars(Apps ~., data=training)
x_train <- predict(one_hot_encoding, training)
x_test <- predict(one_hot_encoding, testing)






######################
#### Problem 1(a) ####
######################
# fit a linear model
lmMod <- lm(Apps ~., data=training)
summary(lmMod)

# predict for the test data
pred <- predict(lmMod, testing)
summary(pred)

# calculate the test error
y_hat <- pred
test_error_lm <- sum((y_hat - y_test)^2)
test_error_lm

lm_info <- postResample(pred, testing$Apps)
lm_info



######################
#### Problem 1(b) ####
######################
# create the ridge
ridge_mod <- glmnet(x=x_train, y=y_train, alpha=0)
ridge_mod

# cross-validate the ridge
cv.out <- cv.glmnet(x_train, y_train, alpha=0)
plot(cv.out)

# choose the best lambda
names(cv.out)
bestlam <- cv.out$lambda.min # the smallest lambda will be the best lambda
bestlam
ridge_pred <- predict(ridge_mod, s=bestlam, type="coefficients") # predict the coefficients for the "optimal" model
ridge_pred


# take everything not in the training set and predict
ridge_pred_2 <- predict(ridge_mod, s=bestlam, newx=x_test, type="response") 
ridge_pred_2

# calculate the test error
test_error_ridge <- sum((ridge_pred_2 - y_test)^2)
test_error_ridge

ridge_info <- postResample(predict(cv.out, x_test), y_test)
ridge_info



######################
#### Problem 1(d) ####
######################
# create the lasso
lasso_mod <- glmnet(x=x_train, y=y_train, alpha=1)
lasso_mod

# cross-validate the lasso
cv.out_lasso <- cv.glmnet(x_train, y_train, alpha=1)
plot(cv.out_lasso)

# choose the best lambda
names(cv.out_lasso)
bestlam <- cv.out_lasso$lambda.min # the smallest lambda will be the best lambda
bestlam
lasso_pred <- predict(lasso_mod, s=bestlam, type="coefficients") # predict the coefficients for the "optimal" model
lasso_pred


# take everything not in the training set and predict
lasso_pred_2 <- predict(lasso_mod, s=bestlam, newx=x_test, type="response") 
lasso_pred_2

# calculate the test error
test_error_lasso <- sum((lasso_pred_2 - y_test)^2)
test_error_lasso

lasso_info <- postResample(predict(cv.out_lasso, x_test), y_test)
lasso_info



######################
#### Problem 1(e) ####
######################
# convert the data for the PCR function
my_dats <- as.matrix(training)
NAmat = matrix(as.numeric(is.na(my_dats)), ncol=18)
nonNAdx = which(rowSums(NAmat)==0)
college_train <- training[nonNAdx, ] # the data we will model

# convert the data for the PCR function
my_dats_two <- as.matrix(testing)
NAmat = matrix(as.numeric(is.na(my_dats_two)), ncol=18)
nonNAdx = which(rowSums(NAmat)==0)
college_test <- testing[nonNAdx, ] # the data we will model

# fit the PCR model
pcr.fit = pcr(Apps ~., data=college_train, scale=TRUE, validation="CV")
summary(pcr.fit)

# plot the MSEP based on our PCR model
validationplot(pcr.fit, val.type="MSEP") # how many components to minimize the mean-squared-error 

# calculate the test error
training_error_store <- c()
test_error_store <- c()
for (i in 1:18){
  pcr.predict.train = predict(pcr.fit, newdata=college_train, ncomp=i)
  pcr.predict.test = predict(pcr.fit, newdata=college_test, ncomp=i)
  train.error <- mean((pcr.predict.train-y_train)^2)
  test.error <- mean((pcr.predict.test-y_test)^2)
  training_error_store <- c(training_error_store, train.error)
  test_error_store <- c(test_error_store, test.error)
}

quartz()
plot(training_error_store)

quartz()
plot(test_error_store)

# calculate test error
which.min(test_error_store)
pcr_model <- train(x=x_test, y=y_test, method="pcr")
pcr_info <- postResample(predict(pcr_model, x_test), y_test)
pcr_info
coef(pcr_model$finalModel)



######################
#### Problem 1(f) ####
######################
pls.fit = plsr(Apps ~., data=college_train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

# calculate the test error
training_error_store_pls <- c()
test_error_store_pls <- c()
for (i in 1:18){
  pls.predict.train = predict(pls.fit, newdata=college_train, ncomp=i)
  pls.predict.test = predict(pls.fit, newdata=college_test, ncomp=i)
  train.error.pls <- mean((pls.predict.train-y_train)^2)
  test.error.pls <- mean((pls.predict.test-y_test)^2)
  training_error_store_pls <- c(training_error_store_pls, train.error.pls)
  test_error_store_pls <- c(test_error_store_pls, test.error.pls)
}


quartz()
plot(training_error_store_pls)

quartz()
plot(test_error_store_pls)

# calculate test error
which.min(test_error_store_pls)
pls_model <- train(x=x_test, y=y_test, method="pcr")
pls_info <- postResample(predict(pls_model, x_test), y_test)
pls_info
coef(pls_model$finalModel)

######################
#### Problem 1(g) ####
######################
test_errors <- rbind(lm_info, ridge_info, lasso_info, pcr_info, pls_info)
test_errors
residfunc <- function(fit, data) {
  predict(fit, data) - testing$Apps
}

data_frame(Observed = testing$Apps,
           LM = residfunc(lmMod, testing),
           Ridge = residfunc(cv.out, x_test),
           Lasso = residfunc(cv.out_lasso, x_test),
           PCR = residfunc(pcr_model, x_test),
           PLS = residfunc(pls_model, x_test)) %>%
  gather(Model, Residuals, -Observed) %>%
  ggplot(aes(Observed, Residuals, col = Model)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = 'loess', alpha = 0.01, col = 'lightsalmon2') +
  facet_wrap(~ Model, ncol = 5) +
  theme_tufte() +
  theme(legend.position = 'top') +
  coord_flip()

################################################################################################
#### Problem 2
################################################################################################
rm(list = ls())

# set working directory
setwd("/Users/m/Desktop/ADS/Data Mining I/Week 3/HW 2")

library(ISLR)
library(dplyr)
library(plyr)
library(DAAG)
library(car)
library(leaps)
library(glmnet)
library(pls)
library(caret)
library(ggthemes)
library(ggplot2)
library(corrplot)
# set the seed for reproducibility
set.seed(1234)

# load the data
train <- read.delim("train.txt", head=TRUE, sep="\t", dec=".")
tests <- read.delim("test.txt", head=TRUE, sep="\t", dec=".")
targets <- read.delim("targets.txt", head=TRUE, sep="\t", dec=".")

# fix the variable names
my_vars <- c(paste("Var", 1:85, sep=":"), "Purchase")
colnames(train) <- my_vars
test <- cbind(tests, targets)
colnames(test) <- my_vars

x_train <- as.matrix(train[,-86])
y_train <- train$Purchase
x_test <- as.matrix(test[,-86])
y_test <- test$Purchase




# fit a linear model
lmMod <- lm(Purchase ~., data=train)
summary(lmMod)

# predict for the test data
pred <- predict(lmMod, test)
summary(pred)

# report the training error
lm_pred_info <- postResample(pred, test$Purchase)
lm_pred_info

signif_all <- names(all_vifs)

# narrow down our choose of variables
selectMod <- step(lmMod)
all_vifs <- car::vif(selectMod)

# Remove vars with VIF> 4 and re-build model until none of VIFs don't exceed 4.
while(any(all_vifs > 4)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("Purchase ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=train)  # re-build model with new formula
  all_vifs <- car::vif(selectedMod)
}
summary(selectedMod)
coef(selectedMod)
# can see there are 25 variables included in the model







# convert the data for the regsubset function
my_dats <- as.matrix(train)
NAmat = matrix(as.numeric(is.na(my_dats)), ncol=86)
nonNAdx = which(rowSums(NAmat)==0)
caravan_train <- train[nonNAdx, ] # the data we will model

my_dat <- as.matrix(test)
NAmat_two = matrix(as.numeric(is.na(my_dat)), ncol=86)
nonNAdx = which(rowSums(NAmat_two)==0)
caravan_test <- test[nonNAdx, ] # the data we will model



# forward selection
regfit.fwd <- regsubsets(Purchase ~ ., data=caravan_train, nvmax=85, method="forward")
my_sum_fwd <- summary(regfit.fwd)
quartz()
par(mfrow=c(2,2))
plot(my_sum_fwd$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(my_sum_fwd$adjr2, xlab="Number of Variables", ylab="Adjusted R^2", type="l")
plot(my_sum_fwd$cp, xlab="Number of Variables", ylab="Cp", type="l")
plot(my_sum_fwd$bic, xlab="Number of Variables", ylab="BIC", type="l")

val.errors = rep(NA, 85)
x.test = model.matrix(Purchase ~ ., data = caravan_test)
for (i in 1:85) {
  coefi = coef(regfit.fwd, id = i)
  pred = x.test[, names(coefi)] %*% coefi
  val.errors[i] = mean((caravan_test$Purchase - pred)^2)
}
plot(sqrt(val.errors), ylab = "Root MSE", pch = 19, type = "b")
points(sqrt(regfit.fwd$rss[-1]/180), col = "blue", pch = 19, type = "b")
legend("topright", legend = c("Validation"), col = "black", 
       pch = 19)
# can see the model with ~20-28 variables is the optimal




# backward selection
regfit.bwd <- regsubsets(Purchase ~ ., data=caravan_train, nvmax=85, method="backward")
my_sum_bwd <- summary(regfit.bwd)
quartz()
par(mfrow=c(2,2))
plot(my_sum_bwd$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(my_sum_bwd$adjr2, xlab="Number of Variables", ylab="Adjusted R^2", type="l")
plot(my_sum_bwd$cp, xlab="Number of Variables", ylab="Cp", type="l")
plot(my_sum_bwd$bic, xlab="Number of Variables", ylab="BIC", type="l")

val.errors = rep(NA, 85)
x.test = model.matrix(Purchase ~ ., data = caravan_test)
for (i in 1:85) {
  coefi = coef(regfit.bwd, id = i)
  pred = x.test[, names(coefi)] %*% coefi
  val.errors[i] = mean((caravan_test$Purchase - pred)^2)
}
plot(sqrt(val.errors), ylab = "Root MSE", pch = 19, type = "b")
points(sqrt(regfit.bwd$rss[-1]/180), col = "blue", pch = 19, type = "b")
legend("topright", legend = c("Validation"), col = "black", 
       pch = 19)
# can see the model with 33-40 variables is the optimal




# create the ridge
ridge_mod <- glmnet(x=x_train, y=y_train, alpha=0)
ridge_mod

# cross-validate the ridge
cv.out <- cv.glmnet(x_train, y_train, alpha=0)
plot(cv.out)

# choose the best lambda
names(cv.out)
bestlam <- cv.out$lambda.min # the smallest lambda will be the best lambda
bestlam
ridge_pred <- predict(ridge_mod, s=bestlam, type="coefficients") # predict the coefficients for the "optimal" model
ridge_pred


# take everything not in the training set and predict
ridge_pred_2 <- predict(ridge_mod, s=bestlam, newx=x_test, type="response") 
ridge_pred_2

# calculate the test error
test_error_ridge <- sum((ridge_pred_2 - y_test)^2)
test_error_ridge

ridge_info <- postResample(predict(cv.out, x_test), y_test)
ridge_info







# create the lasso
lasso_mod <- glmnet(x=x_train, y=y_train, alpha=1)
lasso_mod

# cross-validate the lasso
cv.out_lasso <- cv.glmnet(x_train, y_train, alpha=1)
plot(cv.out_lasso)

# choose the best lambda
names(cv.out_lasso)
bestlam <- cv.out_lasso$lambda.min # the smallest lambda will be the best lambda
bestlam
ridge_pred <- predict(lasso_mod, s=bestlam, type="coefficients") # predict the coefficients for the "optimal" model
ridge_pred


# take everything not in the training set and predict
lasso_pred_2 <- predict(lasso_mod, s=bestlam, newx=x_test, type="response") 
lasso_pred_2

# calculate the test error
test_error_lasso <- sum((lasso_pred_2 - y_test)^2)
test_error_lasso

lasso_info <- postResample(predict(cv.out_lasso, x_test), y_test)
lasso_info












################################################################################################
#### Problem 3
################################################################################################
rm(list = ls())

# set working directory
setwd("/Users/m/Desktop/ADS/Data Mining I/Week 3/HW 2")

library(ISLR)
library(dplyr)
library(plyr)
library(DAAG)
library(car)
library(leaps)
library(glmnet)
library(pls)
library(caret)
library(ggthemes)
library(ggplot2)
library(corrplot)
# set the seed for reproducibility
set.seed(1234)


df <- data.frame(replicate(20, rnorm(n = 1000)))

df %>% reduce(function(y, x) y + ifelse(runif(1) < 0.5, rnorm(1, mean = 5, sd = 1), 0)*x + rnorm(1000)) -> df$Y

# split the data into training and test sets
trainIndex <- createDataPartition(df$Y, p = 0.9, list = F)

x_train <- df[trainIndex, -21]
y_train <- df[trainIndex, 21]
x_test <- df[-trainIndex, -21]
y_test <- df[-trainIndex, 21]


# perform subset selection and plot the training MSE
best_set <- regsubsets(x = x_train, y = y_train, nvmax = 20)
best_set_summary <- summary(best_set)
best_set_summary

# list and then plot the MSE for the best model of each size
data_frame(train_error = best_set_summary$rss/900, vars = 1:20) %>%
  spread(vars, train_error)
train_error <- best_set_summary$rss/900
plot(train_error, type="b", main="Best MSE for model size", xlab="# of variables")


test_error = rep(NA,19)
test.mat <- model.matrix(Y ~ ., data = df[-trainIndex,])
for (i in 1:20){
  coefs = coef(best_set, id=i)
  pred = test.mat[,names(coefs)]%*%coefs
  test_error[i] = mean((y_test-pred)^2)
}

# plot of the test set MSE associated witht he best model of each size
plot(test_error, type="b", main="Best MSE for model size", xlab="# of variables")

# minimim MSE value is found with 11 variables
which.min(test_error)


corrplot(cor(df), method = 'color', type = 'lower', diag = F)
# we can see that ~10 variables correlate well with the response variable Y

# when setting up our response variable Y at the start of this problem, a runif(1) < 0.5 was used to generate the values
# this would mean that half the time the coefficients would be 0
# half of our 20 varaibles would be 10 and we found the MSE was minimized with 11 variables included


