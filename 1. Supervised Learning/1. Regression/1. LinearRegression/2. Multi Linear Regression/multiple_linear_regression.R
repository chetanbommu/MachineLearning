## Multiple Linear Regression

## import the dataset
dataset = read.csv("50_Startups.csv")

## Encoding catergorical data
dataset$State = factor(dataset$State,
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3))

## Splitting the dataset into the training set and testing set
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

## Feature Scaling
## Not needed

## Fitting Multiple Linear Regression on the training set
regressor = lm(formula = Profit ~ ., data = training_set)
  ## Profit ~ R.D.Spend + Administration + Marketing.Spend + State
  ## R takes spaces as dot, so R D Spend => R.D.Spend
summary(regressor)

## Predicting the test set values
y_pred = predict(regressor, testing_set)

## Except R.D.Spend, all other predictors is of no significance,so we can remove them.
regressor = lm(formula = Profit ~ R.D.Spend, data = training_set)
summary(regressor)
y_pred = predict(regressor, testing_set)

## Building the optimal model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = training_set)
summary(regressor) ## State = 0.974 => p > SL , so let's remove State2 since it is not statistically significant

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, data = training_set)
summary(regressor) ## Administartion = 0.609 => remove it

regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend, data = training_set)
summary(regressor) ## Marketing.Spend = 0.07 => remove it

regressor = lm(formula = Profit ~ R.D.Spend, data = training_set)
summary(regressor) ## Final Model