##############################################
## Example of Decision Trees Classification ##
##############################################

## Dataset Used : Social_Network_Ads.csv
## Target : Whether user purchased or not

##########################################
################# NOTES ##################
##########################################
# 1. Random Forest, Gradient Boosting, etc are upgrades of Decision Trees.

## load data
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5] ## removing User.ID & Gender

str(dataset)

## Encoding the target feature(Purchase) as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0,1))

## Splitting dataset into training and test set
library(caTools)
set.seed(123)
split = sample.split(dataset, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

## Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

## Building a Decision Tree classifier
install.packages('rpart')
library(rpart)
classifier = rpart(formula = Purchased ~ .,data = training_set)

## Plotting Decision tree => Not ideal for large data/number of features
plot(classifier)
text(classifier)

## Predicting the test set results
predicted_set = predict(classifier, newdata = test_set) ## gives probablities of 0 & 1
predicted_set = predict(classifier, newdata = test_set, type = 'class') ## gives class-type
