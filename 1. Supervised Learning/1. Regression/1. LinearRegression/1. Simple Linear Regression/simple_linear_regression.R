## Template

##importing dataset
dataset = read.csv('Salary_Data.csv')

## Splitting the dataset into train and test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3) ## training dataset ratio
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

## Feature Scaling => Not required for Simple Linear Regression
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Simple Linear Regression to the training set
regressor = lm(formula = Salary ~ YearsExperience, data = training_set) ## fitting linear model
  ## formula = Dependent ~ Independent
summary(regressor) ## gives summary about linear regressor

## Predicting the test set results
y_pred = predict(regressor, newdata = test_set)

# install.packages("ggplot2")
library(ggplot2)
## Visualising the Training set results
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), 
             colour = 'blue') +
  ggtitle("Salary vs Experience (Training set)") +
  xlab("Experience in years") + 
  ylab("Salary in $")

## Visualising the Test set results
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), 
            colour = 'blue') +
  ggtitle("Salary vs Experience (Test set)") +
  xlab("Experience in years") + 
  ylab("Salary in $")
