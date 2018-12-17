##########################################
##### Example on Logistic Regression #####
##########################################

## Dataset Used : universalBank.csv
## Target : Check whether the person takes/needs Personal Loan.

## Good Read : https://www.analyticsvidhya.com/blog/2015/11/beginners-guide-on-logistic-regression-in-r/

##########################################
################# NOTES ##################
##########################################
# 1. Logistic Regression is a classification algorithm. It is used to predict a binary outcome (1 / 0, Yes / No, True / False) given a set of independent variables.
# 2. Logistic regression as a special case of linear regression when the outcome variable is categorical, where we are using log of odds as dependent variable.
# 3. Logistic Regression is part of a larger class of algorithms known as Generalized Linear Model (glm)

########################################################
## Want to write your own logistic regression function ?
## https://trainings.analyticsvidhya.com/courses/course-v1:AnalyticsVidhya+Python-Final-Jan-Feb+Python-Session-1/courseware/73167b5cca8447dfa535a80d3961dc61/821ac77ab0104820bfa494cb55b237d9/?activate_block_id=block-v1%3AAnalyticsVidhya%2BPython-Final-Jan-Feb%2BPython-Session-1%2Btype%40sequential%2Bblock%40821ac77ab0104820bfa494cb55b237d9
########################################################

##########################################
### Assumptions of Logistic Regression ###
##########################################
# 1. GLM does not assume a linear relationship between dependent and independent variables. However, it assumes a linear relationship between link function and independent variables in logit model.
# 2. The dependent variable need not to be normally distributed.
# 3. It does not uses OLS (Ordinary Least Square) for parameter estimation. Instead, it uses maximum likelihood estimation (MLE).
# 4. Errors need to be independent but not normally distributed.


##############################################
## Performance of Logistic Regression Model ##
##############################################
# 1. Always Prefer a model with minimum AIC(Akaike Information Criteria – The analogous metric of adjusted R² in logistic regression is AIC) value
# 2. Lower the value of Null Deviance(Null Deviance indicates the response predicted by a model with nothing but an intercept), better the model.
# 3. Lower the value of Residual Deviance(Residual deviance indicates the response predicted by a model on adding independent variables), better the model.
# 4. Confusion Matrix (Recall, Precision & F-Score ).
  ## Recall (P) = TP / (TP + FP) 
  ## Precision (R) =  TP / (TP + FN)
  ## F1-Score =  2PR/ (P + R)
# 5. Higher the AUC(area under curve) of ROC curve, better the prediction power of the model.
  ## ROC Curve : The area under curve (AUC), referred to as index of accuracy(A) or concordance index, is a perfect performance metric for ROC curve.
              ## The ROC of a perfect predictive model has TP equals 1 and FP equals 0.
# 6. Maximum Likelihood Estimate (Multiplication of Predicted values) or Log Likelihood Estimate.
  ## Likelihood indicates goodness of fit as its value approaches 1, and a poor fit of the data as its value approaches 0.
  ## Since value of predicted values i.e., probability lies between 0 & 1.


##############################################
############### Steps To Follow ##############
##############################################
# 1. Load dataset fow which we are building a model into a variable. Here i'm using UniversalBank.csv dataset.


## 1. Load data
universal_bank_dataset = read.csv('LogisticRegression/universalBank.csv')
str(universal_bank_dataset) ## check the datatypes of each column and perform feature engineering if required.
summary(universal_bank_dataset) ## check the Min, Max values of each column and scale them if necessary

## 2. Perform Data Pre-Processing (Outliers, Null impedation, Factor conversion)
  ## i . Remove UnNecessary columns
universal_bank_dataset$ID = NULL ## removes ID column from dataset.

  ## ii . Convert columns datatypes 
universal_bank_dataset$ZIP.Code = as.factor(universal_bank_dataset$ZIP.Code) ## converts ZIP.code into factor data type

## check levels of Zip.code
unique(universal_bank_dataset$ZIP.Code) ## 467 Levels
universal_bank_dataset$ZIP.Code = NULL ## difficult to perfome one-hot encoding

## Handling Experience columns
sum(universal_bank_dataset$Experience < 0)

  ## iii. Correlation
library(corrplot)
corrplot(cor(universal_bank_dataset), method='number') 
  ## Observations: 
    # a. Age & Experience are highly correlated i.e., 0.99. So, remove one. I'm going with Experience
universal_bank_dataset$Experience = NULL

  ## iv. Scaling
min_max_scaling_function = function(x){
  return( (x-min(x)) / (max(x)-min(x)) ) 
}

for(i in 1:ncol(universal_bank_dataset)){
  if(names(universal_bank_dataset)[i] %in% c('Personal.Loan','Online','CD.Account','Securities.Account','CreditCard')){
    universal_bank_dataset[,i] = universal_bank_dataset[,i]
  }
  else{
    universal_bank_dataset[,i] = min_max_scaling_function(universal_bank_dataset[,i])  
  }
}

## Verify scaling is done
summary(universal_bank_dataset)

## 3. Train, Test & Split
nrows = c(1:nrow(universal_bank_dataset)) ## dataset has 5000 rows
training_rows = sample(nrows, 3500)

training_data = universal_bank_dataset[training_rows,]
test_data = universal_bank_dataset[-training_rows,]

## 4. Build Model
model = glm(Personal.Loan~., data = universal_bank_dataset, family = binomial(link = 'logit'))
summary(model)
  ## Observations:
    # Age & Mortigage are no-significance columns
universal_bank_dataset$Age = NULL
universal_bank_dataset$Mortgage = NULL

## Re-Build Model
model = glm(Personal.Loan~., data = universal_bank_dataset, family = binomial(link = 'logit'))
summary(model)

## 5. Prediction (in probablities)
predictions = predict(model, test_data, type = 'response')
predictions = ifelse(predictions>0.09,1,0) 

## 6. Check Performance
## Confusion Matrix (Good Read : https://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/)
confusion_matrix = table(test_data$Personal.Loan, predictions, dnn = c('Actual','Preds'))
## Recall =>TP/actual yes
recall_value = confusion_matrix[2,2] / (confusion_matrix[2,1] + confusion_matrix[2,2])
## Precision => TP/predicted yes
precision_value = confusion_matrix[2,2] / (confusion_matrix[1,2] + confusion_matrix[2,2])
## F- Scofre => This is a weighted average of the true positive rate (recall) and precision 
f_score = 2 * recall_value * precision_value / (recall_value + precision_value)
## Accuracy => (TP+TN)/total
accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / (confusion_matrix[1,1] + confusion_matrix[1,2] + confusion_matrix[2,1] + confusion_matrix[2,2]) 

## ROC Curve
library(ROCR)
prediction_ROCR = prediction(predictions = predictions, labels = test_data$Personal.Loan) # ROCR Prediction function takes predicted values and true values
performance_ROCR = performance(prediction_ROCR,'tpr','fpr')
plot(performance_ROCR, colorize = T,print.cutoffs.at=seq(0,1,by=0.1),
     text.adj=c(1.2,1.2), avg="threshold", lwd=3,
     main= "ROC")

## Area Under ROC Curve
library(pROC)
roc_object = roc(test_data$Personal.Loan, predictions)
auc(roc_object)
