library(DAAG)
library(caret)

# Importing our datasets
#comp | size | complexity | change_churn | total | minor | major | ownership | review_rate | self_reviews |review_churn_rate | too_quick | little_discussion | bugs
trainingDataSet <- read.csv("./data/qt50.csv", sep=",", header = TRUE)
testDataSet <- read.csv("./data/qt51.csv", sep=",", header = TRUE) 

#Formatting our test data set
trainData <- trainingDataSet
correlations <- cor(qt.training_data[-1], qt.training_data[-1]) #Only numeric data type

#Cleaning our highly correlated columns
trainData$review_churn_rate <- NULL #perfect correlation with "review_rate"

#Defining our model

#0. Setup a seed value
set.seed(12345)

#1. Selecting training features (columns used to predict)
selectedFeatures <- c("size","","","","","","","","","","","","")
trainDataFilteredRows <- trainData[, selectedFeatures]

#


#Evaluating our model
testData <- testDataSet


#Results 
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#



