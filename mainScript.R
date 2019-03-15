library(DAAG)
library(caret)

# IMPORT DATA 

#comp | size | complexity | change_churn | total | minor | major | ownership | review_rate | self_reviews |review_churn_rate | too_quick | little_discussion | bugs
trainingDataSet <- read.csv("./data/qt50.csv", sep=",", header = TRUE)
testDataSet <- read.csv("./data/qt51.csv", sep=",", header = TRUE) 
trainData <- trainingDataSet[-1]
testData <- testDataSet[-1]
#Formatting our test data set

#trainData$bugs <- ifelse(trainData$bugs==0, 0,1)
#testData$bugs <- ifelse(testData$bugs==0, 0,1)
#trainData$bugs <- as.factor(trainData$bugs)
#testData$bugs <- as.factor(testData$bugs)
#trainData$bug <- trainData$bugs == 0
#trainData$bug <- trainData$bugs == 0

correlations <- cor(qt.training_data[-1], qt.training_data[-1] ,method = "spearman") #Only numeric data type


#Cleaning our highly correlated columns
trainData$review_churn_rate <- NULL #perfect correlation with "review_rate"

#DEFINING MODEL

#0. Setup a seed number
set.seed(12345)

#1. Selecting training features (columns used to predict)
selectedFeatures <- c("size","complexity","change_churn","total","minor","major","ownership", "review_rate","self_reviews","too_quick","little_discussion","bugs")
      
trainDataFilteredRows <- trainData[, selectedFeatures]

#Formula
modelFormula <- as.formula(bugs ~ . , selectedFeatures)
logRegressionModel <- glm(formula = modelFormula, data = trainDataFilteredRows, family = binomial("logit"))
#logRegressionModel <- train(bugs ~ . , data = trainDataFilteredRows, method="glm", family=binomial)
summary(logRegressionModel)

#Variable contributions
anova(logRegressionModel, test="Chisq")

#Try performing drop One to evaluate contribution
drop1(logRegressionModel, test="Chisq")


#VALIDATION -> Cross Validation

cv.binary(logRegressionModel, rand = NULL, nfolds = 10, print.details = TRUE)



#EVALUATING MODEL

testData$review_churn_rate <- NULL #perfect correlation with "review_rate"

logRegressionPrediction <- predict(logRegressionModel, type="response", newdata=testData)
#logRegressionPrediction <- predict(logRegressionModel, testData)

#RESULTS 
confMatrix <- table(logRegressionPrediction>=0.70, testData$bugs)
#conMatrix <- confusionMatrix(logRegressionPrediction, testData[,"bugs"])
accuracy <- (confMatrix[,"FALSE"]+confMatrix[,"TRUE"])/sum(confMatrix)
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



