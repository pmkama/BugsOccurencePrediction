library(DAAG)
library(caret)

# IMPORT DATA 

#comp | size | complexity | change_churn | total | minor | major | ownership | review_rate | self_reviews |review_churn_rate | too_quick | little_discussion | bugs
trainingDataSet <- read.csv("./data/qt50.csv", sep=",", header = TRUE)
testDataSet <- read.csv("./data/qt51.csv", sep=",", header = TRUE) 

#Formatting our test data set
trainData <- trainingDataSet
trainData$bugs <- ifelse(trainData$bugs==0, 0,1)
trainData$bugs <- as.factor(trainData$bugs)
#trainData$bug <- trainData$bugs == 0
#trainData$bug <- trainData$bugs == 0

correlations <- cor(qt.training_data[-1], qt.training_data[-1]) #Only numeric data type

#Cleaning our highly correlated columns
trainData$review_churn_rate <- NULL #perfect correlation with "review_rate"

#DEFINING MODEL

#0. Setup a seed number
set.seed(12345)

#1. Selecting training features (columns used to predict)
selectedFeatures <- c("size","complexity","change_churn","minor","major","ownership","review_rate","self_reviews","too_quick","total","comp","little_discussion","bugs")
trainDataFilteredRows <- trainData[, selectedFeatures]

#Formula
modelFormula <- as.formula(paste("bugs ~", paste(selectedFeatures, collapse = "+")))
logRegressionModel <- glm(bugs ~., data = trainDataFilteredRows, family = binomial("logit"))
summary(logRegressionModel)

#Variable contributions
anova(logRegressionModel, test="Chisq")

#Try performing drop One to evaluate contribution
drop1(logRegressionModel, test="Chisq")


#VALIDATION -> Cross Validation

cv.binary(logRegressionModel, rand = NULL, nfolds = 10, print.details = TRUE)



#EVALUATING MODEL
testData <- testDataSet
logRegressionPrediction <- predict(logRegressionModel, type = "response", newdata="testData")

#RESULTS 
confMatrix <- confusionMatrix(logRegressionPrediction, testData[, "bugs"])

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



