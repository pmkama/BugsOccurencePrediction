library(DAAG)

#GETTING THE DATA

trainingDataSet <- read.csv("./data/qt50.csv", sep=",", header = TRUE)
testDataSet <- read.csv("./data/qt51.csv", sep=",", header = TRUE) 


#CLEANING DATA 

#1. Computing correlation

trainData <- trainingDataSet[-1] #Remove the fist column of String
trainData$bugs <- ifelse(trainData$bugs==0, 0, 1)
spearmanCorrelation <- cor(trainData[-1], trainData[-1], method = "spearman") #Spearman correlation

#Choice between highly correlated features

trainData$complexity <- NULL #size over complexity
trainData$review_churn_rate <- NULL #review_rates over review_churn_rate and too_quick
trainData$too_quick <- NULL
trainData$total <- NULL#Major over Total



#BUILD AND TRAIN THE MODEL

#2. Build model using chosen features

set.seed(122345)
selectedFeatures <- c("size","minor","major","ownership","review_rate") #Selecting features:  we start by choosing all of them and after three iteration we end up with these one


lgModelFormula <- as.formula(paste("bugs ~ ", paste(selectedFeatures, collapse = "+"))) #Creating the formula with the selected features for our model.

lgModel<-glm(lgModelFormula, family=binomial("logit"), data = trainData) # create the model with our train data set


#3. Assess contribution of each features :  *, **, and *** contribute significantly to prediction

  #3.1 Coeficient test with summary
summary(lgModel)
  #3.2 anova analysis
anova(lgModel)
  #3.3 Drop One test
drop1(lgModel)

#5. Evaluate the model and reimprove based on the characteristics we have
  #AIC of the model
AIC(lgModel)
  #K-folds Cross Validation
cv.binary(lgModel, rand=NULL, nfolds = 10, print.details = TRUE)



#TESTING THE MODEL 

#get the test data set
testData<- testDataSet

#run our model on test data set to predict
lgPrediction <- predict(lgModel, type="response", newdata = testData) 

#get the confusion matrix to be able to visualize results
confMatrix <- table(lgPrediction>0.5, testData$bugs) 

#compute accurracy
modelPredictionAccurray <- (confMatrix["FALSE", "0"]+confMatrix["TRUE", "1"])/sum(confMatrix) 

#print the accuracy value we just computed 
print(modelPredictionAccurray) # -> 0.955 :)









