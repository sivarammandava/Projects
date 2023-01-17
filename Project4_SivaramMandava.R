####################################
### Name: Sivaram Mandava
### Project 4
####################################

### Load Libraries
library(DMwR)
library(ggplot2)
library(DataExplorer)
library(caret)
library(dplyr)
library(usdm)
library(VIF)
library(VIM)
library(corrplot)
library(MASS)
library(tidyr)
library(ggplot2)
library(DataExplorer)
library(dplyr)
library(corrplot)
library(cluster)
library(fpc)
library(ggpubr)
library(factoextra)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
library(ineq)
library(InformationValue)
library(randomForest)
library(tree)
library(class)
library(e1071)
library(xgboost)
library(ipred)
library(plyr)
library(mlbench)
library(gbm)
library(adabag)


### Read from CSV
cars = read.csv("Cars-dataset.csv")

### Data Analysis and Data Clean Up
dim(cars)

str(cars)

summary(cars)

cars$Engineer = as.factor(cars$Engineer)
cars$MBA = as.factor(cars$MBA)
cars$Gender = as.factor(cars$Gender)
cars$Transport = as.factor(cars$Transport)
cars$license = as.factor(cars$license)

str(cars)

sapply(cars,function(x)sum(is.na(x)))

plot_missing(cars)

cars = knnImputation(cars)

plot_missing(cars)


### Exploratory Data Analysis

### Correlation Matrix

carDataNumeric = cars %>% select_if(is.numeric)
carCor = cor(carDataNumeric)
corrplot(carCor, method = "number") 

### Plots

hist(cars$Age, col = "blue")

hist(cars$Work.Exp, col = "yellow")

hist(cars$Salary, col = "orange")

hist(cars$Distance, col = "red")

transportNum = as.numeric(cars$Transport)

hist(transportNum, col = "purple")

licenseNum = as.numeric(cars$license)

hist(licenseNum, col = "green")

plot(cars$Transport, main = "Type of Transportation")

plot_density(cars)

create_report(
  data = cars,
  output_file = "carsReport.html",
  output_dir = getwd(),
  config = configure_report()
)

boxplot(cars$Age~cars$Engineer,
        xlab = "Engineer",
        ylab = "Age")

boxplot(cars$Age~cars$MBA,
        xlab = "MBA",
        ylab = "Age")

boxplot(cars$Salary~cars$Engineer,
        xlab = "Engineer",
        ylab = "Salary")

boxplot(cars$Salary~cars$MBA,
        xlab = "MBA",
        ylab = "Salary")

boxplot(cars$Salary~cars$Gender,
        xlab = "Gender",
        ylab = "Salary")

boxplot(cars$Work.Exp~cars$Gender,
        xlab = "Gender",
        ylab = "Work Exp")

boxplot(cars$Work.Exp~cars$Transport,
        xlab = "Transport",
        ylab = "Work Exp")

boxplot(cars$Salary~cars$Transport,
        xlab = "Transport",
        ylab = "Salary")

boxplot(cars$Salary~cars$license,
        xlab = "License",
        ylab = "Salary")

boxplot(cars$Distance~cars$license,
        xlab = "License",
        ylab = "Distance")

boxplot(cars$Distance~cars$Transport,
        xlab = "License",
        ylab = "Distance")

### Multicollinearity Check

str(cars)


cars$Engineer = as.numeric(cars$Engineer)
cars$MBA = as.numeric(cars$MBA)
cars$license = as.numeric(cars$license)


str(cars)

cars$Transport = ifelse(cars$Transport == "Car" ,1,0)

cars$Gender = ifelse(cars$Gender == "Female" ,1,0)

table(cars$Transport)

str(cars$Transport)

table(cars$Transport)

prop.table(table(cars$Transport))*100

vifcor(cars[-9])

str(cars)

cars$Age = as.numeric(cars$Age)
cars$Work.Exp = as.numeric(cars$Work.Exp)

### Check for outliers

boxplot(cars$Age, main = "Age plot", ylab = "Age", col = "Red")
boxplot(cars$Salary, main = "Salary plot", ylab = "Salary", col = "Brown")
boxplot(cars$Distance, main = "Distance plot", ylab = "Distance", col = "Blue")

### Remove Outliers For Age

quantile(cars$Age, c(0.95))
cars$Age[which(cars$Age>37)]=37


### Remove Outliers For Salary

quantile(cars$Salary, c(0.95))
cars$Age[which(cars$Salary>41.92)]=41.92

### Remove Outliers For Distance

quantile(cars$Distance, c(0.95))
cars$Distance[which(cars$Distance>17.92)]=17.92

table(cars$Transport)

### Balancing using SMOTE

cars$Transport = as.factor(cars$Transport)
str(cars)

seed = 1000
set.seed(seed)

carSmote = sample.split(cars$Transport, SplitRatio = 0.7)
carSmoteTrain = subset(cars, carSmote == TRUE)
carSmoteTest = subset(cars, carSmote == FALSE)

str(carSmoteTrain)

carSmoteTrain$Transport = as.factor(carSmoteTrain$Transport)

carSmoteBalTrain = SMOTE(carSmoteTrain$Transport ~., carSmoteTrain, perc.over =  450,
                    k = 5, perc.under = 125)
table(carSmoteBalTrain$Transport)

prop.table(table(carSmoteBalTrain$Transport))*100

### Logistic Regression

set.seed(seed)

carLR = sample.split(carSmoteBalTrain$Transport, SplitRatio = 0.67)
carLRTrain = subset(cars, carLR == TRUE)
carLRTest = subset(cars, carLR == FALSE)

carLRModel = glm(formula = carLRTrain$Transport ~., carLRTrain, family = binomial )
summary(carLRModel)

carLRModelNoAge = glm(formula = carLRTrain$Transport ~. -carLRTrain$Age, carLRTrain, family = binomial )
summary(carLRModelNoAge)

carLRModelNoAgeTest = glm(formula = carLRTest$Transport ~. -carLRTest$Age, carLRTest, family = binomial )
summary(carLRModelNoAgeTest)

### Model Performance of Logistic Regression

predictCarLRTrain = predict(carLRModelNoAge, newdata = carLRTrain[,-9], type = "response")
predictTableTrain = table(carLRTrain$Transport, predictCarLRTrain > 0.5) 
predictTableTrain

### Accuracy
sum(diag(predictTableTrain))/sum(predictTableTrain)*100

### Sensitivity
(predictTableTrain[2,2])/(predictTableTrain[2,2]+predictTableTrain[1,2])

### Specificity
(predictTableTrain[1,1])/(predictTableTrain[1,1]+predictTableTrain[2,1])

predictCarLRTest = predict(carLRModelNoAgeTest, newdata = carLRTest[,-9], type = "response")
predictTableTest = table(carLRTest$Transport, predictCarLRTest > 0.5) 
predictTableTest

### Accuracy
sum(diag(predictTableTest))/sum(predictTableTest)*100

### Sensitivity
(predictTableTest[2,2])/(predictTableTest[2,2]+predictTableTest[1,2])

### Specificity
(predictTableTest[1,1])/(predictTableTest[1,1]+predictTableTest[2,1])

remove(predictCarLRTest)

### ROC Curve for Train

rocPredictTrain = predict(carLRModelNoAge, newdata = carLRTrain)
predTrain = prediction(rocPredictTrain, carLRTrain$Transport)
perfTrain = performance(predTrain, "tpr", "fpr")
plot(perfTrain, colorize = T)

### ROC Curve for Test

rocPredictTest = predict(carLRModelNoAgeTest, newdata = carLRTest)
predTest = prediction(rocPredictTest, carLRTest$Transport)
perfTest = performance(predTest, "tpr", "fpr")
plot(perfTest, colorize = T)


### Check using AUC Performance

aucTrain = performance(predTrain,"auc"); 
aucTrain = as.numeric(aucTrain@y.values)

aucTest = performance(predTest,"auc"); 
aucTest = as.numeric(aucTest@y.values)

aucTrain
aucTest

### Check KS Performance

KSTrain = max(perfTrain@y.values[[1]]-perfTrain@x.values[[1]])
KSTest = max(perfTest@y.values[[1]]-perfTest@x.values[[1]])

KSTrain
KSTest


### KNN Classification

set.seed(seed)

dim(cars)

index = sample(418,250)

index

carsKNNTrain = cars[index,]

carsKNNTest = cars[-index,]

names(cars)

trainControl = trainControl(method = "cv", number = 10)

knnTrain = caret::train(Transport~.,
                        method = "knn",
                        tuneGrid = expand.grid(k = 1:15),
                        trControl = trainControl,
                        metric = "Accuracy",
                        data = carLRTrain)

knnTrain

knnTrainPredict = predict(knnTrain, carLRTest)
knnTrainPredict

knnTable = table(carLRTest$Transport, knnTrainPredict)

knnTable

### Accuracy
sum(diag(knnTable))/sum(knnTable)*100

### Sensitivity
(knnTable[2,2])/(knnTable[2,2]+knnTable[1,2])

### Specificity
(knnTable[1,1])/(knnTable[1,1]+knnTable[2,1])

knnTableTest = table(carLRTest$Transport, knnTrainPredict)

### Accuracy
sum(diag(knnTableTest))/sum(knnTableTest)*100

### Sensitivity
(knnTableTest[2,2])/(knnTableTest[2,2]+knnTableTest[1,2])

### Specificity
(knnTableTest[1,1])/(knnTableTest[1,1]+knnTableTest[2,1])

### Naive Bayes

nbTrain = naiveBayes(Transport~., data = carLRTrain)
nbTrain

predNBTrain = predict(nbTrain, newdata = carLRTrain, type = "class")
nbTrainTable  = table(carLRTrain$Transport, predNBTrain)
nbTrainTable

nbPredict = predict(nbTrain, carLRTest)

nbPredictTable = table(carLRTest$Transport, nbPredict)
nbPredictTable

### Accuracy
sum(diag(nbPredictTable))/sum(nbPredictTable)*100

### Sensitivity
(nbPredictTable[2,2])/(nbPredictTable[2,2]+nbPredictTable[1,2])

### Specificity
(nbPredictTable[1,1])/(nbPredictTable[1,1]+nbPredictTable[2,1])

### Bagging

baggingModel = bagging(Transport~., data = carLRTrain)

bagPredict = predict(baggingModel, carLRTest)

bagPredictTable = table(carLRTest$Transport, bagPredict)
bagPredictTable

### Accuracy
sum(diag(bagPredictTable))/sum(bagPredictTable)*100

### Sensitivity
(bagPredictTable[2,2])/(bagPredictTable[2,2]+bagPredictTable[1,2])

### Specificity
(bagPredictTable[1,1])/(bagPredictTable[1,1]+bagPredictTable[2,1])


### Boosting

dataTrain = as.matrix(carLRTrain[,1:8])
transpotTrain = as.matrix(carLRTrain[,9])
dataTest = as.matrix(carLRTest[,1:8])

boostingModel = xgboost(data = dataTrain,
                        label = transpotTrain,
                        nrounds = 2000)

boostPredict = predict(boostingModel, newdata = dataTest)

boostPredictTable = table(carLRTest$Transport, boostPredict > 0.5)
boostPredictTable

### Accuracy
sum(diag(boostPredictTable))/sum(boostPredictTable)*100

### Sensitivity
(boostPredictTable[2,2])/(boostPredictTable[2,2]+boostPredictTable[1,2])

### Specificity
(boostPredictTable[1,1])/(boostPredictTable[1,1]+boostPredictTable[2,1])

