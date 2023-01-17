####################################
### Name: Sivaram Mandava
### Project 3
####################################

### Load Libraries
library(readxl)
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

library(devtools)
devtools::install_github('araastat/reprtree')
library(reprtree)

### Set Working Directory
setwd("~/Documents/UT/M3 Machine Learning/Project 3")

### Read from CSV
bankLoanData = read_excel("Thera Bank_Personal_Loan_Modelling-dataset-1.xlsx", sheet = 2)
bankDataDefiniton = read_excel("Thera Bank_Personal_Loan_Modelling-dataset-1.xlsx", sheet = 1)

### Data Analysis and Data Clean Up
str(bankLoanData)

dim(bankLoanData)

summary(bankLoanData)


### Make Column Names readable
colnames(bankLoanData) = make.names(colnames(bankLoanData))

### Change class of columns to factors
bankLoanData$ID = as.factor(bankLoanData$ID)
bankLoanData$ZIP.Code = as.factor(bankLoanData$ZIP.Code)
bankLoanData$Education = as.factor(bankLoanData$Education)
bankLoanData$Personal.Loan = as.factor(bankLoanData$Personal.Loan)
bankLoanData$Securities.Account = as.factor(bankLoanData$Securities.Account)
bankLoanData$CD.Account = as.factor(bankLoanData$CD.Account)
bankLoanData$Online = as.factor(bankLoanData$Online)
bankLoanData$CreditCard = as.factor(bankLoanData$CreditCard)

summary(bankLoanData)

### Drop Rows that conaitn NA

containsNA = apply(bankLoanData, 1, function(x){
                                                  any(is.na(x))
                                                })
sum(containsNA)

bankLoanData = bankLoanData %>% drop_na()

summary(bankLoanData)

### Exploratory Data Analysis

plot_density(bankLoanData)

create_report(
  data = bankLoanData,
  output_file = "BankLoanDataReport.html",
  output_dir = getwd(),
  config = configure_report()
)

boxplot(bankLoanData %>% select_if(is.numeric))

plot_bar(bankLoanData %>% select_if(is.factor))

loanAgePlot = ggplot(bankLoanData, aes(bankLoanData$Age..in.years., fill= bankLoanData$Personal.Loan)) + geom_bar(alpha=0.5)
loanExpPlot = ggplot(bankLoanData, aes(bankLoanData$Experience..in.years., fill= bankLoanData$Personal.Loan)) + geom_bar(alpha=0.5)
loanIncomePlot = ggplot(bankLoanData, aes(bankLoanData$Income..in.K.month., fill= bankLoanData$Personal.Loan)) + geom_bar(alpha=0.5)
loanFamilyPlot = ggplot(bankLoanData, aes(bankLoanData$Family.members, fill= bankLoanData$Personal.Loan)) + geom_bar(alpha=0.5)
loanCCPlot = ggplot(bankLoanData, aes(bankLoanData$CCAvg, fill= bankLoanData$Personal.Loan)) + geom_histogram(alpha=0.5, bins = 50)
loanEducationPlot = ggplot(bankLoanData, aes(bankLoanData$Education, fill= bankLoanData$Personal.Loan)) + geom_bar(alpha=0.5)
loanMortgagePlot = ggplot(bankLoanData, aes(bankLoanData$Mortgage, fill= bankLoanData$Personal.Loan)) + geom_density(alpha=0.5)
loanSecuritiesPlot = ggplot(bankLoanData, aes(bankLoanData$Securities.Account, fill= bankLoanData$Personal.Loan)) + geom_bar(alpha=0.5)
loanCDPlot = ggplot(bankLoanData, aes(bankLoanData$CD.Account, fill= bankLoanData$Personal.Loan)) + geom_bar(alpha=0.5)
loanOnlinePlot = ggplot(bankLoanData, aes(bankLoanData$Online, fill= bankLoanData$Personal.Loan)) + geom_bar(alpha=0.5)
LoanCreditPlot = ggplot(bankLoanData, aes(bankLoanData$CreditCard, fill= bankLoanData$Personal.Loan)) + geom_bar(alpha=0.5)

ggarrange(loanAgePlot, loanExpPlot, loanIncomePlot, ncol = 3)
ggarrange(loanFamilyPlot, loanCCPlot, loanEducationPlot, ncol = 3)
ggarrange(loanMortgagePlot, loanSecuritiesPlot, loanCDPlot, ncol = 3)
ggarrange(loanOnlinePlot, LoanCreditPlot, ncol = 2)

### Additional Data CLean Up

bankLoanData = bankLoanData[,-c(1,5)]

### Corelation Matrix
bankDataNumeric = bankLoanData %>% select_if(is.numeric)
bankCor = cor(bankDataNumeric)
corrplot(bankCor, method = "number") 

### CLustering

bankDataScaled = scale(bankDataNumeric, center = TRUE) 
bankDataNumeric = dist(bankDataScaled, method = "euclidean")


seed=1000
set.seed(seed)

totWss=rep(0,5)
for(k in 1:5){
  set.seed(seed)
  clust=kmeans(x=bankDataScaled, centers=k, nstart=5)
  totWss[k]=clust$tot.withinss
}
plot(c(1:5), totWss, type="b", xlab="Number of Clusters",
     ylab="sum of 'Within groups sum of squares'")  

bankDataCluster = kmeans(bankDataScaled, 3)
bankDataCluster
bankDataClusterPlot = clusplot(bankDataScaled, bankDataCluster$cluster, 
                               color = TRUE, shade = TRUE, labels = 6,
                               lines = 1)

### Plot where Cluster is 1
plot(bankDataScaled[bankDataCluster$cluster == 1], col = "red")

### Plot where Cluster is 2
plot(bankDataScaled[bankDataCluster$cluster == 2], col = "cyan") 

### Plot where Cluster is 3
plot(bankDataScaled[bankDataCluster$cluster == 3], col = "purple")

### All 3 combined
plot(bankDataScaled[bankDataCluster$cluster == 1], col = "red")
points(bankDataScaled[bankDataCluster$cluster == 2], col = "cyan") 
points(bankDataScaled[bankDataCluster$cluster == 3], col = "purple")

### Verifying if our clustering is accurate
fit = kmeans(bankDataScaled, 3)
plotcluster(bankDataScaled, fit$cluster)

fviz_cluster(bankDataCluster, data = bankDataScaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

### Split into test and train
set.seed(seed)
bankDataSplit = sample.split(bankLoanData$Personal.Loan, SplitRatio = 0.65)
bankDataTrain = subset(bankLoanData, bankDataSplit == TRUE)
bankDataTest = subset(bankLoanData, bankDataSplit == FALSE)

### Check Split Data
dim(bankDataTrain)
dim(bankDataTest)

### CART Model Train
bankCARTModelTrain = rpart(formula = Personal.Loan ~ ., 
              data = bankDataTrain, method = "class", cp=0.0, minbucket=2)
bankCARTModelTrain
rpart.plot(bankCARTModelTrain)

### Determine the CP for Pruning Train
printcp(bankCARTModelTrain)
plotcp(bankCARTModelTrain)

### Prune Train
bankCARTModelTrain = prune(bankCARTModelTrain, cp = 0.008) 
rpart.plot(bankCARTModelTrain)

printcp(bankCARTModelTrain)
plotcp(bankCARTModelTrain)

data.frame(bankCARTModelTrain$variable.importance)

### CART Model Test
bankCARTModelTest = rpart(formula = Personal.Loan ~ ., 
                           data = bankDataTest, method = "class", cp=0.0, minbucket=2)
bankCARTModelTest
rpart.plot(bankCARTModelTest)

### Determine the CP for Pruning Test
printcp(bankCARTModelTest)
plotcp(bankCARTModelTest)

### Prune Test
bankCARTModelTest = prune(bankCARTModelTest, cp = 0.09) 
rpart.plot(bankCARTModelTest)

printcp(bankCARTModelTest)
plotcp(bankCARTModelTest)

### Consfusion Matrix Train for CART
bankPredictTrain=predict(bankCARTModelTrain,newdata=bankDataTrain,type = "class")
bankProbTrain=predict(bankCARTModelTrain,newdata=bankDataTrain,type = "prob")[,"1"]

tablePredictTrain=table(bankDataTrain$Personal.Loan,bankPredictTrain)
tableProbTrain=table(bankDataTrain$Personal.Loan,bankProbTrain)
bankPredictTrain
tablePredictTrain

### Accuracy
sum(diag(tablePredictTrain))/sum(tablePredictTrain)
(tablePredictTrain[1,1]+tablePredictTrain[2,2])/nrow(bankDataTrain)

### Sensitivity
(tablePredictTrain[2,2])/(tablePredictTrain[2,2]+tablePredictTrain[1,2])

### Specificity
(tablePredictTrain[1,1])/(tablePredictTrain[1,1]+tablePredictTrain[2,1])

### Model Performance
bankPerformDataTrain = bankDataTrain
bankPerformDataTrain$Predict = predict(bankCARTModelTrain,data=bankPerformDataTrain,type="class")
bankPerformDataTrain$Prob.Predict = predict(bankCARTModelTrain,data=bankPerformDataTrain,type="prob")[,"1"]

predObjTrain = prediction(bankPerformDataTrain$Prob.Predict, bankPerformDataTrain$Personal.Loan)

perfROCTrain = performance(predObjTrain, "tpr", "fpr")
plot(perfROCTrain)

KSTrain = max(perfROCTrain@y.values[[1]]-perfROCTrain@x.values[[1]])
aucTrain = performance(predObjTrain,"auc"); 
aucTrain = as.numeric(aucTrain@y.values)

giniTrain = ineq(bankPerformDataTrain$Prob.Predict, type="Gini")

KSTrain
aucTrain
giniTrain

Concordance(actuals = bankPerformDataTrain$Personal.Loan, predictedScores = bankPerformDataTrain$Prob.Predict)


### Confusin Matrix For test
bankPredictTest=predict(bankCARTModelTest,newdata=bankDataTest,type = "class")
bankProbTest=predict(bankCARTModelTest,newdata=bankDataTest,type = "prob")[,"1"]

tablePredictTest=table(bankDataTest$Personal.Loan,bankPredictTest)
tableProbTest=table(bankDataTest$Personal.Loan,bankProbTest)
bankPredictTest
tablePredictTest

### Accuracy
sum(diag(tablePredictTest))/sum(tablePredictTest)
(tablePredictTest[1,1]+tablePredictTest[2,2])/nrow(bankDataTest)

### Sensitivity
(tablePredictTest[2,2])/(tablePredictTest[2,2]+tablePredictTest[1,2])

### Specificity
(tablePredictTest[1,1])/(tablePredictTest[1,1]+tablePredictTest[2,1])

### Model Performance
bankPerformDataTest = bankDataTest
bankPerformDataTest$Predict = predict(bankCARTModelTest,data=bankPerformDataTest,type="class")
bankPerformDataTest$Prob.Predict = predict(bankCARTModelTest,data=bankPerformDataTest,type="prob")[,"1"]

predObjTest = prediction(bankPerformDataTest$Prob.Predict, bankPerformDataTest$Personal.Loan)

perfROCTest = performance(predObjTest, "tpr", "fpr")
plot(perfROCTest)

KSTest = max(perfROCTest@y.values[[1]]-perfROCTest@x.values[[1]])
aucTest = performance(predObjTest,"auc"); 
aucTest = as.numeric(aucTest@y.values)

giniTest = ineq(bankPerformDataTest$Prob.Predict, type="Gini")

KSTest
aucTest
giniTest

Concordance(actuals = bankPerformDataTest$Personal.Loan, predictedScores = bankPerformDataTest$Prob.Predict)


### Random Forest

bankDataRFTrain = subset(bankLoanData, bankDataSplit == TRUE)
bankDataRFTest = subset(bankLoanData, bankDataSplit == FALSE) 
bankDataRFTuneTest = subset(bankLoanData, bankDataSplit == FALSE)

set.seed(seed)
bankRFModel = randomForest(Personal.Loan~., data = bankDataRFTrain)
print(bankRFModel)

plot(bankRFModel)
varImpPlot(bankRFModel)
importance(bankRFModel) 

### Display the RF
model <- randomForest(Personal.Loan ~ ., data=bankDataRFTrain, importance=TRUE, ntree=50, mtry = 2, do.trace=10)

reprtree:::plot.getTree(model)

set.seed(seed)

tRndFor = tuneRF(bankDataRFTrain[,-c(8)],
                 bankDataRFTrain$Personal.Loan,
                 mtryStart = 3, 
                 ntreeTry = 51, 
                 stepFactor = 1.5, 
                 improve = 0.0001, 
                 trace=TRUE, 
                 plot = TRUE,
                 doBest = TRUE,
                 nodesize = 10, 
                 importance=TRUE
)
importance(tRndFor)

set.seed(seed)

bankRFTuneModelTrain = randomForest(Personal.Loan~., data = bankDataRFTrain,
                               ntree = 51, mtry = 3,
                              )

print(bankRFTuneModelTrain)

bankRFTuneModelTest = randomForest(Personal.Loan~., data = bankDataRFTest,
                                    ntree = 51, mtry = 3,
)

print(bankRFTuneModelTest)


### Confusion Matrix Train for RF

bankPredictRFTrain=predict(bankRFTuneModelTrain,newdata=bankDataRFTrain,type = "class")
bankProbRFTrain=predict(bankRFTuneModelTrain,newdata=bankDataRFTrain,type = "prob")[,"1"]

tablePredictRFTrain=table(bankDataRFTrain$Personal.Loan,bankPredictRFTrain)
tableProbRFTrain=table(bankDataRFTrain$Personal.Loan,bankProbRFTrain)
bankPredictRFTrain
tablePredictRFTrain

### Accuracy
sum(diag(tablePredictRFTrain))/sum(tablePredictRFTrain)
(tablePredictRFTrain[1,1]+tablePredictRFTrain[2,2])/nrow(bankDataRFTrain)

### Sensitivity
(tablePredictRFTrain[2,2])/(tablePredictRFTrain[2,2]+tablePredictRFTrain[1,2])

### Specificity
(tablePredictRFTrain[1,1])/(tablePredictRFTrain[1,1]+tablePredictRFTrain[2,1])

### Model Performance
bankPerformDataRFTrain = bankDataRFTrain
bankPerformDataRFTrain$Predict = predict(bankRFTuneModelTrain,data=bankPerformDataRFTrain,type="class")
bankPerformDataRFTrain$Prob.Predict = predict(bankRFTuneModelTrain,data=bankPerformDataRFTrain,type="prob")[,"1"]

predObjRFTrain = prediction(bankPerformDataRFTrain$Prob.Predict, bankPerformDataRFTrain$Personal.Loan)

perfROCRFTrain = performance(predObjRFTrain, "tpr", "fpr")
plot(perfROCRFTrain)

KSRFTrain = max(perfROCRFTrain@y.values[[1]]-perfROCRFTrain@x.values[[1]])
aucRFTrain = performance(predObjRFTrain,"auc"); 
aucRFTrain = as.numeric(aucRFTrain@y.values)

giniRFTrain = ineq(bankPerformDataRFTrain$Prob.Predict, type="Gini")

KSRFTrain
aucRFTrain
giniRFTrain

Concordance(actuals = bankPerformDataRFTrain$Personal.Loan, predictedScores = bankPerformDataRFTrain$Prob.Predict)


### Confusion Matrix Test for RF

bankPredictRFTest=predict(bankRFTuneModelTest,newdata=bankDataRFTest,type = "class")
bankProbRFTest=predict(bankRFTuneModelTest,newdata=bankDataRFTest,type = "prob")[,"1"]

tablePredictRFTest=table(bankDataRFTest$Personal.Loan,bankPredictRFTest)
tableProbRFTest=table(bankDataRFTest$Personal.Loan,bankProbRFTest)
bankPredictRFTest
tablePredictRFTest

### Accuracy
sum(diag(tablePredictRFTest))/sum(tablePredictRFTest)
(tablePredictRFTest[1,1]+tablePredictRFTest[2,2])/nrow(bankDataRFTest)

### Sensitivity
(tablePredictRFTest[2,2])/(tablePredictRFTest[2,2]+tablePredictRFTest[1,2])

### Specificity
(tablePredictRFTest[1,1])/(tablePredictRFTest[1,1]+tablePredictRFTest[2,1])

### Model Performance
bankPerformDataRFTest = bankDataRFTest
bankPerformDataRFTest$Predict = predict(bankRFTuneModelTest,data=bankPerformDataRFTest,type="class")
bankPerformDataRFTest$Prob.Predict = predict(bankRFTuneModelTest,data=bankPerformDataRFTest,type="prob")[,"1"]

predObjRFTest = prediction(bankPerformDataRFTest$Prob.Predict, bankPerformDataRFTest$Personal.Loan)

perfROCRFTest = performance(predObjRFTest, "tpr", "fpr")
plot(perfROCRFTest)

KSRFTest = max(perfROCRFTest@y.values[[1]]-perfROCRFTest@x.values[[1]])
aucRFTest = performance(predObjRFTest,"auc"); 
aucRFTest = as.numeric(aucRFTest@y.values)

giniRFTest = ineq(bankPerformDataRFTest$Prob.Predict, type="Gini")

KSRFTest
aucRFTest
giniRFTest

Concordance(actuals = bankPerformDataRFTest$Personal.Loan, predictedScores = bankPerformDataRFTest$Prob.Predict)


