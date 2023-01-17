####################################
### Name: Sivaram Mandava
### Project 2
####################################

### Set Working Directory
setwd("~/Documents/UT/M2 Statistics/Week 4/Project 2")

### Import and Use readr package
install.packages("readr")
library("readr")

#########
### Problem 1
#########

### Read from CSV
tempData = read.csv("Cold_Storage_Temp_Data.csv")

### Find out Names of the Columns
names(tempData)

### Check Summary
summary(tempData)

### Change class of columns to factors
tempData$Season = as.factor(tempData$Season)
tempData$Month = as.factor(tempData$Month)

### Check Summary
summary(tempData)

### Find mean cold storage temperature for Summer, Winter and Rainy Season
meanRainyTemp = mean(tempData$Temperature[tempData$Season == "Rainy"])
meanWinterTemp = mean(tempData$Temperature[tempData$Season == "Winter"])
meanSummerTemp = mean(tempData$Temperature[tempData$Season == "Summer"])

### Mean temperature for Rainy Season
print(meanRainyTemp)
### Mean temperature for Winter Season
print(meanWinterTemp)
### Mean temperature for Summer Season
print(meanSummerTemp)

### Combine means into a single Dataframe
meanSeasonTemps = data.frame(meanRainyTemp, meanWinterTemp, meanSummerTemp)


### Display all season means
print(meanSeasonTemps)

### import libraries required to plot graphs
library("ggplot2")

TempGraph = hist(tempData$Temperature,
                     xlab = "Temperature",
                     main = "Temperature Distribution")
TempGraph

tempBoxplot = boxplot(tempData$Temperature, main = "Temperature Boxplot", ylab = "Temperature")


### Find overall mean for the full year
meanYearTemp = mean(tempData$Temperature)

### Display overall mean
print(meanYearTemp)

### Find Standard Deviation for the full year
SDYearTemp = sd(tempData$Temperature)

### Display Standard Deviation in temperature
print(SDYearTemp)

### Assume Normal distribution, what is the probability of temperature having fallen below 2 C
probLessThan2 = pnorm(2, mean = meanYearTemp, sd = SDYearTemp)
print(probLessThan2)

### Assume Normal distribution, what is the probability of temperature having gone above 4 C
probMoreThan4 = 1 - pnorm(4, mean = meanYearTemp, sd = SDYearTemp)
print(probMoreThan4)

### What will be the penalty for the AMC Company
totalProb = probLessThan2 + probMoreThan4
print(totalProb)
print(totalProb*100)

#########
### Since the probability of temperature going outside the 2 - 4 C during the 
### one-year contract was above 2.5% and less than 5% then the penalty would be 10%
#########

### Perform a one-way ANOVA test to determine if there is a significant 
### difference in Cold Storage temperature between rainy, summer and winter 
### seasons and comment on the findings.
anovatemp = aov(tempData$Temperature~tempData$Season)
anovatemp
summary(anovatemp)

### Performing the TukeyHSD test to get the p adj values
TukeyHSD(anovatemp)

#########
### Problem 2
#########

### Read from CSV
tempDataMarch = read.csv("Cold_Storage_Mar2018.csv")

### Find out Names of the Columns
names(tempDataMarch)

### Check Summary
summary(tempDataMarch)

### Change class of columns to factors
tempDataMarch$Season = as.factor(tempDataMarch$Season)
tempDataMarch$Month = as.factor(tempDataMarch$Month)

### Histogram of Temperature Distribution
marchHist = hist(tempDataMarch$Temperature, 
                 xlab = "Temperature",
                 main = "Temperature Distribution")
marchHist

### State the Hypothesis, perform hypothesis test and determine p-value

### Using z-test

### Get Sample Mean
sampleMean = mean(tempDataMarch$Temperature)
print(sampleMean)

### Get Standard Deviation
sampleSD = sd(tempDataMarch$Temperature)
print(sampleSD)

### Get Standard Error
stdError = sampleSD/sqrt(35)
print(stdError)

mu = 3.9

### Get Z Test Value
zTestValue = (sampleMean - mu) / stdError
print(zTestValue)


### Get Critical Z value
qnorm(0.1)

### Using t-test

### Perform T Test
tTestValue = t.test(tempDataMarch$Temperature,
       alternative = "greater",
       mu = 3.9, paired = FALSE, var.equal = FALSE,
       conf.level = 0.9)
tTestValue

tValue = 2.7524
df = 34
pValue=0.004711
