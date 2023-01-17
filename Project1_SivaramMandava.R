####################################
### Name: Sivaram Mandava
### Project 1
####################################

### Set Working Directory
setwd("~/Documents/UT/M1 Intro To R/Week 4/Project 1")

### Import and Use readr package
install.packages("readr")
library("readr")

### Read from CSV
cardioDataset = read_csv("CardioGoodFitness.csv")

### Find out Names of the Columns
names(cardioDataset)

### Check Summary
summary(cardioDataset)

### Check max values of columns
max(cardioDataset$Age)
max(cardioDataset$Education)
max(cardioDataset$Usage)
max(cardioDataset$Fitness)
max(cardioDataset$Income)
max(cardioDataset$Miles)

### Check min values of columns
min(cardioDataset$Age)
min(cardioDataset$Education)
min(cardioDataset$Usage)
min(cardioDataset$Fitness)
min(cardioDataset$Income)
min(cardioDataset$Miles)

### Change class of all columns to factors
cardioDataset$Product = as.factor(cardioDataset$Product)
cardioDataset$Gender = as.factor(cardioDataset$Gender)
cardioDataset$MaritalStatus = as.factor(cardioDataset$MaritalStatus)
cardioDataset$Education = as.factor(cardioDataset$Education)
cardioDataset$Usage = as.factor(cardioDataset$Usage)
cardioDataset$Fitness = as.factor(cardioDataset$Fitness)

### Check Summary to make sure all columns are factors
summary(cardioDataset)

### Check internal structure
str(cardioDataset)

### Look at the dataset
View(cardioDataset)

### Check the dimension of the dataset
dim(cardioDataset)

### Sort all information based on Product
Data195 = cardioDataset[cardioDataset$Product == "TM195",]
Data498 = cardioDataset[cardioDataset$Product == "TM498",]
Data798 = cardioDataset[cardioDataset$Product == "TM798",]

### Check Summary of newly created dataframes
summary(Data195)
summary(Data498)
summary(Data798)

### import libraries required to plot graphs
library("ggplot2")

### Univariate Analysis

### Creation of Graph to see product distribution
productDistribution = qplot(Product, data = cardioDataset,
                            main = "Product Distribution",
                            xlab = "Products",
                            ylab = "Frequency"
)

### Plotting the above Graph
productDistribution

### Creation of Graph to see Age distribution
ageDistribution = qplot(Age, data = cardioDataset, binwidth = 5, boundary = 15,
                            main = "Age Distribution",
                            xlab = "Age",
                            ylab = "Frequency"
)

ageDistributionBoxplot = qplot(Age, data = cardioDataset, geom = "boxplot",
                        main = "Age Distribution",
                        xlab = "Age",
                        ylab = "Frequency"
)

### Plotting the above Graph
ageDistribution
ageDistributionBoxplot + coord_flip()

### Creation of Graph to see Gender distribution
genderDistribution = qplot(Gender, data = cardioDataset,
                            main = "Gender Distribution",
                            xlab = "Gender",
                            ylab = "Frequency"
)

### Plotting the above Graph
genderDistribution

### Creation of Graph to see Education distribution
educationDistribution = qplot(Education, data = cardioDataset,
                           main = "Education Distribution",
                           xlab = "Education",
                           ylab = "Frequency"
)

### Plotting the above Graph
educationDistribution

### Creation of Graph to see Marital Status distribution
maritalStatusDistribution = qplot(MaritalStatus, data = cardioDataset,
                              main = "Marital Status Distribution",
                              xlab = "Marital Status",
                              ylab = "Frequency"
)

### Plotting the above Graph
maritalStatusDistribution

### Creation of Graph to see Usage distribution
usageDistribution = qplot(Usage, data = cardioDataset,
                              main = "Usage Distribution",
                              xlab = "Usage",
                              ylab = "Frequency"
)

### Plotting the above Graph
usageDistribution

### Creation of Graph to see Usage distribution
fitnessDistribution = qplot(Fitness, data = cardioDataset,
                          main = "Fitness Distribution",
                          xlab = "Fitness",
                          ylab = "Frequency"
)

### Plotting the above Graph
fitnessDistribution

### Creation of Graph to see Fitness distribution
incomeDistribution = qplot(Income, data = cardioDataset, binwidth = 10000, boundary = 10000,
                          main = "Income Distribution",
                          xlab = "Income",
                          ylab = "Frequency"
)

incomeDistributionBoxplot = qplot(Income, data = cardioDataset, geom = "boxplot",
                           main = "Income Distribution",
                           xlab = "Income",
                           ylab = "Frequency"
)

### Plotting the above Graph
incomeDistribution 
incomeDistributionBoxplot + coord_flip()

### Creation of Graph to see Miles distribution
milesDistribution = qplot(Miles, data = cardioDataset, binwidth = 50, boundary = 0,
                           main = "Miles Distribution",
                           xlab = "Miles",
                           ylab = "Frequency"
)

milesDistributionBoxplot = qplot(Miles, data = cardioDataset, geom = "boxplot",
                          main = "Miles Distribution",
                          xlab = "Miles",
                          ylab = "Frequency"
)

### Plotting the above Graph
milesDistribution
milesDistributionBoxplot + coord_flip()

### Usage Data for TM195
TM195Usage = qplot(Usage, data = Data195,
                   main = "Usage Distribution 195",
                   xlab = "Usage",
                   ylab = "Count"
)

### Plotting the above Histogram
TM195Usage

### Usage Data for TM498
TM498Usage = qplot(Usage, data = Data498,
                   main = "Usage Distribution 498",
                   xlab = "Usage",
                   ylab = "Count"
)


### Plotting the above Histogram
TM498Usage

### Usage Data for TM798
TM798Usage = qplot(Usage, data = Data798,
                   main = "Usage Distribution 798",
                   xlab = "Usage",
                   ylab = "Count"
)


### Plotting the above Histogram
TM798Usage

### Use ggpubr
library(ggpubr)

### Display Usage of each Product together
ggarrange(TM195Usage, TM498Usage, TM798Usage, 
          ncol = 3)

### Boxplot to Find Relation Between Income and Usage for TM195    
usageIncomeTM195Boxplot = qplot(Usage, Income, data = Data195, geom = "boxplot",
      main = "Income/Usage TM195",
      xlab = "Usage",
      ylab = "Income")  

### Boxplot to Find Relation Between Income and Usage for TM498
usageIncomeTM498Boxplot = qplot(Usage, Income, data = Data498, geom = "boxplot",
      main = "Income/Usage TM498",
      xlab = "Usage",
      ylab = "Income")

### Boxplot to Find Relation Between Income and Usage for TM798
usageIncomeTM798Boxplot = qplot(Usage, Income, data = Data798, geom = "boxplot",
      main = "Income/Usage TM798",
      xlab = "Usage",
      ylab = "Income")

### Display above created Boxplots
usageIncomeTM195Boxplot
usageIncomeTM498Boxplot
usageIncomeTM798Boxplot

### Display Relation Between Income and Usage for all products together
ggarrange(usageIncomeTM195Boxplot, usageIncomeTM498Boxplot, usageIncomeTM798Boxplot,
          ncol = 3)

### Barplot for Relation Between Gender and Fitness of TM195
fitnessGenderTM195Barplot = qplot(Fitness,fill=Gender,data = Data195, geom = "bar",
      main = "Gender vs Fitness of TM195",
      xlab = "Fitness",
      ylab = "Count")

### Barplot for Relation Between Gender and Fitness of TM498
fitnessGenderTM498Barplot = qplot(Fitness,fill=Gender,data = Data498, geom = "bar",
                                  main = "Gender vs Fitness of TM498",
                                  xlab = "Fitness",
                                  ylab = "Count")

### Barplot for Relation Between Gender and Fitness of TM798
fitnessGenderTM798Barplot = qplot(Fitness,fill=Gender,data = Data798, geom = "bar",
                                  main = "Gender vs Fitness of TM798",
                                  xlab = "Fitness",
                                  ylab = "Count")

### Display above created Barplots
fitnessGenderTM195Barplot
fitnessGenderTM498Barplot
fitnessGenderTM798Barplot

### Display Relation Between Gender and Fitness for all products together
ggarrange(fitnessGenderTM195Barplot, fitnessGenderTM498Barplot, fitnessGenderTM798Barplot,
          ncol = 3)


### Display Relation Between Fitness and Age for TM195
fitnessAgeTM195Histogram = qplot(Age,fill=Fitness, data = Data195,geom="histogram", binwidth = 5, boundary = 15,
      main = "Relation Between Fitness and Age of TM195",
      xlab = "Age",
      ylab = "Count")

### Display Relation Between Fitness and Age for TM498
fitnessAgeTM498Histogram = qplot(Age,fill=Fitness, data = Data498,geom="histogram", binwidth = 5, boundary = 15,
      main = "Relation Between Fitness and Age of TM498",
      xlab = "Age",
      ylab = "Count")

### Display Relation Between Fitness and Age for TM798
fitnessAgeTM798Histogram = qplot(Age,fill=Fitness, data = Data798,geom="histogram", binwidth = 5, boundary = 15,
      main = "Relation Between Fitness and Age of TM798",
      xlab = "Age",
      ylab = "Count")

### Display above created Histomgrams
fitnessAgeTM195Histogram
fitnessAgeTM498Histogram
fitnessAgeTM798Histogram

### Display Relation Between Miles and Usage for TM195
milesUsageTM195Barplot = qplot(Miles,fill=Usage, data = Data195, binwidth = 10, boundary = 30,
                                main = "Miles vs Usage of TM195",
                                xlab = "Miles",
                                ylab = "Frequency")

### Display Relation Between Miles and Usage for TM498
milesUsageTM498Barplot = qplot(Miles,fill=Usage, data = Data498, binwidth = 10, boundary = 30,
                               main = "Miles vs Usage of TM498",
                               xlab = "Miles",
                               ylab = "Frequency")

### Display Relation Between Miles and Usage for TM798
milesUsageTM798Barplot = qplot(Miles,fill=Usage, data = Data798, binwidth = 10, boundary = 30,
                               main = "Miles vs Usage of TM798",
                               xlab = "Miles",
                               ylab = "Frequency")

### Display above created Barplots
milesUsageTM195Barplot
milesUsageTM498Barplot
milesUsageTM798Barplot

### Display Relation Between Fitness and Miles for TM195
milesFitnessTM195Barplot = qplot(Miles,fill=Fitness, data = Data195, binwidth = 10, boundary = 30,
                               main = "Miles vs Fitness of TM195",
                               xlab = "Miles",
                               ylab = "Frequency")

### Display Relation Between Fitness and Miles for TM498
milesFitnessTM498Barplot = qplot(Miles,fill=Usage, data = Data498, binwidth = 10, boundary = 30,
                               main = "Miles vs Fitness of TM498",
                               xlab = "Miles",
                               ylab = "Frequency")

### Display Relation Between Fitness and Miles for TM798
milesFitnessTM798Barplot = qplot(Miles,fill=Usage, data = Data798, binwidth = 10, boundary = 30,
                               main = "Miles vs Fitness of TM798",
                               xlab = "Miles",
                               ylab = "Frequency")

### Display above created Barplots
milesFitnessTM195Barplot
milesFitnessTM498Barplot
milesFitnessTM798Barplot

### Display Relation Between Marital Status and Miles for TM195
milesMaritalStatusTM195Barplot = qplot(Miles,fill=MaritalStatus, data = Data195, binwidth = 10, boundary = 30,
                                 main = "Miles vs Marital Status of TM195",
                                 xlab = "Miles",
                                 ylab = "Frequency")

### Display Relation Between Marital Status and Fitness for TM498
milesMaritalStatusTM498Barplot = qplot(Miles,fill=MaritalStatus, data = Data498, binwidth = 10, boundary = 30,
                                 main = "Miles vs Marital Status of TM498",
                                 xlab = "Miles",
                                 ylab = "Frequency")

### Display Relation Between Marital Status and Fitness for TM798
milesMaritalStatusTM798Barplot = qplot(Miles,fill=MaritalStatus, data = Data798, binwidth = 10, boundary = 30,
                                 main = "Miles vs Marital Status of TM798",
                                 xlab = "Miles",
                                 ylab = "Frequency")

### Display above created Barplots
milesMaritalStatusTM195Barplot
milesMaritalStatusTM498Barplot
milesMaritalStatusTM798Barplot

### Display Relation Between Marital Status and Usage for TM195
usageMaritalStatusTM195Barplot = qplot(Usage,fill=MaritalStatus, data = Data195, binwidth = 10, boundary = 30,
                                       main = "Usage vs Marital Status of TM195",
                                       xlab = "Usage",
                                       ylab = "Frequency")

### Display Relation Between Marital Status and Usage for TM498
usageMaritalStatusTM498Barplot = qplot(Usage,fill=MaritalStatus, data = Data498, binwidth = 10, boundary = 30,
                                       main = "Usage vs Marital Status of TM498",
                                       xlab = "Usage",
                                       ylab = "Frequency")

### Display Relation Between Marital Status and Usage for TM798
usageMaritalStatusTM798Barplot = qplot(Usage,fill=MaritalStatus, data = Data798, binwidth = 10, boundary = 30,
                                       main = "Usage vs Marital Status of TM798",
                                       xlab = "Usage",
                                       ylab = "Frequency")

### Display above created Barplots
usageMaritalStatusTM195Barplot
usageMaritalStatusTM498Barplot
usageMaritalStatusTM798Barplot
