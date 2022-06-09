#Downloading train and test data
trainFile = "adult.data"; testFile = "adult.test"

if (!file.exists(trainFile))
  download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                destfile = trainFile)

if (!file.exists(testFile))
  download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test",
                destfile = testFile)  
#Assigning column names
colNames = c("age", "workclass", "fnlwgt", "education",
             "educationnum", "maritalstatus", "occupation",
             "relationship", "race", "sex", "capitalgain",
             "capitalloss", "hoursperweek", "nativecountry",
             "incomelevel")

#reading training data
training = read.table(trainFile, header = FALSE, sep = ",", strip.white = TRUE, col.names = colNames, na.strings = "?", stringsAsFactors = TRUE)

#Display the structure of the data
str(training)

#To check if our table has any missiing values
table(complete.cases(training))
summary(training[!complete.cases(training),])

#Removing NA's
TrainSet = training[na.omit(training$workclass) , na.omit(training$occupation),]
TrainSet = training[na.omit(training$nativecountry),]

#Removing unnecessary variables
TrainSet$fnlwgt = NULL

#Data Exploration
#Exploring the age variable
summary(TrainSet$age)

#Boxplot for age variable
boxplot(age ~ incomelevel, data = TrainSet,
        main = "Income levels based on the Age of an individual",
        xlab = "Income Level", 
        ylab = "Age",
        col = "salmon")


#Histogram for age variable
Income_Below_50k = (TrainSet$incomelevel == "<=50k")
xlimit = c(min(TrainSet$age), max(TrainSet$age))
ylimit = c(0, 1600)

library("ggplot2")
hist1 = qplot(age, data = TrainSet[Income_Below_50k,], margins = TRUE,
              binwidth = 2, xlim = xlimit, ylim = ylimit, colour = incomelevel)

hist2 = qplot(age, data = TrainSet[!Income_Below_50k,], margins = TRUE,
              binwidth = 2, xlim = xlimit, ylim = ylimit, colour = incomelevel)

install.packages("gridExtra")
library(gridExtra)
library("grid")
grid.arrange(hist1, hist2, nrow = 2)

#Exploring the educationnum variable
summary(TrainSet$educationnum)

#Boxplot for educationnum variable
boxplot(educationnum ~ incomelevel, data = TrainSet,
        main = "Years of education distribution for different income levels",
        xlab = "Income Levels",
        ylab = "Years of Education",
        col = "green")

#Exploring the capital-gain and capital-loss variable
summary(TrainSet[TrainSet$incomelevel == "<=50k",
                 c("capitalgain", "capitalloss")])

#Exploring the hours-per-week variable
summary(TrainSet$hoursperweek)
boxplot(hoursperweek ~ incomelevel, data = TrainSet,
        main = "Hours Per Week distribution for different income levels",
        xlab = "Income Levels",
        ylab = "Hours Per Week",
        col = "salmon")
#Evaluating the work-class variable
summary(TrainSet$workclass)
qplot(incomelevel, data = TrainSet, fill = workclass) + facet_grid (.~ workclass)

