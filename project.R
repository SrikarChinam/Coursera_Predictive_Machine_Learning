# Background
# 
# Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large 
# amount of data about personal activity relatively inexpensively. These type of devices are part of 
# the quantified self movement – a group of enthusiasts who take measurements about themselves 
# regularly to improve their health, to find patterns in their behavior, or because they are tech 
# geeks. One thing that people regularly do is quantify how much of a particular activity they do, 
# but they rarely quantify how well they do it. In this project, your goal will be to use data from 
# accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to 
# perform barbell lifts correctly and incorrectly in 5 different ways. More information is available
# from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight 
# Lifting Exercise Dataset). 
# 
# Data 
# 
# The training data for this project are available here: 
#   https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
# 
# The test data are available here: 
#   https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
# 
# The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 
# If you use the document you create for this class for any purpose please cite them as they have 
# been very generous in allowing their data to be used for this kind of assignment. 
# 
# What you should submit
# 
# The goal of your project is to predict the manner in which they did the exercise. This is the 
# "classe" variable in the training set. You may use any of the other variables to predict with. 
# You should create a report describing how you built your model, how you used cross validation, 
# what you think the expected out of sample error is, and why you made the choices you did. You 
# will also use your prediction model to predict 20 different test cases. 


#SOLUTION:

#load train data
train <- read.csv("~/Desktop/Coursera/machine learning/ML project/pml-training.csv")
test<- read.csv("~/Desktop/Coursera/machine learning/ML project/pml-testing.csv")

#create training and test set from train data set 
library(caret)

set.seed(456)
inTrain <- createDataPartition(train$classe, p = 0.8, list = FALSE)
training <- train[inTrain, ]
testing <- train[-inTrain, ]



#selecting the right predictors
# remove predictors which have a near zero variance
nzv <- nearZeroVar(training)
training <- training[, -nzv]

#remove predictors with more than 60% NA values
NAlength <- sapply(training, function(x) {
  sum(!(is.na(x) | x == ""))
})
rmNApred <- which(NAlength < 0.6 * length(training$classe))
training <- training[, -rmNApred]


#remove descriptive predictors
rmdesc <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
rmdesc<- match(rmdesc,colnames(training))
rmdesc<-rmdesc[!is.na(rmdesc)]
training <- training[, -rmdesc]


# model using Randomforests
library(randomForest)
modFit <- randomForest(classe ~ ., data = training, importance = TRUE, ntrees = 10)


# model validation
predtesting<-predict(modFit,testing)
confusionMatrix(predtesting, testing$classe)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    A    B    C    D    E
# A 1116    0    0    0    0
# B    0  757    6    0    0
# C    0    2  678    4    0
# D    0    0    0  639    2
# E    0    0    0    0  719
# 
# Overall Statistics
# 
# Accuracy : 0.9964        
# 95% CI : (0.994, 0.998)
# No Information Rate : 0.2845        
# P-Value [Acc > NIR] : < 2.2e-16     
# 
# Kappa : 0.9955        
# Mcnemar's Test P-Value : NA            
# 
# Statistics by Class:
# 
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            1.0000   0.9974   0.9912   0.9938   0.9972
# Specificity            1.0000   0.9981   0.9981   0.9994   1.0000
# Pos Pred Value         1.0000   0.9921   0.9912   0.9969   1.0000
# Neg Pred Value         1.0000   0.9994   0.9981   0.9988   0.9994
# Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
# Detection Rate         0.2845   0.1930   0.1728   0.1629   0.1833
# Detection Prevalence   0.2845   0.1945   0.1744   0.1634   0.1833
# Balanced Accuracy      1.0000   0.9977   0.9947   0.9966   0.9986



# model prediction for given test dataset
predtest<-predict(modFit,test)
answers <- as.vector(predtest)

pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
  }
}

pml_write_files(answers)