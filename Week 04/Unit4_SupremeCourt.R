setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 04")
# Unit 4 - "Judge, Jury, and Classifier" Lecture


# VIDEO 4

# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                    data = Train, method="class", minbucket=25)

prp(StevensTree)

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/nrow(Test)

# ROC curve
library(ROCR)

PredictROC <- predict(StevensTree, newdata = Test)
PredictROC

pred <- prediction(PredictROC[,2], Test$Reverse)  # a ROCR function
perf <- performance(pred, "tpr", "fpr")           # a ROCR function
plot(perf)

# exercise:
AUC <- as.numeric(performance(pred, "auc")@y.values)
StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                    data = Train, method="class", minbucket=5)

summary(StevensTree2)
prp(StevensTree2)
StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                     data = Train, method="class", minbucket=100)

prp(StevensTree3)


# VIDEO 5 - Random Forests

# Install randomForest package
#install.packages("randomForest")
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse  = as.factor(Test$Reverse)

# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                             data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(40+74)/(40+37+19+74)

# exercise
set.seed(100)
StevensForest2 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                             data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest2 = predict(StevensForest2, newdata = Test)
table(Test$Reverse, PredictForest2)
(43+74)/nrow(Test)

set.seed(200)
StevensForest3 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                              data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest3 = predict(StevensForest3, newdata = Test)
table(Test$Reverse, PredictForest3)
(45+76)/nrow(Test)

# VIDEO 6

# Install cross-validation packages
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)
prp(StevensTreeCV)
summary(StevensTreeCV)

