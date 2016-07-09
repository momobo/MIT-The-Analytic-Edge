setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 04")
# note: ES2 da confrontare con TensorFlow

gerber <- read.csv("gerber.csv")
str(gerber)

mean(gerber$voting)

mean(gerber[gerber$civicduty ==1,"voting"])
mean(gerber[gerber$hawthorne ==1,"voting"])
mean(gerber[gerber$self ==1,"voting"])
mean(gerber[gerber$neighbors==1,"voting"])

model1 <- glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family=binomial)
summary(model1)
TestPrediction = predict(model1, type="response")
table(gerber$voting , TestPrediction >= 0.3)
(134513 + 51966)/nrow(gerber)
table(gerber$voting , TestPrediction >= 0.5)
235388/nrow(gerber)

ROCRpred = prediction(TestPrediction, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

# TREES
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors+ sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CartModel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CartModel4, digit=6)
abs(0.34 - 0.296638)

CartModel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
Test5Prediction = predict(CartModel5, type="vector")

prp(CartModel5, digit=6)
mc <- 0.302795 
wc <- 0.290456
mn <- 0.345818
wn <- 0.334176
abs(wc-wn) - abs(mc-mn)

model2 <- glm(voting ~ sex+control, data=gerber, family=binomial)
summary(model2)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model2, newdata=Possibilities, type="response")

wclog <- 0.2908065
abs(wc -wclog)

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")
wclog2 <- 0.2904558
abs(wc -wclog2)

#-------------------------------------------------------------------------
# ES 2
#-------------------------------------------------------------------------

letters <- read.csv("letters_ABPR.csv")
str(letters)
letters$isB = as.factor(letters$letter == "B")
set.seed(1000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, spl==TRUE)
test  = subset(letters, spl==FALSE)
sum(test$isB == F)/nrow(test)

CARTb = rpart(isB ~ . - letter, data=train, method="class")

# accuracy on the test set
PredictTest = predict(CARTb, newdata = test, type = "class")
table(test$isB, PredictTest)

(1114 + 351 )/nrow(test)

# random forest
library(randomForest)
set.seed(1000)
CartBForest = randomForest(isB ~ . - letter, data=train)
PredictTest = predict(CartBForest, newdata = test, type = "class")
table(test$isB, PredictTest)
(1164 + 374 )/nrow(test)

# multiclass
letters$letter = as.factor( letters$letter )

set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, spl==TRUE)
test  = subset(letters, spl==FALSE)
table(test$letter)
401/nrow(test)

CARTletter = rpart(letter ~ . - isB, data=train, method="class")
PredictTest = predict(CARTletter, newdata = test, type = "class")
table(test$letter, PredictTest)
(348 + 318 + 363 + 340 )/nrow(test)

set.seed(1000)
CARTlForest = randomForest(letter ~ . - isB, data=train, method="class")
PredictTest = predict(CARTlForest, newdata = test, type = "class")
table(test$letter, PredictTest)
(390 + 380 + 393 + 364 )/nrow(test)

#-------------------------------------------------------------------------
# ES 3
#-------------------------------------------------------------------------

census <- read.csv("census.csv")
str(census)

set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test  = subset(census, spl==FALSE)
table(test$over50k)

401/nrow(test)

model1 <- glm(over50k ~ ., data=train, family=binomial)
summary(model1)

TestPrediction = predict(model1, newdata=test, type="response")
table(test$over50k , TestPrediction >= 0.5)
Accuracy <- (9051+1888)/nrow(test)
9713/nrow(test)

library(ROCR)

ROCRpred = prediction(TestPrediction, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)


CART = rpart(over50k ~ ., data=train, method="class")
prp(CART)

PredictTest = predict(CART, newdata = test, type = "class")
table(test$over50k, PredictTest)

(9243 + 1596 )/nrow(test)

# ROCR
PredictTest = predict(CART, newdata = test)

ROCRpred = prediction(PredictTest[,2], test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Prediction function
ROCRpred = prediction(PredictTest[,2], test$over50k)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# RANDOM FOREST
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

library(randomForest)
set.seed(1)
ModelForest = randomForest(over50k ~ ., data=trainSmall)
PredictTest = predict(ModelForest, newdata = test, type = "class")
table(test$over50k, PredictTest)
(9586 + 1093 )/nrow(test)

# random forest dotchart
vu = varUsed(ModelForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(ModelForest$forest$xlevels[vusorted$ix]))

varImpPlot(ModelForest)

# cross validation
library(caret)
library(e1071)
# Define cross-validation experiment
set.seed(2)
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))


# Perform the cross validation
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )

# Create a new CART model
modelCART = rpart(over50k ~ ., data = train, method="class", cp = 0.002)

# Make predictions
PredictCART = predict(modelCART, newdata = test, type = "class")
table(test$over50k, PredictCART)
(9178+1838)/nrow(test)

prp(modelCART)

summary(modelCART)









