setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Kaggle MIT")
Sys.setlocale("LC_ALL", "C")

train = read.csv("train2016.csv", na.strings = c("NA",""))
test  = read.csv("test2016.csv", na.strings = c("NA",""))


# mean is approx 1980
train$YOB <- ifelse(train$YOB > 2000 | train$YOB < 1935, NA , train$YOB )
train$YOB <- ifelse(is.na(train$YOB), 1980 , train$YOB )
test$YOB <- ifelse(test$YOB > 2000 | test$YOB < 1935, NA , test$YOB )
test$YOB <- ifelse(is.na(test$YOB), 1980 , test$YOB )
train$YOB <- train$YOB / 2000
test$YOB  <- test$YOB  / 2000

library(FNN)
# normalize
str(train)

x.train <- train[ , c(2:6,8:108)]
x.test  <- test[ , c(2:107)]

kdist <- dist(x.train)
# Hierarchical clustering
cluster = hclust(kdist, method = "ward.D") 
# Plot the dendrogram
plot(cluster)



#fitrf <-rfImpute(Party ~ . -USER_ID, data=train)
modelrf <- randomForest(Party ~ . -USER_ID, data=train, type="classification")
predRF  <- predict(modelrf, type="prob", newdata=test1)[,2]
?randomForest

summary(modelrf)
# And then make predictions on the test set:


threshold = 0.5
PredTestLabels = as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog9.csv", row.names=FALSE,quote = FALSE)
