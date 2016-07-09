setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Kaggle MIT")
Sys.setlocale("LC_ALL", "C")

train = read.csv("train2016.csv")
test  = read.csv("test2016.csv")

for(col in names(train)){
  levels(train[,col])[levels(train[,col])==""] <- "Unk"
}
for(col in names(test)){
  levels(test[,col])[levels(test[,col])==""] <- "Unk"
}

library(randomForest)

table(train$YOB, useNA = "ifany")

train$YOB <- ifelse(train$YOB > 2000 | train$YOB < 1935, NA , train$YOB )


# mean is approx 1980
train$YOB <- ifelse(train$YOB > 2000 | train$YOB < 1935, NA , train$YOB )
train$YOB <- ifelse(is.na(train$YOB), 1980 , train$YOB )
test$YOB <- ifelse(test$YOB > 2000 | test$YOB < 1935, NA , test$YOB )
test$YOB <- ifelse(is.na(test$YOB), 1980 , test$YOB )



?regsubsets
#SimpleMod = glm(Party ~ . -USER_ID , data=train, family=binomial)

head(test)
summary(train)
#fitrf <-rfImpute(Party ~ . -USER_ID, data=train)
modelrf <- randomForest(Party ~ . -USER_ID, data=train)
predRF  <- predict(modelrf, type="prob", newdata=test)[,2]


summary(modelrf)
# And then make predictions on the test set:


threshold = 0.5
PredTestLabels = as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog4.csv", row.names=FALSE,quote = FALSE)
