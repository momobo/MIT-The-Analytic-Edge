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

library(caTools)
set.seed(3000)
spl = sample.split(train$Party, SplitRatio = 0.7)
sub.train = subset(train, spl==TRUE)
sub.test  = subset(train, spl==FALSE)

oob.err  <- double(13)
test.err <- double(13)
threshold = 0.5
for(mtry in 1:60){
  fit <- randomForest(Party ~ . -USER_ID, data=sub.train, mtry=mtry, ntree=400)
  oob.err[mtry] <- fit$err.rate[400]
  predRF  <- predict(fit, type="prob", newdata=sub.test)[,2]
  PredTestLabels <- as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))
  test.err[mtry] <- sum(PredTestLabels != sub.test$Party)/nrow(sub.test)
  cat(mtry, " ")
}
test.err
matplot(1:mtry, cbind(test.err, oob.err), pch=19, col=c("red", "blue"), type="b", ylab="Error")
legend("topright", legend=c("OOB", "TEST"), pch=19, col=c("red", "blue"))

#fitrf <-rfImpute(Party ~ . -USER_ID, data=train)
modelrf <- randomForest(Party ~ . -USER_ID, data=train)
predRF  <- predict(modelrf, type="prob", newdata=test)[,2]


summary(modelrf)
# And then make predictions on the test set:


threshold = 0.5
PredTestLabels = as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog4.csv", row.names=FALSE,quote = FALSE)
