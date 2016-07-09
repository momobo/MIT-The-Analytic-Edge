setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Kaggle MIT")
Sys.setlocale("LC_ALL", "C")

train = read.csv("train2016.csv")
test  = read.csv("test2016.csv")

# for(col in names(train)){
#   levels(train[,col])[levels(train[,col])==""] <- "Unk"
# }
# for(col in names(test)){
#   levels(test[,col])[levels(test[,col])==""] <- "Unk"
# }
questions = grep("^Q",names(train))

# loop over all the questions
for (i in questions) {
  # convert the ith question, note double brackets [[ notation ...
  train[[i]] = as.numeric(train[[i]]) - 1
}

questions = grep("^Q",names(test))

# loop over all the questions
for (i in questions) {
  # convert the ith question, note double brackets [[ notation ...
  test[[i]] = as.numeric(test[[i]]) - 1
}
# mean is approx 1980
train$YOB <- ifelse(train$YOB > 2000 | train$YOB < 1935, NA , train$YOB )
train$YOB <- ifelse(is.na(train$YOB), 1980 , train$YOB )
test$YOB <- ifelse(test$YOB > 2000 | test$YOB < 1935, NA , test$YOB )
test$YOB <- ifelse(is.na(test$YOB), 1980 , test$YOB )
# train$AgeGroup <- cut(2013 - train$YOB,c(0, 25, 35, 45, 55, 65, 100), ordered_result = T)
# test$AgeGroup  <- cut(2013 - test$YOB, c(0, 25, 35, 45, 55, 65, 100),  ordered_result = T)
# train$YOB <- NULL
# test$YOB  <- NULL
# table(train$AgeGroup, useNA="ifany")
# table(test$AgeGroup, useNA="ifany")

train$Gender = as.numeric(train$Gender) - 1
test$Gender  = as.numeric(test$Gender) - 1
train$Income = as.numeric(train$Income) - 1
test$Income  = as.numeric(test$Income) - 1
train$EducationLevel = as.numeric(train$EducationLevel) - 1
test$EducationLevel  = as.numeric(test$EducationLevel) - 1
train$HouseholdStatus = as.numeric(train$HouseholdStatus) - 1
test$HouseholdStatus  = as.numeric(test$HouseholdStatus) - 1

library(glmnet)


x.train <- as.matrix(train[ , c(2:6,8:108)])
x.test  <- as.matrix(test[ , c(2:107)])
y <- (train$Party)

x.test[,5]
fit <- glmnet(x.train, y, family="binomial")
plot(fit, xvar="lambda", label=T)
cv.fit <- cv.glmnet(x.train, y, family="binomial")
plot(cv.fit)
coef(cv.fit)

pred=predict(lasso.tr,x[-train,])
predlasso  <- predict(cv.fit, x.test, type="class")
summary(modelrf)
# And then make predictions on the test set:


#threshold = 0.5
#PredTestLabels = as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))
PredTestLabels = as.factor(predlasso)

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog7.csv", row.names=FALSE,quote = FALSE)

