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




#fitrf <-rfImpute(Party ~ . -USER_ID, data=train)
modelrf <- randomForest(Party ~ Q98197+ Q98869+ Q99480+ Q100010+ Q100562+ Q100680+ Q101162+ 
                          Q102089+ Q102674+ Q103293+ Q106272+ Q106388+ Q108343+ Q108617+ 
                          Q108754+ Q109244+ Q109367+ Q112512+ Q113181+ Q114152+ Q115195+ 
                          Q120379+ Q120650+ Q122771+ Q123464+ Q123621+ Q115611+ Q116441+
                          Q116601+ Q116953+ Q118117+ Q119851+ Q120014+ Q120379+ Q120650+ 
                          Q122771+ Q123464+ Q123621 +
                          Income + YOB + Gender + HouseholdStatus + EducationLevel, data=train)
predRF  <- predict(modelrf, type="prob", newdata=test)[,2]


summary(modelrf)
# And then make predictions on the test set:


threshold = 0.5
PredTestLabels = as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog5.csv", row.names=FALSE,quote = FALSE)
