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
relevant <- c(   "YOB"       ,    "Gender"     ,   "EducationLevel",
                 "Q122120"   ,    "Q121699"   ,    "Q120379"    ,   "Q120650"       ,
                 "Q120472"   ,    "Q120014"   ,    "Q116881"    ,   "Q116601"       ,
                 "Q115602"  ,     "Q115610"  ,     "Q115611"   ,    "Q115390"      ,
                 "Q115195"  ,     "Q114517"  ,     "Q114386"   ,    "Q113992"      ,
                 "Q113583"  ,     "Q113181"  ,     "Q111848"   ,    "Q110740"      ,
                 "Q109367"  ,     "Q109244"  ,     "Q108855"   ,    "Q108856"      ,
                 "Q107869"  ,     "Q106272"  ,     "Q106388"   ,    "Q105655"      ,
                 "Q102089"  ,     "Q101163"  ,     "Q101596"   ,    "Q100689"      ,
                 "Q99716"   ,     "Q99480"   ,     "Q98059"    ,    "Q98078"       ,
                 "Q98197"  )   

# mean is approx 1980
train$YOB <- ifelse(train$YOB > 2000 | train$YOB < 1935, NA , train$YOB )
train$YOB <- ifelse(is.na(train$YOB), 1980 , train$YOB )
test$YOB <- ifelse(test$YOB > 2000 | test$YOB < 1935, NA , test$YOB )
test$YOB <- ifelse(is.na(test$YOB), 1980 , test$YOB )

train1 <- train[,c("Party", relevant)]
test1  <- test[,relevant]

#fitrf <-rfImpute(Party ~ . -USER_ID, data=train)
modelrf <- randomForest(Party ~ ., data=train1)
predRF  <- predict(modelrf, type="prob", newdata=test1)[,2]
?randomForest

summary(modelrf)
# And then make predictions on the test set:


threshold = 0.5
PredTestLabels = as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog9.csv", row.names=FALSE,quote = FALSE)
