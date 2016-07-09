setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Kaggle MIT")
Sys.setlocale("LC_ALL", "C")
library(randomForest)

train = read.csv("train2016.csv", na.strings = c("NA",""))
test  = read.csv("test2016.csv", na.strings = c("NA",""))

# feature org
# mean is approx 1980
train$NumNaQ <- 0
for(row in 1:nrow(train)){
  train[row, "NumNaQ"] <- sum(is.na(train[row,]))/101
}

test$NumNaQ <- 0
for(row in 1:nrow(test)){
  test[row, "NumNaQ"] <- sum(is.na(test[row,]))/101
}

train$YOB <- ifelse(train$YOB > 2000 | train$YOB < 1935, NA , train$YOB )
train$YOB <- ifelse(is.na(train$YOB), 1980 , train$YOB )
test$YOB <- ifelse(test$YOB > 2000 | test$YOB < 1935, NA , test$YOB )
test$YOB <- ifelse(is.na(test$YOB), 1980 , test$YOB )
#train$AgeGroup <- cut(2013 - train$YOB,c(0, 29, 44, 60, 100), ordered_result = T)
#test$AgeGroup  <- cut(2013 - test$YOB, c(0, 29, 44, 60, 100), ordered_result = T)
train$Age <- (2013 - train$YOB)
test$Age  <- (2013 - test$YOB)
train$YOB <- NULL
test$YOB <- NULL

#table(train$AgeGroup, useNA="ifany")
#table(test$AgeGroup, useNA="ifany")


# imputation
library(missForest)
traintest <- missForest(rbind(train[,-6], test))
L <- nrow(train)
i.train <- traintest$ximp[1:L,]
i.train$Party <- train$Party
i.test  <- traintest$ximp[(L+1):nrow(traintest$ximp),]

# validation
library(caTools)
set.seed(3001)
spl = sample.split(i.train$Party, SplitRatio = 0.7)
sub.train = subset(i.train, spl==TRUE)
sub.test  = subset(i.train, spl==FALSE)



#----------------------------
oob.err  <- double(20)
test.err <- double(20)
threshold = 0.5
# mtry <- 10
# loop to find it
mtry <- 6
for(mtry in 1:20){
  fit <- randomForest(Party ~ NumNaQ + Q98197+ Q98869+ Q99480+ Q100010+ Q100562+ Q100680+ Q101162+ 
                        Q102089+ Q102674+ Q103293+ Q106272+ Q106388+ Q108343+ Q108617+ 
                        Q108754+ Q109244+ Q109367+ Q112512+ Q113181+ Q114152+ Q115195+ 
                        Q120379+ Q120650+ Q122771+  Q123621+ Q115611+ Q116441+
                        Q116601+ Q116953+ Q118117+ Q119851+ Q120014+ Q120379+ Q120650+ 
                        Q122771+ Q123464+ Q123621 + NumNaQ +
                        Income + Gender + HouseholdStatus + EducationLevel , data=sub.train, mtry=mtry, ntree=400)
  oob.err[mtry] <- fit$err.rate[400]
  predRF  <- predict(fit, type="prob", newdata=sub.test)[,2]
  PredTestLabels <- as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))
  test.err[mtry] <- sum(PredTestLabels != sub.test$Party)/nrow(sub.test)
  cat(mtry, test.err[mtry],"   ")
}
matplot(1:mtry, cbind(oob.err, test.err), pch=19, col=c("red", "blue"), type="b", ylab="Error")
legend("topright", legend=c("OOB", "TEST"), pch=19, col=c("red", "blue"))

# fit$importance > 19

# end validation


fit <- randomForest(Party ~ NumNaQ + Q98197+ Q98869+ Q99480+ Q100010+ Q100562+ Q100680+ Q101162+ 
                      Q102089+ Q102674+ Q103293+ Q106272+ Q106388+ Q108343+ Q108617+ 
                      Q108754+ Q109244+ Q109367+ Q112512+ Q113181+ Q114152+ Q115195+ 
                      Q120379+ Q120650+ Q122771+  Q123621+ Q115611+ Q116441+
                      Q116601+ Q116953+ Q118117+ Q119851+ Q120014+ Q120379+ Q120650+ 
                      Q122771+ Q123621 + NumNaQ +
                      Income + Gender + HouseholdStatus + EducationLevel , data=i.train, mtry=3)
fit$importance
fit$err.rate[500]
predRF  <- predict(fit, type="prob", newdata=i.test)[,2]

threshold <- 0.5
PredTestLabels <- as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog22.csv", row.names=FALSE,quote = FALSE)

# 0.61 male



