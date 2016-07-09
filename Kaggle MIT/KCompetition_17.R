setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Kaggle MIT")
Sys.setlocale("LC_ALL", "C")

train = read.csv("train2016.csv", na.strings = c("NA",""))
test  = read.csv("test2016.csv", na.strings = c("NA",""))

library(randomForest)

# mean is approx 1980
train$YOB <- ifelse(train$YOB > 2000 | train$YOB < 1935, NA , train$YOB )
train$YOB <- ifelse(is.na(train$YOB), 1980 , train$YOB )
test$YOB <- ifelse(test$YOB > 2000 | test$YOB < 1935, NA , test$YOB )
# #test$YOB <- ifelse(is.na(test$YOB), 1980 , test$YOB )
# 
# library(caTools)
# set.seed(3000)
# spl = sample.split(train$Party, SplitRatio = 0.7)
# sub.train = subset(train, spl==TRUE)
# sub.test  = subset(train, spl==FALSE)
# 
# #install_github("stekhoven/missForest")
# library(missForest)
# # try rfimpute
# sub.train.inp <- missForest(sub.train)
# sub.test.inp  <- missForest(sub.test)
# 
# 
# sub.train.inp$OOBerror
# sub.test.inp$OOBerror
# 
# i.train <- sub.train.inp$ximp
# i.test  <- sub.test.inp$ximp
# 
# oob.err  <- double(30)
# test.err <- double(30)
# threshold = 0.5
# # mtry <- 10
# # loop to find it
# for(mtry in 1:40){
#   fit <- randomForest(Party ~ . -USER_ID, data=i.train, mtry=mtry, ntree=400)
#   oob.err[mtry] <- fit$err.rate[400]
#   predRF  <- predict(fit, type="prob", newdata=i.test)[,2]
#   PredTestLabels <- as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))
#   test.err[mtry] <- sum(PredTestLabels != i.test$Party)/nrow(i.test)
#   cat(mtry, test.err[mtry],"   ")
# }
# #sort(fit$importance)
# fit$confusion
# plot(test.err)
# plot(oob.err)
# 
# matplot(1:mtry, cbind(oob.err, test.err), pch=19, col=c("red", "blue"), type="b", ylab="Error")
# legend("topright", legend=c("OOB", "TEST"), pch=19, col=c("red", "blue"))
# 
# # 22 is a good compromise

# go for the complete set#
library(mice)
L <- nrow(train)
traintest.inp <- mice(rbind(train[,-7], test))
traintest.com <- complete(traintest.inp)

i.train <- traintest.com[1:L,]
i.train$Party <- train$Party
i.test  <- traintest.com[(L+1):nrow(traintest.com),]



fit <- randomForest(Party  ~  Q98197 + Q98869+ Q99480+ Q100010+ Q100562+ Q100680+ Q101162+ 
                      Q102089+ Q102674+ Q103293+ Q106272+ Q106388+ Q108343+ Q108617+ 
                      Q108754+ Q109244+ Q109367+ Q112512+ Q113181+ Q114152+ Q115195+ 
                      Q120379+ Q120650+ Q122771+ Q123464+ Q123621+ Q115611+ Q116441+
                      Q116601+ Q116953+ Q118117+ Q119851+ Q120014+ Q120379+ Q120650+ 
                      Q122771+ Q123464+ Q123621 +
                      Income + YOB + Gender + HouseholdStatus + EducationLevel, data=i.train)

predRF  <- predict(fit, type="prob", newdata=i.test)[,2]

threshold <- 0.5
PredTestLabels <- as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog15.csv", row.names=FALSE,quote = FALSE)

# 0.61 male



