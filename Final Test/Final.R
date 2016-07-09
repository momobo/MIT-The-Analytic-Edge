setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Final Test")
Sys.setlocale("LC_ALL", "C")


#Final Exam
fedFunds <- read.csv("federalFundsRate.csv", stringsAsFactors =F )

str(fedFunds)
head(fedFunds)

mean(fedFunds$RaisedFedFunds ==1)

library(plyr)
ddply(fedFunds, ~ Chairman,summarise,mnth=sum(!is.na(Chairman)))

##

fedFunds$Chairman       = as.factor(fedFunds$Chairman )
fedFunds$DemocraticPres = as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds = as.factor(fedFunds$RaisedFedFunds)

library(rpart)
library(randomForest)


#a <- randomForest(fedFunds ~ ., data=fedFunds)

set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)
train = subset(fedFunds, spl==TRUE)
test  = subset(fedFunds, spl==FALSE)

lr <- glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, 
          data=train, family="binomial")
summary(lr)

newd <- with(newd, data.frame(Streak = -3, PreviousRate = 1.17,Unemployment = 5.1,
                              HomeownershipRate = 65.3,  DemocraticPres = factor(0), MonthsUntilElection = 18))
str(newd)           
predict(lr, type="response", newdata=newd)
--
  #########
# Predictions on the test set
predictTest = predict(lr, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$RaisedFedFunds, predictTest > 0.5)

pred <- ifelse(predictTest > 0.5, 1, 0)
sum(pred==0)

##########
ROCRpred = prediction(predictTest, test$RaisedFedFunds)
as.numeric(performance(ROCRpred, "auc")@y.values)
##################
perf <- performance(ROCRpred,"tpr","fpr")

plot(perf,col="black",lty=3, lwd=3)
######################
table(test$RaisedFedFunds, predictTest > 0.37)
TN <- 36
FP <-      51
FN <- 14
TP <-      74
FPR <- FP /(FP+TN)
TPR <- TP /(TP+FN)
c(TPR, FPR)

################################

#      # Define cross-validation experiment
#      numFolds = trainControl( method = "cv", number = 10 )
#      cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 
#      
#      # Perform the cross validation
#      train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
#      
#      # Create a new CART model
#      StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)
#      
#      # Make predictions
#      PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
#      table(Test$Reverse, PredictCV)
#      (59+64)/(59+18+29+64)
#      prp(StevensTreeCV)
#      summary(StevensTreeCV)



library(caret)
cpGrid = expand.grid( .cp = seq(0.001,0.05,0.001))
set.seed(201)
numFolds = trainControl( method = "cv", number = 10 )
gbmFit1 <- train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, 
                data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

gbmFit1
summary(gbmFit1)
gbmFit1$resample

###################################################

cpCart = rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, 
                    data = train, method="class", cp=0.016)

prp(cpCart)

# Make predictions
PredictCART = predict(cpCart, newdata = newd, type = "class")
PredictCART == 0

 
 ##############################
PredictCART = predict(cpCart, newdata = test, type = "class")
 
table(test$RaisedFedFunds, PredictCART)
(64+48)/nrow(test)


#################################################################################
#################################################################################
#################################################################################
## chapter 6
#################################################################################

Households <- read.csv("Households.csv")
str(households)
sum(households$MorningPct==100)
sum(households$AfternoonPct==100)
#################
households[households$AvgSalesValue > 150,]
min(households[households$AvgDiscount > 25, ]$AvgSalesValue)
sum(households$NumVisits >= 300)/nrow(households)
########################
library(caret)
preproc = preProcess(Households)
HouseholdsNorm = predict(preproc, Households)
max(HouseholdsNorm$NumVisits)
min(HouseholdsNorm$AfternoonPct)
###############
set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)
###############
set.seed(200)
km = kmeans(HouseholdsNorm, centers = 10)
sort(table(km$cluster))
###############
km$cluster
hh <- Households
hh$cluster <- km$cluster
km$centers
###############
set.seed(5000)
km = kmeans(HouseholdsNorm, centers = 5)
sort(table(km$cluster))
km$centers

#################################################################################
#################################################################################
## 
#################################################################################
energy <- read.csv("energy.csv")
str(energy)
plot(energy$YEAR, energy$GenTotalRenewable)
library(ggplot2)
ggplot(data=energy[energy$STATE =="MA" | energy$STATE =="CA" | energy$STATE =="ID" | energy$STATE =="AZ",], aes(x=YEAR, y=GenTotalRenewable, col=STATE) ) + geom_line( size=2)
################
# 0 = republican

rep <- energy[energy$presidential.results==0, ]
dem <- energy[energy$presidential.results==1, ]

summary(energy$presidential.results)
mean(rep$AllSourcesCO2, na.rm=T)
mean(dem$AllSourcesCO2, na.rm=T)
mean(rep$AllSourcesNOx, na.rm=T)
mean(dem$AllSourcesNOx, na.rm=T)

cor(x=energy$AllSourcesCO2, y=energy$EsalesCommercial , use="complete.obs")
cor(x=energy$AllSourcesSO2, y=energy$EsalesIndustrial , use="complete.obs")
cor(x=energy$AllSourcesNOx, y=energy$EsalesResidential , use="complete.obs")

####
ggplot(data=energy, aes(x=STATE, y=EPriceTotal) ) + geom_boxplot()

l <- tapply(energy$EPriceTotal, energy$STATE, mean)
l2 <- tapply(energy$GenTotal, energy$STATE, mean)
sort(l2)
###############################
set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]

str(energy)
energy$GenSolarBinary <- as.factor(energy$GenSolarBinary)
mod = glm(GenSolarBinary ~ GenHydro + 
            GenSolar + CumlFinancial + 
            CumlRegulatory + Total.salary + 
            Import, data=train, family="binomial")
summary(mod)
############################################
predGLM  <- predict(mod, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$GenSolarBinary, predGLM > 0.5)
accuracy <- (154+18)/nrow(test)

table(test$presidential.results, test$GenSolarBinary, predGLM > 0.5)
isdem <- test$presidential.results == 1

table(test[isdem,]$GenSolarBinary, predGLM[isdem] > 0.5)
accudem <- (64+16)/nrow(test[isdem,])

table(test[!isdem,]$GenSolarBinary, predGLM[!isdem] > 0.5)
accurep <- (90+2)/nrow(test[!isdem,])
####

limit <- c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary", "Import")
train.limited <- train[,limit]
test.limited  <- test[, limit]
library(caret)
preproc <- preProcess(train.limited)
train.norm =  predict(preproc, train.limited)
preproc2 <- preProcess(test.limited)
test.norm  =  predict(preproc2, test.limited)


set.seed(144)
km = kmeans(train.norm, centers = 2, iter.max=1000)
sort(table(km$cluster))
km$centers
##
library(flexclust)
KMC.kcca = as.kcca(km, train.norm)
clusterTrain = predict(KMC.kcca)
clusterTest  = predict(KMC.kcca, newdata=test.norm)

train$cluster <- clusterTrain
train1 <- train[train$cluster == 1,]
train2 <- train[train$cluster == 2,]
test$cluster <- clusterTest
test1 <- test[test$cluster == 1,]
test2 <- test[test$cluster == 2,]


table(train1$presidential.results)
table(train2$presidential.results)

table(train1$CumlRegulatory)
table(train2$CumlRegulatory)

mean(train1$CumlFinancial)
mean(train2$CumlFinancial)

mean(train1$AllSourcesCO2, na.rm=T)
mean(train2$AllSourcesCO2, na.rm=T)
mean(train1$AllSourcesSO2, na.rm=T)
mean(train2$AllSourcesSO2, na.rm=T)
mean(train1$AllSourcesNOx, na.rm=T)
mean(train2$AllSourcesNOx, na.rm=T)

mod1 <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory+ Total.salary+ Import,
           data=train1, family="binomial")
summary(mod1)

####
predictTest = predict(mod1, type="response", newdata=test1)
table(test1$GenSolarBinary, predictTest > 0.5)
(114+4)/nrow(test1) 

predictTest.orig = predict(mod, type="response", newdata=test1)
table(test1$GenSolarBinary, predictTest.orig > 0.5)
115/nrow(test1)
###

mod2 <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory+ Total.salary+ Import,
            data=train2, family="binomial")
summary(mod2)
summary(mod1)
predictTest2 = predict(mod2, type="response", newdata=test2)
table(test2$GenSolarBinary, predictTest2 > 0.5)
(40+20)/nrow(test2)
predictTest2.orig = predict(mod, type="response", newdata=test2)
table(test2$GenSolarBinary, predictTest2.orig > 0.5)
(39+18)/nrow(test2)

##
AllPrediction = c(predictTest, predictTest2)
AllOutcomes = c(test1$GenSolarBinary, test2$GenSolarBinary)

table(AllOutcomes, AllPrediction > 0.5)
(154+24)/nrow(test)
