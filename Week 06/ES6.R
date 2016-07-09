setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 06")
Sys.setlocale("LC_ALL", "C")

#---------------- DAILY KOS

dailykos <- read.csv("dailykos.csv")
str(dailykos)

# Compute distances
distance <- dist(dailykos, method="euclidean")

# Hierarchical clustering
clusterkos = hclust(distance, method = "ward.D") 

# Plot the dendrogram
plot(clusterkos)

# Assign points to clusters
clusterGroups = cutree(clusterkos, k = 7)
table(clusterGroups)

cluster01 <- dailykos[clusterGroups==1,]
cluster02 <- dailykos[clusterGroups==2,]
cluster03 <- dailykos[clusterGroups==3,]
cluster04 <- dailykos[clusterGroups==4,]
cluster05 <- dailykos[clusterGroups==5,]
cluster06 <- dailykos[clusterGroups==6,]
cluster07 <- dailykos[clusterGroups==7,]

tail(sort(colMeans(cluster01)))
tail(sort(colMeans(cluster02)))
tail(sort(colMeans(cluster03)))
tail(sort(colMeans(cluster04)))
tail(sort(colMeans(cluster05)))
tail(sort(colMeans(cluster06)))
tail(sort(colMeans(cluster07)))

# k means clustering
set.seed(1000)
kclust = kmeans(dailykos, centers = 7)
str(kclust)
table(kclust$cluster)

Kcluster01 <- dailykos[kclust$cluster==1,]
Kcluster02 <- dailykos[kclust$cluster==2,]
Kcluster03 <- dailykos[kclust$cluster==3,]
Kcluster04 <- dailykos[kclust$cluster==4,]
Kcluster05 <- dailykos[kclust$cluster==5,]
Kcluster06 <- dailykos[kclust$cluster==6,]
Kcluster07 <- dailykos[kclust$cluster==7,]

tail(sort(colMeans(Kcluster01)))
tail(sort(colMeans(Kcluster02)))
tail(sort(colMeans(Kcluster03)))
tail(sort(colMeans(Kcluster04)))
tail(sort(colMeans(Kcluster05)))
tail(sort(colMeans(Kcluster06)))
tail(sort(colMeans(Kcluster07)))

dkclus <- dailykos
dkclus$hclus <- clusterGroups
dkclus$kclus <- kclust$cluster


table(dkclus$hclus, dkclus$kclus)
table(dkclus$hclus)
table(dkclus$kclus)

#------------------------------------------------------------------------
#-----------------------  AIRLINE MARKET SEGMENTATION

airlines <- read.csv("AirlinesCluster.csv")
summary(airlines)

# normalize
library(caret)

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

# Compute distances
distance <- dist(airlinesNorm, method="euclidean")

# Hierarchical clustering
clusterair = hclust(distance, method = "ward.D") 

# Plot the dendrogram
plot(clusterair)

# Assign points to clusters
clusterGroups = cutree(clusterair, k = 5)
table(clusterGroups)

str(airlines)
tapply(airlines$Balance,         clusterGroups, mean)
tapply(airlines$QualMiles,       clusterGroups, mean)
tapply(airlines$BonusMiles,      clusterGroups, mean)
tapply(airlines$BonusTrans,      clusterGroups, mean)
tapply(airlines$FlightMiles,     clusterGroups, mean)
tapply(airlines$FlightTrans,     clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)

# now kmeans
set.seed(88)
kclust = kmeans(airlinesNorm, centers = 5)
table(kclust$cluster)
kclust$center

#------------------------------------------------------------------------
#------------  Predicting Stock Returns with Cluster-Then-Predict

stocks <- read.csv("StocksCluster.csv")
str(stocks)
mean(stocks$PositiveDec)
f <- c("ReturnJan", "ReturnFeb", "ReturnMar", "ReturnApr", "ReturnMay", "ReturnJune", "ReturnJuly", "ReturnAug", "ReturnSep", "ReturnOct", "ReturnNov")
cor(stocks[,f])

summary(stocks)

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest  = subset(stocks, spl == FALSE)

## logistic regression
StocksModel <- glm(stocksTrain$PositiveDec ~ ., data=stocksTrain, family=binomial)
pred <- predict(StocksModel, type="response")

table(stocksTrain$PositiveDec, pred >= 0.5)
(990+3640)/nrow(stocksTrain)

pred.test <- predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, pred.test >= 0.5)
(417+1553)/nrow(stocksTest)
table(stocksTest$PositiveDec)
1897/nrow(stocksTest)

# Problem 3.1 - Clustering Stocks 
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# pre process
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

#Problem 3.4 - Clustering Stocks 
set.seed(144)
km = kmeans(normTrain, centers = 3)
str(km)
table(km$cluster)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest  = predict(km.kcca, newdata=normTest)
table(clusterTest)

# Problem 4.1 - Cluster-Specific Predictions 
stocksTrain1 <- stocksTrain[clusterTrain==1,]
stocksTrain2 <- stocksTrain[clusterTrain==2,]
stocksTrain3 <- stocksTrain[clusterTrain==3,]
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)
stocksTest1 <- stocksTest[clusterTest==1,]
stocksTest2 <- stocksTest[clusterTest==2,]
stocksTest3 <- stocksTest[clusterTest==3,]


# Problem 4.2 - Cluster-Specific Predictions 
StocksModel1 <- glm(stocksTrain1$PositiveDec ~ ., data=stocksTrain1, family=binomial)
StocksModel2 <- glm(stocksTrain2$PositiveDec ~ ., data=stocksTrain2, family=binomial)
StocksModel3 <- glm(stocksTrain3$PositiveDec ~ ., data=stocksTrain3, family=binomial)
StocksModel1$coefficients
StocksModel2$coefficients
StocksModel3$coefficients


# Problem 4.3 - Cluster-Specific Predictions

pred.test1 <- predict(StocksModel1, newdata=stocksTest1, type="response")
table(stocksTest1$PositiveDec, pred.test1 >= 0.5)
(30+774)/nrow(stocksTest1)

pred.test2 <- predict(StocksModel2, newdata=stocksTest2, type="response")
table(stocksTest2$PositiveDec, pred.test2 >= 0.5)
(388+ 757)/nrow(stocksTest2)

pred.test3 <- predict(StocksModel3, newdata=stocksTest3, type="response")
table(stocksTest3$PositiveDec, pred.test3 >= 0.5)
(49+13)/nrow(stocksTest3)

#Problem 4.4 - Cluster-Specific Predictions 
AllPredictions = c(pred.test1, pred.test2, pred.test3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions >=0.5)

(467+1544)/length(AllPredictions)

