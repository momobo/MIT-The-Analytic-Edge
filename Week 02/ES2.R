setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 02")

climate <- read.csv("climate_change.csv")
str(climate)

train <- subset(climate, climate$Year <= 2006)
test  <- subset(climate, climate$Year > 2006)
tail(test)

model1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(model1)

# correlation
cor(train[, c("MEI", "CO2", "CH4", "N2O", "CFC.11", "CFC.12", "TSI", "Aerosols")])

model2 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data=train)
summary(model2)

model3 <- step(model1)
summary(model3)

# predictions
testres <- predict(model3, newdata= test)

# Compute out-of-sample R^2
SSE = sum((testres - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST
R2

###################################################################################
# II set PISA Data

ptrain <- read.csv("pisa2009train.csv")
ptest  <- read.csv("pisa2009test.csv")

tapply(ptrain$readingScore, ptrain$male, mean)

# missing
summary(ptrain)

pisaTrain = na.omit(ptrain)

pisaTest = na.omit(ptest)
str(pisaTest)

### relevel

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore <- lm(readingScore ~ .  ,data=pisaTrain)
summary(lmScore)

install.packages("Metrics")
require(Metrics)
rmse(pisaTrain$readingScore, predict(lmScore))

## test
predtest <- predict(lmScore, newdata=pisaTest)
r <- range(predtest)
r[2] -r[1]
# Compute out-of-sample R^2
SSE = sum((predtest - pisaTest$readingScore)^2)
RMSE <- sqrt(SSE/nrow(pisaTest))
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
R2 = 1 - SSE/SST
R2

###################################################################################
# III set flu data
FluTrain <- read.csv("FluTrain.csv")
head(FluTrain)
?order
tail(FluTrain[order(FluTrain$ILI),])

tail(FluTrain[order(FluTrain$Queries),])

hist(FluTrain$ILI)
# skew right

plot(FluTrain$Queries, log(FluTrain$ILI))
#------

FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

cor(log(FluTrain$ILI), FluTrain$Queries)

#  test set

FluTest <- read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
head(PredTest1)

# problem 3.2
head(PredTest1)
pt <- PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
ot <- FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),"ILI"]
(ot - pt)/ot

SSEt <- sum((PredTest1 - FluTest$ILI)^2)
RMSEt <- sqrt(SSEt / nrow(FluTest))
RMSEt
#----------- TIME SERIES
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

summary(ILILag2)
plot(log(FluTrain$ILI), log(ILILag2))

FluTrend2 <-  lm(log(ILI) ~ Queries+ log(ILILag2) , data = FluTrain)
summary(FluTrend2)

# back to test set
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

summary(ILILag2)

## completing first two ILILag2 in test
FluTest$ILILag2[1] <- FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] <- FluTrain$ILI[nrow(FluTrain)]

# again test set performance
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSEt2 <- sum((PredTest2 - FluTest$ILI)^2)
RMSEt2 <- sqrt(SSEt2 / nrow(FluTest))
RMSEt2
