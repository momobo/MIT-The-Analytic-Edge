setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 03")

songs <- read.csv("songs.csv")
str(songs)
summary(songs)
table(songs$year)
table(songs$artistname)
songs[songs$artistname=="Michael Jackson",c("songtitle", "Top10")]

unique(songs$timesignature)

hist(songs$timesignature)

order(songs, songs$tempo, decreasing=TRUE)
head(songs[ order(-songs[,"tempo"]), ])

SongsTrain <- songs[songs$year <= 2009,]
SongsTest  <- songs[songs$year > 2009,]
nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")

# note: removing var with - does not works with categoric variable
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(SongsTrain$energy, SongsTrain$loudness)

# note: removing var with - works only with numeric variable
SongsLog2 = glm(Top10 ~ . -loudness, data=SongsTrain[,], family=binomial)
SongsLog3 = glm(Top10 ~ . -energy, data=SongsTrain, family=binomial)
summary(SongsLog2)
summary(SongsLog3)

# -- validating our models
TestPrediction = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10 , TestPrediction >= 0.45)
Accuracy <- (309+19)/nrow(SongsTest)
1-sum(SongsTest$Top10)/nrow(SongsTest)
TP <- 19
TN <- 309
FP <- 5
FN <- 40
Sensitivity <- TP / (TP+FN)
Specificity <- TN / (TN+FP)
Sensitivity
Specificity

# ----------------------------- ES II --------------------------------------------------
parole <- read.csv("parole.csv")

sum(parole$violator)

parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
str(parole)
table(parole$state)
summary(parole)

set.seed(144)

library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

model1 = glm(violator ~ ., data=train, family=binomial)
summary(model1)

# Consider a parolee who is male, of white race, aged 50 years at prison release, 
# from the state of Maryland, served 3 months, had a maximum sentence of 12 months, 
# did not commit multiple offenses, and committed a larceny. 
# Answer the following questions based on the model's predictions for this individual. 
# (HINT: You should use the coefficients of your model, the Logistic Response Function, 
# and the Odds equation to solve this problem.)
logit <- -4.2411574 + 0.3869904 + 0.8867192 + 50*(-0.0001756) + 3* (-0.1238867) + 12*0.0802954 + 0.6837143
odds <- exp(logit)
P <- 1/(1+exp(-logit))

# evaluation on test set
TestPrediction <- predict(model1, newdata=test, type="response")
max(TestPrediction)
table(test$violator , TestPrediction >= 0.5)
Accuracy <- (167+12)/nrow(test)
1-sum(test$violator)/nrow(test)
TP <- 12
TN <- 167
FP <- 12
FN <- 11
Sensitivity <- TP / (TP+FN)
Specificity <- TN / (TN+FP)
Sensitivity
Specificity

library(ROCR)

ROCRpred = prediction(TestPrediction, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

# ----------------------------- ES III --------------------------------------------------
loans <- read.csv("loans.csv")
summary(loans)
str(loans)
mean(loans$not.fully.paid)

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed


loans <- read.csv("loans_imputed.csv")
# -- 
library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test  = subset(loans, split == FALSE)
model1 <- glm(not.fully.paid ~ ., data=train, family=binomial )
summary(model1)

TestPrediction <- predict(model1, newdata=test, type="response")
test$predicted.risk <- TestPrediction
table(test$not.fully.paid , test$predicted.risk >= 0.5)
Accuracy <- (2400+3)/nrow(test)
1-sum(test$not.fully.paid)/nrow(test)
TP <- 
TN <- 
FP <- 
FN <- 
Sensitivity <- TP / (TP+FN)
Specificity <- TN / (TN+FP)
Sensitivity
Specificity
ROCRpred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

model.baseline <- glm(not.fully.paid ~ int.rate, data=train, family=binomial )
summary(model.baseline)

TestPredBaseline <- predict(model.baseline, newdata=test, type="response")
test$baseline <- TestPredBaseline
max(TestPredBaseline)
table(test$not.fully.paid , test$baseline >= 0.5)

ROCR.base = prediction(test$baseline, test$not.fully.paid)
as.numeric(performance(ROCR.base, "auc")@y.values)
C <- 10
R <- 0.06
t <- 3
C * exp(R*t)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)

highInterest  <- subset(test, test$int.rate >= 0.15)
mean(highInterest$profit)
sum(highInterest$not.fully.paid)/nrow(highInterest)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
best100  <- subset(highInterest, highInterest$predicted.risk <= cutoff)
mean(best100$profit)
sum(best100$not.fully.paid)
