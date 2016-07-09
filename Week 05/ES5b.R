setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 05")
Sys.setlocale("LC_ALL", "C")

wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
str(wiki)

sum(wiki$Vandal==1)


corpusAdded <- Corpus(VectorSource(wiki$Added))


corpusAdded[[1]][2]

corpusAdded = tm_map(corpusAdded, PlainTextDocument)


corpusAdded <-  tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <-  tm_map(corpusAdded, stemDocument)
corpusAdded
dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded

inspect(dtmAdded[1,])
findFreqTerms(dtmAdded, lowfreq=20)

# beware to the trick.
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
str(wordsAdded)
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# removed
corpusRemoved <- Corpus(VectorSource(wiki$Removed))

corpusRemoved[[1]][2]

corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)


corpusRemoved <-  tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <-  tm_map(corpusRemoved, stemDocument)
corpusRemoved
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
dtmRemoved

# beware to the trick.
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
str(wordsRemoved)
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved) 
str(wikiWords)
colnames(wikiWords)

wikiWords$Vandal <- wiki$Vandal
 
2061/nrow(wikiWords)

set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)

train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords,  spl == FALSE)

table(test$Vandal )
618/nrow(test)

wikiCART <- rpart(Vandal~., data=train, method="class")
prp(wikiCART)

pred <- predict(wikiCART, newdata=test)[,2]
table(test$Vandal, pred >= 0.5)

pred2 <- predict(wikiCART, newdata=test, type="class")
table(test$Vandal, pred2)
(618+12)/nrow(test)
##########

wikiWords2 <- wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP)


wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2 <- rpart(Vandal~., data=wikiTrain2, method="class")
prp(wikiCART2)

pred2 <- predict(wikiCART2, newdata=wikiTest2)[,2]
table(wikiTest2$Vandal, pred2 >= 0.5)


(609+57)/nrow(wikiTest2)
#

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2 <- rpart(Vandal~., data=wikiTrain2, method="class")
prp(wikiCART2)

pred2 <- predict(wikiCART2, newdata=wikiTest2)[,2]
table(wikiTest2$Vandal, pred2 >= 0.5)


(514+248)/nrow(wikiTest2)


wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, spl==TRUE)
wikiTest3 = subset(wikiWords3, spl==FALSE)

wikiCART3 <- rpart(Vandal~., data=wikiTrain3, method="class")
prp(wikiCART3)

pred3 <- predict(wikiCART3, newdata=wikiTest3)[,2]
table(wikiTest3$Vandal, pred3 >= 0.5)


(595+241)/nrow(wikiTest2)

#------------------------------------------------------------------------------
# automating reviews in medicine

clinicalTrial <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
str(clinicalTrial)

max(sapply(clinicalTrial$abstract, FUN= nchar))

sum(clinicalTrial$abstract == "")

min(sapply(clinicalTrial$title, FUN= nchar))
which(nchar(clinicalTrial$title)==28)
clinicalTrial[1258,]
library(tm)

corpusTitle    <- Corpus(VectorSource(clinicalTrial$title))
corpusAbstract <- Corpus(VectorSource(clinicalTrial$abstract))

corpusTitle    <- tm_map(corpusTitle,    tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

corpusTitle    <- tm_map(corpusTitle,    PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)
corpusTitle[[1]][1]
corpusAbstract[[1]][1]

corpusTitle    <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

corpusTitle    <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle    <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

dtmTitle     <- DocumentTermMatrix(corpusTitle)
dtmAbstract  <- DocumentTermMatrix(corpusAbstract)

findFreqTerms(dtmTitle, lowfreq=20)

dtmTitle    <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

dfTitle    <- as.data.frame(as.matrix(dtmTitle))
dfAbstract <- as.data.frame(as.matrix(dtmAbstract))
str(dfTitle)
sort(colSums(dfAbstract))


colnames(dfTitle)    = paste0("T", colnames(dfTitle))
colnames(dfAbstract) = paste0("A", colnames(dfAbstract))

dtm = cbind(dfTitle, dfAbstract)

dtm$trial <- clinicalTrial$trial
str(dtm)
dim(dtm)

set.seed(144)
spl = sample.split(dtm$trial, 0.7)

train = subset(dtm, spl == TRUE)
test  = subset(dtm, spl == FALSE)

table(train$trial)
730/nrow(train)

trialCART <- rpart(trial ~ . , data=train, method="class")
prp(trialCART)

pred <- predict(trialCART)
max(pred[,2])

summary(pred)
table(train$trial, pred[,2] >= 0.5)

TP <- 441
TN <- 631
FP <- 99
FN <- 131
Accuracy <- (TP+TN)/nrow(train)
Sensitivity <- TP / (TP+FN)
Specificity <- TN / (TN+FP)
Sensitivity
Specificity

# test
predtest <- predict(trialCART, newdata=test)
table(test$trial, predtest[,2] >= 0.5)
(261+162)/nrow(test)

library(ROCR)
predROC <- prediction(predtest[,2], test$trial)  # a ROCR function
perf <- performance(predROC, "tpr", "fpr")           # a ROCR function
plot(perf)

# exercise:
AUC <- as.numeric(performance(predROC, "auc")@y.values)

#------------------------------------------------------------------------------
# ham spam 1
emails <- read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)

table(emails$spam)
substr(emails$text,1,10)
       
max(sapply(emails$text, FUN= nchar))
which(nchar(emails$text) == min(sapply(emails$text, FUN= nchar)))
emails[1992,]

library(tm)
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)


corpus <-  tm_map(corpus, removeWords, stopwords("english"))
corpus <-  tm_map(corpus, stemDocument)
corpus
dtm <- DocumentTermMatrix(corpus)
dtm

#findFreqTerms(dtm, lowfreq=20)

# beware to the trick.
spdtm  <- removeSparseTerms(dtm, 0.95)
spdtm 

emailsSparse <- as.data.frame(as.matrix(spdtm))
names(emailsSparse) <- make.names(names(emailsSparse))

sort(colSums(emailsSparse))

emailsSparse$spam <- emails$spam
sort(colSums(emailsSparse[emailsSparse$spam == 0,]))
sort(colSums(emailsSparse[emailsSparse$spam == 1,]))

emailsSparse$spam <- as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)

train = subset(emailsSparse, spl == TRUE)
test  = subset(emailsSparse, spl == FALSE)
library(rpart)
library(rpart.plot)

spamGLM <- glm(spam ~ ., data=train, family=binomial)

spamCART <- rpart(spam ~ ., data=train, method="class")
prp(spamCART)

library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data=train)

predGLM  <- predict(spamGLM, type="response")
predCART <- predict(spamCART)[,2]
predRF   <- predict(spamRF, type="prob")[,2]
sum(predGLM < 0.00001)
sum(predGLM > 0.99999)

length(predGLM)
4000 - 3046 - 954

#
summary(spamGLM)
prp(spamCART)
#

## train GLM
table(train$spam, predGLM >=0.5)
(3052+954)/nrow(train)

library(ROCR)
predROC <- prediction(predGLM, train$spam)  # a ROCR function
perf <- performance(predROC, "tpr", "fpr")           # a ROCR function
plot(perf)

# AUC
AUC <- as.numeric(performance(predROC, "auc")@y.values)



## train CART
table(train$spam, predCART >=0.5)
(2885+894)/nrow(train)

predROCCART <- prediction(predCART, train$spam)  # a ROCR function
perfCART <- performance(predROCCART, "tpr", "fpr")           # a ROCR function
plot(perfCART)

AUC <- as.numeric(performance(predROCCART, "auc")@y.values)

## train RF
table(train$spam, predRF >=0.5)
(3013+914)/nrow(train)

predROCRF <- prediction(predRF, train$spam)  # a ROCR function
perfRF <- performance(predROCRF, "tpr", "fpr")           # a ROCR function
plot(perfRF)

AUC <- as.numeric(performance(predROCRF, "auc")@y.values)

## test predictions
predGLMtest  <- predict(spamGLM, newdata=test, type="response")
predCARTtest <- predict(spamCART, newdata=test)[,2]
predRFtest   <- predict(spamRF, newdata=test, type="prob")[,2]

# test GLM
predGLMtest  <- predict(spamGLM, newdata=test, type="response")
table(test$spam, predGLMtest >=0.5)
(1257+376)/nrow(test)

predROCCGLM <- prediction(predGLMtest, test$spam)  # a ROCR function
perftestGLM <- performance(predROCCGLM, "tpr", "fpr")           # a ROCR function
plot(perftestGLM)

AUC <- as.numeric(performance(predROCCGLM, "auc")@y.values)

# test CART
table(test$spam, predCARTtest >=0.5)
(1228+386)/nrow(test)

predROCCART  <- prediction(predCARTtest, test$spam)  # a ROCR function
perftestCART <- performance(predROCCART, "tpr", "fpr")           # a ROCR function
plot(perftestCART)

AUC <- as.numeric(performance(predROCCART, "auc")@y.values)

# test RF
table(test$spam, predRFtest >=0.5)
(1290+386)/nrow(test)

predROCRF <- prediction(predRFtest, test$spam)  # a ROCR function
perftestRF <- performance(predROCRF, "tpr", "fpr")           # a ROCR function
plot(perftestRF)

AUC <- as.numeric(performance(predROCRF, "auc")@y.values)
