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

corpusTitle    <- tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract <- tm_map(corpusTitle, content_transformer(tolower))

corpusTitle    <- tm_map(corpusTitle, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)

corpusTitle    <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusTitle, removePunctuation)

corpusTitle    <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusTitle, removeWords, stopwords("english"))







