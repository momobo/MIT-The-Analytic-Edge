setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Kaggle MIT")
Sys.setlocale("LC_ALL", "C")

train = read.csv("train2016.csv", na.strings = c("NA",""))
test  = read.csv("test2016.csv", na.strings = c("NA",""))

queTrain = grep("^Q",names(train))
# loop over all the questions
for (i in c(2:6,queTrain)) {
  train[[i]] = as.numeric(train[[i]]) - 1
}
queTest = grep("^Q",names(test))
# loop over all the questions
for (i in c(2:6,queTest)) {
  test[[i]] = as.numeric(test[[i]]) - 1
}

# mean is approx 1980
train$YOB <- ifelse(train$YOB > 2000 | train$YOB < 1935, NA , train$YOB )
train$YOB <- ifelse(is.na(train$YOB), 1980 , train$YOB )
test$YOB <- ifelse(test$YOB > 2000 | test$YOB < 1935, NA , test$YOB )
test$YOB <- ifelse(is.na(test$YOB), 1980 , test$YOB )
train$YOB <- train$YOB / 2000
test$YOB  <- test$YOB  / 2000


x.train <- train[ , c(2:6,8:108)]
x.test  <- test[  , c(2:107)]

#kdist <- dist(x.train)

# function per custom dist
custom.dist <- function(my.list, my.function) {
  n <- nrow(my.list)
  mat <- matrix(0, ncol = n, nrow = n)
  colnames(mat) <- rownames(mat) <- rownames(my.list)
  my.list <- as.matrix(my.list)
  for(i in 1:n) {
    for(j in 1:n) {
      if(i>j){
        mat[i,j] <- my.function(my.list[i,],my.list[j,])
      }
    }
  }
  return(as.dist(mat))
}

distWithNA <- function(A,B) { 
  i <- is.na(A) | is.na(B)
  dist <- dist(rbind(A[!i]*10, B[!i])) * sqrt(length(A) / length(A[!i]))
  return(dist)
}

  Rprof ( tf <- "log.log",  memory.profiling = TRUE )
kdist2 <- custom.dist(x.train, distWithNA)
  Rprof ( NULL ) ; print ( summaryRprof ( tf )  )


# Hierarchical clustering
cluster = hclust(kdist2, method = "ward.D") 
# Plot the dendrogram
plot(cluster)

d <- as.matrix(kdist2)
n <- nrow(x.train)
set.seed(1)
indices <- sort(sample(1:n, n * (2 / 3)))
training.set <- train[indices, 1:2]
test.set     <- train[-indices, 1:2]

training.original.labels <- train[indices, 7] 
test.original.labels     <- train[-indices, 7]

library(FastKNN)

predicted.set <- knn_test_function(training.set, test.set, d, training.original.labels, k = 3)
accuracy <- sum(predicted.set != test.original.labels)/nrow(test.set)
length(predicted.set)
length(test.original.labels)
nrow(test.set)


# 52% BAD!

summary(train)


#------------------------ old stuff

#fitrf <-rfImpute(Party ~ . -USER_ID, data=train)
modelrf <- randomForest(Party ~ . -USER_ID, data=train, type="classification")
predRF  <- predict(modelrf, type="prob", newdata=test1)[,2]
?randomForest

summary(modelrf)
# And then make predictions on the test set:


threshold = 0.5
PredTestLabels = as.factor(ifelse(predRF<threshold, "Democrat", "Republican"))

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog9.csv", row.names=FALSE,quote = FALSE)
