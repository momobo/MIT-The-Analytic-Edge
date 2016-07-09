# get list of questions i.e. all columns starting with a 'Q'
questions = grep("^Q",names(train))

# loop over all the questions
for (i in questions) {
  # convert the ith question, note double brackets [[ notation ...
  train[[i]] = as.numeric(train[[i]]) - 1
}

#-------------------------------


library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)



#---------------------------------


x <- as.factor(c(1,2,NA,NA,"aa",1))
x1 <- replace(x,which(is.na(x)),"umpf")

#-----------------------------------


library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data=train)

# different prediction in different algorithm
predGLM  <- predict(spamGLM, type="response")
predCART <- predict(spamCART)[,2]
predRF   <- predict(spamRF, type="prob")[,2]
sum(predGLM < 0.00001)
sum(predGLM > 0.99999)

#-------------------------------------------------------
for(col in names(train)){
  levels(train[,col])[levels(train[,col])==""] <- "Unk"
}
for(col in names(test)){
  levels(test[,col])[levels(test[,col])==""] <- "Unk"
}
#-------------------------------------------------------

i <- is.na(A) | is.na(B)
dist(rbind(A[!i], B[!i])) * sqrt(length(A) / length(A[!i]))

#--------------------------------------------------------
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
  dist <- dist(rbind(A[!i], B[!i])) * sqrt(length(A) / length(A[!i]))
  return(dist)
}

for(i in 2:1){print(i)}
q <- mat()
# distWithNA(x.train[1,],x.train[2,])
# A <- x.train[1,]
# B <- x.train[2,]
# dist(rbind(A[!i], B[!i]))
# my.list <- x.train[1:2,]
# my.function <- distWithNA
# w <- my.list[j]
# str(w)
# y <- x.train[1,]
