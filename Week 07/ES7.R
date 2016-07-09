setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 07")
Sys.setlocale("LC_ALL", "C")
library(ggplot2)
library(maps)
library(ggmap)

statesMap <- map_data("state")
str(statesMap)
# 1.1, 1.2
unique(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "red") 

#2.1
polling <- read.csv("PollingData_Imputed.csv")
Train <- subset(polling, Year <= 2008)
Test  <- subset(polling, Year > 2008)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
sum(predictionDataFrame$TestPredictionBinary)
mean(predictionDataFrame$TestPrediction)

# 2.2, 2.3
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

# 2.4, 2.5
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = "legend", 
                      breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = "legend", 
                      breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")
# 3.1
predictionMap[predictionMap$region == "florida",]

#4
?geom_polygon
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", alpha= .3) + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = "legend", 
                      breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")

# linetype, size, alpha
#-------------------------------------------------------------------------
# 1.1
edges <- read.csv("edges.csv")
users <- read.csv("users.csv")
nrow(users)
usersNotAlone <- length(unique(c(edges$V1, edges$V2)))
(nrow(edges)*2)/nrow(users)

# 1.2
head(users)
table(users$school, users$locale)

table(users$gender, users$school)

#install.packages("igraph")
library(igraph)
?graph.data.frame

# 2.1
g = graph.data.frame(edges, FALSE, users) 
plot(g, vertex.size=5, vertex.label=NA)

deg <- degree(g)
sum(deg > 9)

# 2.4
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

max(V(g)$size)
min(V(g)$size)

# 3.1
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

# 3.2
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

# 3.3
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

?igraph.plotting
plot(g, vertex.label=NA, edge.width=3)
#-------------------------------------------------------------------------
# 1.1
library(tm)
tweets <- read.csv("tweets.csv", stringsAsFactors = F)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

freq = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(freq))


#2.1
#install.packages("wordcloud")
library(wordcloud)

sort(colSums(allTweets))
colnames(allTweets)
?wordcloud
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

#2.4
  tweets <- subset(tweets, Avg <=-1)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))
freq = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(freq))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), random.order = F)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), rot.per = .5)


library(RColorBrewer)
?brewer.pal
display.brewer.all()

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), min.freq = 10, random.color=F, colors=brewer.pal(9, "Purples") )


