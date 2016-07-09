setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 02")
# VIDEO 2

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
# run scored - run allowed
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

q <- data.frame(RD=c(99, 100))

res <- predict(WinsReg, newData = data.frame(RD=c(99.0)))
summary(WinsReg)
coef(WinsReg)[1]+ 99 * coef(WinsReg)[2]

# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
#--------------------
runs <- function(OBP, SLG){
  coef(RunsReg)[1] +  coef(RunsReg)[2] * OBP +  coef(RunsReg)[3] * SLG
}
runs(0.311, 0.405)

RunsAll <- lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsAll)
runsAll <- function(OOBP, OSLG){
  coef(RunsAll)[1] +  coef(RunsAll)[2] * OOBP +  coef(RunsAll)[3] * OSLG
}

runsAll(0.297, 0.37)

#-----------------
EricChavez      <- runs(0.338, 	0.540 ) 
JeremyGiambi 	  <- runs(0.391, 	0.450 ) 
FrankMenechino 	<- runs(0.369, 	0.374 )
GregMyers 	    <- runs(0.313, 	0.447 ) 
CarlosPena     	<- runs(0.361, 	0.500 ) 
EricChavez     
JeremyGiambi 	 
FrankMenechino 
GregMyers 	   
CarlosPena     


teamRank = c(1,2,3,3,4,4,4,4,5,5)





