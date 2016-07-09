setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 01")
# Get the current directory
getwd()
# Read the csv file
mvt = read.csv("mvtWeek1.csv")
# Structure of the dataset
str(mvt)
nrow(mvt)
length(names(mvt))
max(mvt$ID)
min(mvt$Beat)

nrow(mvt[mvt$Arrest==T,])
nrow(mvt[mvt$LocationDescription=="ALLEY",])
head(mvt)
unique(substr(mvt$Date, 1, 10))


DateConvert <- as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert
table(mvt$Month)

table(mvt$Weekday)


table(mvt$Month, mvt$Arrest)
hist(as.numeric(format(mvt$Date, format="%Y")), breaks=100)

boxplot(mvt[mvt$Arrest==T,]$Date) 

mvt2001<- mvt[mvt$Year==2001,]
summary(mvt2001)
nrow(mvt2001[mvt2001$Arrest==T,])/nrow(mvt2001)

mvt2007<- mvt[mvt$Year==2007,]
nrow(mvt2007[mvt2007$Arrest==T,])/nrow(mvt2007)

mvt2012<- mvt[mvt$Year==2012,]
nrow(mvt2012[mvt2012$Arrest==T,])/nrow(mvt2012)

sort(table(mvt$LocationDescription))

top5 <- mvt[mvt$LocationDescription == "STREET" | 
              mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
              mvt$LocationDescription == "ALLEY" |
              mvt$LocationDescription == "GAS STATION" |
              mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL"  ,]

sort(table(top5$LocationDescription))

nrow(top5)

top5$LocationDescription = factor(top5$LocationDescription)

table(top5$LocationDescription, top5$Arrest)

table( top5[top5$LocationDescription=="GAS STATION",]$Weekday)
table( top5[top5$LocationDescription=="DRIVEWAY - RESIDENTIAL",]$Weekday)

# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------


# es 2
setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 01")
# Get the current directory
getwd()
# Read the csv file
IBM <- read.csv("IBMstock.csv")
GE <- read.csv("GEstock.csv")
ProcterGamble <- read.csv("ProcterGamblestock.csv")
CocaCola <- read.csv("CocaColastock.csv")
Boeing <- read.csv("Boeingstock.csv")

IBM$Date           <- as.Date(IBM$Date, "%m/%d/%y")
GE$Date            <- as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date      <- as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date <- as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date        <- as.Date(Boeing$Date, "%m/%d/%y")

tail(GE)
summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
mean(IBM$StockPrice)

sd(ProcterGamble$StockPrice)

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("1983-01-01")), lwd=2)
abline(v=as.Date(c("1984-01-01")), lwd=2)
#----------------------

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432],          col="black")
lines(IBM$Date[301:432], IBM$StockPrice[301:432],          col="blue")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],          col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],          col="orange")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1995-01-01")), lwd=2)
abline(v=as.Date(c("2006-01-01")), lwd=2)

abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-15")), lwd=2)

CocaCola[CocaCola$Date == "1997-11-01",]$StockPrice           - CocaCola[CocaCola$Date == "1997-09-01",]$StockPrice
IBM[IBM$Date == "1997-11-01",]$StockPrice                     - IBM[IBM$Date == "1997-09-01",]$StockPrice
GE[GE$Date == "1997-11-01",]$StockPrice                       - GE[GE$Date == "1997-09-01",]$StockPrice
ProcterGamble[ProcterGamble$Date == "1997-11-01",]$StockPrice - ProcterGamble[ProcterGamble$Date == "1997-09-01",]$StockPrice
Boeing[Boeing$Date == "1997-11-01",]$StockPrice               - Boeing[Boeing$Date == "1997-09-01",]$StockPrice

tapply(IBM$StockPrice, months(IBM$Date), mean, na.rm=TRUE)
mean(IBM$StockPrice)
tapply(GE$StockPrice, months(GE$Date), mean, na.rm=TRUE)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean, na.rm=TRUE)

# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------


# es 3
setwd("C:\\Users\\mmorelli\\Google Drive\\MITx 15 071x The Analytics Edge\\Week 01")
# Get the current directory
getwd()
# Read the csv file
CPS = read.csv("CPSData.csv")
summary(CPS)

sort(table(CPS$Industry)) 
sort(table(CPS$State)) 
sort(table(CPS$CountryOfBirthCode)) 

# nrow(CPS[CPS$CountryOfBirthCode==57,]) /nrow(CPS)

sort(table(CPS$Citizenship)) 

nrow(CPS[CPS$Citizenship!="Non-Citizen",]) /nrow(CPS)

(table(CPS$Race, CPS$Hispanic)) 

summary(CPS)
#table(CPS$State)

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$State, is.na(CPS$MetroAreaCode))
table(CPS$State, (CPS$MetroAreaCode))
table(CPS$Region, is.na(CPS$MetroAreaCode))
table(CPS$State, is.na(CPS$MetroAreaCode))
ss <- tapply( is.na(CPS$MetroAreaCode), CPS$State, mean, na.rm=TRUE)
sort(ss)

 
MetroAreaMap  = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)
sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic,CPS$MetroArea, mean, na.rm=TRUE))
sort(tapply(CPS$Race=="Asian",CPS$MetroArea, mean, na.rm=TRUE))

#3.6
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))[1]

#4.1
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
summary(CPS)

#4.2
sort(table(CPS$Country))

#4.3
CPS43 <- CPS[CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA" ,]
CPS43 <- CPS43[! is.na(CPS43$Country),]
mean(CPS43$Country != "United States")

#4.4
sort(tapply(CPS$Country == "India",   CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Brazil",  CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))






