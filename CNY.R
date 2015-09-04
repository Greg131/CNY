setwd("~/Dropbox/Finance ??conomie et analyses/Analyses/Analyse Reminbi/CNY")

if (!file.exists("../data"))       {
  dir.create("../data")
}





#fileUrl <- "https://research.stlouisfed.org/fred2/series/DEXCHUS/"
#download.file(fileUrl, destfile = "../data/fredcnyusddaily.xlsx", method = "curl")
#dateDownloaded <- date()
#dateDownloaded

library(xlsx)
?read.xlsx()
CNYUSD <- read.xlsx("../data/fredcnyusd.xls",sheetIndex=1,header=TRUE, startRow=11)
names(CNYUSD)

sum(CNYUSD$DEXCHUS==0)
CNYUSD <- CNYUSD[CNYUSD$DEXCHUS!=0,]
sum(CNYUSD$DEXCHUS==0)
# supprimer les 0
# .....

USDEUR <- read.xlsx("../data/fredusdeur.xls",sheetIndex=1,header=TRUE, startRow=11)
sum(USDEUR$DEXUSEU==0)
USDEUR <- USDEUR[USDEUR$DEXUSEU!=0,]
sum(USDEUR$DEXUSEU==0)


data = merge(CNYUSD,USDEUR,by.x="observation_date",by.y="observation_date", all = FALSE) # all pour ajouter row avec de NA si dans un seul..


names(data) <- c("observation_date", "CNYUSD", "USDEUR")
names(data)
data$CNYEUR <- data$CNYUSD*data$USDEUR
summary(data)

boxplot(data$CNYUSD, col = "blue")
hist(data$CNYUSD, col = "green") # freq est le nb de points...
rug(data$CNYUSD) # plot all the points


hist(data$USDEUR,  density = 20,col = "steelblue", breaks = 20, border = "red") # freq est le nb de points...
rug(data$USDEUR) # plot all the points




# Board of Governors of the Federal Reserve System (US), 
# China / U.S. Foreign Exchange Rate [DEXCHUS], retrieved from FRED, Federal Reserve Bank of St. Louis 
# https://research.stlouisfed.org/fred2/series/DEXCHUS/, September 4, 2015.


pch = 19


plot(data$observation_date,data$CNYUSD,pch = 4)

plot(data$observation_date,data$CNYEUR,pch = 4)

plot(data$observation_date,data$CNYEUR)

