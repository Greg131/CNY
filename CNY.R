setwd("~/Dropbox/Finance ??conomie et analyses/Analyses/Analyse Reminbi/CNY")

if (!file.exists("../data"))       {
  dir.create("../data")
}

# Board of Governors of the Federal Reserve System (US), 
# China / U.S. Foreign Exchange Rate [DEXCHUS], retrieved from FRED, Federal Reserve Bank of St. Louis 
# https://research.stlouisfed.org/fred2/series/DEXCHUS/, September 4, 2015.

#fileUrl <- "https://research.stlouisfed.org/fred2/series/DEXCHUS/"
#download.file(fileUrl, destfile = "../data/fredcnyusddaily.xlsx", method = "curl")
#dateDownloaded <- date()
#dateDownloaded

library(xlsx)
?read.xlsx()
CNYUSD <- read.xlsx("../data/fredcnyusd.xls",sheetIndex=1,header=TRUE, startRow=11)
names(CNYUSD)
USDEUR <- read.xlsx("../data/fredusdeur.xls",sheetIndex=1,header=TRUE, startRow=11)
names(USDEUR)

# suppression des 0

sum(CNYUSD$DEXCHUS==0)
CNYUSD <- CNYUSD[CNYUSD$DEXCHUS!=0,]
sum(CNYUSD$DEXCHUS==0)

sum(USDEUR$DEXUSEU==0)
USDEUR <- USDEUR[USDEUR$DEXUSEU!=0,]
sum(USDEUR$DEXUSEU==0)

# merge pour les dates d'observation communes

data = merge(CNYUSD,USDEUR,by.x="observation_date",by.y="observation_date", all = FALSE) # all pour ajouter row avec de NA si dans un seul..
names(data) <- c("observation_date", "CNYUSD", "USDEUR")
names(data)
data$CNYEUR <- data$CNYUSD*data$USDEUR
summary(data)

head(data)
tail(data)

# analyse

# Etendue temporelle du 04/01/1999 au 28/08/2015
summary(data$observation_date)

# Densit?? d'observation
nombre_observation <- dim(data)[1]
debut <- min(data$observation_date)
fin <- max(data$observation_date)
debut
fin
fin - debut
nombre_observation
(fin - debut)[[1]]
d <- nombre_observation/(fin - debut)[[1]]
d

summary(data$CNYUSD)
summary(data$USDEUR)
summary(data$CNYEUR)

minCNYEUR <- min(data$CNYEUR)

par(mfrow = c(3,3), mar = c(4,4,2,1)) 

with(data, plot(observation_date, CNYUSD, main = "Evolution USD CNY", type = "n")) # set up sans plotter
with(data, lines(observation_date, CNYUSD, col = "blue"))


with(data, plot(observation_date, USDEUR, main = "Evolution EUR USD", type = "n")) # set up sans plotter
with(data, lines(observation_date, USDEUR, col = "blue"))


with(data, plot(observation_date, CNYEUR, main = "Evolution CNY EUR", type = "n")) # set up sans plotter
with(data, lines(observation_date, CNYEUR, col = "blue"))
with(data, abline(h=8.25, col = "red", lwd = 2, lty = 2))


boxplot(data$CNYUSD, col = "steelblue")

boxplot(data$USDEUR, col = "steelblue")

boxplot(data$CNYEUR, col = "steelblue")

hist(data$CNYUSD, density = 20,col = "steelblue", breaks = 20) # freq est le nb de points...
rug(data$CNYUSD) # plot all the points


hist(data$USDEUR, density = 20,col = "steelblue", breaks = 20) # freq est le nb de points...
rug(data$USDEUR) # plot all the points


hist(data$CNYEUR, density = 20,col = "steelblue", breaks = 20) # freq est le nb de points...
rug(data$CNYEUR) # plot all the points


dev.copy(png, file = "Synth??se CNY.png")
dev.off()

par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1)) #  initialiser

with(data, plot(observation_date, CNYUSD, main = "Evolution USD CNY", type = "n")) # set up sans plotter
with(data, lines(observation_date, CNYUSD, col = "blue"))

dev.copy(png, file = "Evolution CNY USD.png")
dev.off()


with(data, plot(observation_date, USDEUR, main = "Evolution EUR USD", type = "n")) # set up sans plotter
with(data, lines(observation_date, USDEUR, col = "blue"))


dev.copy(png, file = "Evolution EUR USD.png")
dev.off()


with(data, plot(observation_date, CNYEUR, main = "Evolution EUR CNY", type = "n")) # set up sans plotter
with(data, lines(observation_date, CNYEUR, col = "blue"))
with(data, abline(h=8.25, col = "red", lwd = 2, lty = 2))

dev.copy(png, file = "Evolution EUR CNY.png")
dev.off()

# Zoom 2015
data2015 <- data[data$observation_date > "2015-01-01",]

with(data2015, plot(observation_date, CNYUSD, main = "Evolution USD CNY", type = "n")) # set up sans plotter
with(data2015, lines(observation_date, CNYUSD, col = "blue"))

dev.copy(png, file = "Zoom ??t?? 2015 USD CNY.png")
dev.off()

with(data2015, plot(observation_date, CNYEUR, main = "Evolution CNY EUR Zoom sur 2015", type = "n")) # set up sans plotter
with(data2015, lines(observation_date, CNYEUR, col = "blue"))
with(data, abline(h=minCNYEUR, col = "red", lwd = 2, lty = 2))


dev.copy(png, file = "Zoom ??te 2015CNY EUR .png")
dev.off()


# Zoom ete 2015
dataete2015 <- data[data$observation_date > "2015-08-01",]
with(dataete2015, plot(observation_date, CNYUSD, main = "August devaluation", type = "n")) # set up sans plotter
with(dataete2015, lines(observation_date, CNYUSD, col = "blue"))

dev.copy(png, file = "Zoom aout 2015 USD CNY.png")
dev.off()

with(dataete2015, plot(observation_date, CNYEUR, main = "Evolution CNY EUR Zoom sur 2015", type = "n")) # set up sans plotter
with(dataete2015, lines(observation_date, CNYEUR, col = "blue"))
with(data, abline(h=minCNYEUR, col = "red", lwd = 2, lty = 2))


dev.copy(png, file = "Zoom aout 2015CNY EUR .png")
dev.off()

