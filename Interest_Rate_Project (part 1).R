rm(list=ls())
library(quantmod)
library(xts)
Tenors = 1:12
NumTenors = length(Tenors)
for (Currency in c("USD", "CAD")) {
  VarName = paste(Currency, "LIBOR", sep="_")
  
  for (t in 1:NumTenors) {
    if (Tenors[t] < 10) {
      symbol = paste(Currency, Tenors[t], "MTD156N", sep="") 
    } else symbol = paste(Currency, Tenors[t], "MD156N", sep="") 
    
    getSymbols(Symbols = symbol, src = "FRED")
    DF <- na.omit(data.frame(get(symbol)))
    DF$Date = row.names(DF)
    
    if (t == 1) {
      assign(VarName, DF)
    } else {
      assign(VarName, merge(get(VarName), DF, by="Date"))
    }
    cat(symbol, "is done\n")
    rm(list = c(symbol))
  }
}

rm(DF)

library(ggfortify)
library (gridExtra)

# original data
CAD = xts(CAD_LIBOR[,-1], order.by = as.Date(CAD_LIBOR[,1],format = "%Y-%m-%d"))
USD = xts(USD_LIBOR[,-1], order.by = as.Date(USD_LIBOR[,1],format = "%Y-%m-%d"))
CAD.df = data.frame(date=index(CAD), coredata(CAD))
USD.df = data.frame(date=index(USD), coredata(USD))

## ggplot - For loop for all Tenors 
for(i in 2:13){
  print(ggplot(CAD.df, aes(x=date)) + 
          geom_line(aes(y=CAD.df[,i])) + labs(title = 'Time Series plot of Canadian Libor Rates', y = i-1))
}

for(i in 2:13){
  print(ggplot(USD.df, aes(x=date)) + 
          geom_line(aes(y=USD.df[,i])) + labs(title = 'Time Series plot of US Libor Rates', y = i-1))
}

# This is for Average mean of CAD and USD lines
meanCAD = rowMeans(CAD.df[2:13])
meanUSD = rowMeans(USD.df[2:13])
datelineU = (USD.df[,1])
datelineC = (CAD.df[,1])
USDM= data.frame(datelineU,meanUSD)
CADM= data.frame(datelineC,meanCAD)

ggplot(data = USDM, aes(x=USDM[,1]))+
  geom_line(aes(y=USDM[,2]),color = 'blue')+
  xlab('Date')+ylab('Interest Rate')+
  ggtitle('Mean Interest rate all Tenors USD')

ggplot(data = CADM, aes(x=CADM[,1]))+ 
  geom_line(aes(y=CADM[,2]),color ='red')+
  xlab('Date')+ylab('Interest Rate')+
  ggtitle('Mean Interest rate all Tenors CAD')

###########################################


#Plot time series of IRs and their 1-day and 30-day increments(12 in one plot)
#IRS
plot.xts(CAD,main="CAD")
plot.xts(USD,main="USD")
#30 DAYS
plot.xts(diff(CAD[,12], lag=30),main="CAD 30 day increments")
plot.xts(diff(USD[,2], lag=30),main="USD 30 day increments")
#1 DAY
plot.xts(diff(CAD[,2]), main = "CAD 1 day increments")
plot.xts(diff(USD[,2]), main = "USD 1 day increments")

#Plot histogram of IRs and their 1-day and 30-day increments(Using one month IR for example)
#IRS
hist(CAD[,2],main="CAD one month IR", breaks = 100, xlab = "one month IR for CAD")
hist(USD[,2],main="USD one month IR", breaks = 100, xlab = "one month IR for USD")
#30 DAYS
hist(diff(CAD[,2], lag=30),main="CAD 30 days increments for one month IR", breaks = 100, xlab = "CAD 30 days increments for one month IR")
hist(diff(USD[,2], lag=30),main="USD 30 days increments for one month IR", breaks = 100, xlab = "USD 30 days increments for one month IR")
#1 DAY
hist(diff(CAD[,2]),main="CAD 1 day increments for one month IR", breaks = 100, xlab = "CAD 1 day increments for one month IR")
hist(diff(USD[,2]),main="USD 1 day increments for one month IR", breaks = 100, xlab = "USD 1 day increments for one month IR")

#average term structure
#y is the mean value for each column, x is the tenor
plot(colMeans(CAD.df[2:13]), type = 'l', xlab = 'tenors', ylab = 'Average IR')
plot(colMeans(USD.df[2:13]), type = 'l', xlab = 'tenors', ylab = 'Average IR')

#standard deviation
#y is the standard deviation for each column, x is the tenor
library(matrixStats)
plot(colSds(data.matrix(CAD.df[2:13])), type = 'l', xlab = 'Tenors')
plot(colSds(data.matrix(USD.df[2:13])), type = 'l', xlab = 'Tenors')
