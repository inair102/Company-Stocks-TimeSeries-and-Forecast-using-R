library(fracdiff)
library(tseries)
library(quadprog)
library(forecast)

setwd("path which contains the .csv files with stock data, eg: F:/Study/StocksData")
company_name_year1985 <- as.matrix(read.csv("table85-10.csv", header = TRUE ,sep = "," ))
company_name_year1995 <- as.matrix(read.csv("table95-10.csv", header = TRUE ,sep = "," ))
company_name_year2000 <- as.matrix(read.csv("table2000-10.csv", header = TRUE ,sep = "," ))
company_name_year2010 <- as.matrix(read.csv("table10-15.csv", header = TRUE ,sep = "," ))

head(as.matrix(company_name_year1985))
head(as.matrix(company_name_year1995))
head(as.matrix(company_name_year2000))
head(as.matrix(company_name_year2010))
ncompany_name_year1985<-as.numeric(company_name_year1985[,7], header = "")
ncompany_name_year1995<-as.numeric(company_name_year1995[,7], header = "")
ncompany_name_year2000<-as.numeric(company_name_year2000[,7], header = "")
ncompany_name_year2010<-as.numeric(company_name_year2010[,7], header = "")

rcompany_name_year1985_ac<-rev(ncompany_name_year1985[2:300])
rcompany_name_year1995_ac<-rev(ncompany_name_year1995[2:180])
rcompany_name_year2000_ac<-rev(ncompany_name_year2000[2:120])
rcompany_name_year2010_ac<-rev(ncompany_name_year2010[2:60])

rcompany_name_year1985_ac_prior <- head(c(rcompany_name_year1985_ac[1],rcompany_name_year1985_ac),length(rcompany_name_year1985_ac))
rcompany_name_year1995_ac_prior <- head(c(rcompany_name_year1995_ac[1],rcompany_name_year1995_ac),length(rcompany_name_year1995_ac))
rcompany_name_year2000_ac_prior <- head(c(rcompany_name_year2000_ac[1],rcompany_name_year2000_ac),length(rcompany_name_year2000_ac))
rcompany_name_year2010_ac_prior <- head(c(rcompany_name_year2010_ac[1],rcompany_name_year2010_ac),length(rcompany_name_year2010_ac))

rcompany_name_year1985_ac_rtns<-rcompany_name_year1985_ac/rcompany_name_year1985_ac_prior-1
rcompany_name_year1995_ac_rtns<-rcompany_name_year1995_ac/rcompany_name_year1995_ac_prior-1
rcompany_name_year2000_ac_rtns<-rcompany_name_year2000_ac/rcompany_name_year2000_ac_prior-1
rcompany_name_year2010_ac_rtns<-rcompany_name_year2010_ac/rcompany_name_year2010_ac_prior-1

cumprod(rcompany_name_year1985_ac_rtns+1)
tail(cumprod(rcompany_name_year1985_ac_rtns+1),1)
cumprod(rcompany_name_year1995_ac_rtns+1)
tail(cumprod(rcompany_name_year1995_ac_rtns+1),1)
cumprod(rcompany_name_year2000_ac_rtns+1)
tail(cumprod(rcompany_name_year2000_ac_rtns+1),1)
cumprod(rcompany_name_year2010_ac_rtns+1)
tail(cumprod(rcompany_name_year2010_ac_rtns+1),1)

TS85 <- ts(rcompany_name_year1985_ac_rtns,frequency=12,start=c(1985,10))
TS95 <- ts(rcompany_name_year1995_ac_rtns,frequency=12,start=c(1995,10))
TS2000 <- ts(rcompany_name_year2000_ac_rtns,frequency=12,start=c(2000,10))
TS2010 <- ts(rcompany_name_year2010_ac_rtns,frequency=12,start=c(2000,15))

hw_company_name_year85_ac_rtns_fit <- HoltWinters(TS85)
hw_company_name_year95_ac_rtns_fit <- HoltWinters(TS95)
hw_company_name_year2000_ac_rtns_fit <- HoltWinters(TS2000)
hw_company_name_year2010_ac_rtns_fit <- HoltWinters(TS2010)

plot.ts(hw_company_name_year85_ac_rtns_fit$fitted)
plot.ts(hw_company_name_year95_ac_rtns_fit$fitted)
plot.ts(hw_company_name_year2000_ac_rtns_fit$fitted)
plot.ts(hw_company_name_year2010_ac_rtns_fit$fitted)

hw_company_name_year85_ac_rtns_fc <- forecast.HoltWinters(hw_company_name_year85_ac_rtns_fit, h = 60)
hw_company_name_year95_ac_rtns_fc <- forecast.HoltWinters(hw_company_name_year95_ac_rtns_fit, h = 60)
hw_company_name_year2000_ac_rtns_fc <- forecast.HoltWinters(hw_company_name_year2000_ac_rtns_fit, h = 60)
hw_company_name_year2010_ac_rtns_fc <- forecast.HoltWinters(hw_company_name_year2010_ac_rtns_fit, h = 60)

hw_company_name_year85_ac_rtns_fc
hw_company_name_year95_ac_rtns_fc
hw_company_name_year2000_ac_rtns_fc
hw_company_name_year2010_ac_rtns_fc

plot.forecast(hw_company_name_year85_ac_rtns_fc)
plot.forecast(hw_company_name_year95_ac_rtns_fc)
plot.forecast(hw_company_name_year2000_ac_rtns_fc)
plot.forecast(hw_company_name_year2010_ac_rtns_fc)

plot(TS85, main = "Time Series for All", xlab = "Year", ylab = "Stocks", col = "green")
lines(TS95, col="red")
lines(TS2000, col="purple")
lines(TS2010, col="yellow")
legend(1987,0.4, c("1985-10", "1995-10", "2000-10", "2010-15"), c("green", "red", "purple", "yellow"))

plot(hw_company_name_year85_ac_rtns_fc,main = "Forecast for All", xlab = "Year", ylab = "Stocks", 
     col = "green")
lines(hw_company_name_year95_ac_rtns_fc$x, col="red")
lines(hw_company_name_year2000_ac_rtns_fc$x, col="purple")
lines(hw_company_name_year2010_ac_rtns_fc$x, col="yellow")
legend(1987,0.4, c("1985-10", "1995-10", "2000-10", "2010-15"), c("green", "red", "purple", "yellow"))