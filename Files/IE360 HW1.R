require(openxlsx)   #for reading data
library(ggplot2)    # for plotting histograms
library(xts)        # extended time series library
library(corrplot)   # for correlation analysis

setwd("C:\\Users\\Win10\\OneDrive - boun.edu.tr\\Belgeler")
data_all<-read.xlsx("EVDS (4).xlsx") #reading the EVDS data

search_dollar<-read.csv("DollarSearch.csv")       #reading the google search data for dollar
search_inflation<-read.csv("InflationSearch.csv") #reading the google search data for inflation
search_card<-read.csv("Card Payment Search.csv")  #reading the google search data for card payment

search_dollar$Month<-as.yearmon(x = search_dollar$Month)        #manipulating date
search_inflation$Month<-as.yearmon(x = search_inflation$Month) #manipulating date
search_card$Month<-as.yearmon(x = search_card$Month)         #manipulating date

data_all$Tarih<-as.yearmon(x = data_all$Tarih) #manipulating date

#creating the time series object for evds data
data_ts <- xts(x = data_all[-1],order.by = data_all$Tarih,frequency = 4) 

#creating the time series object for dollar search data
dollar_ts<-xts(x=search_dollar[-1],order.by=search_dollar$Month,frequency=4)

#creating the time series object for inflation search data
inflation_ts<-xts(x=search_inflation[-1],order.by=search_dollar$Month,frequency=4)

#creating the time series object for card payment search data
card_ts<-xts(x=search_card[-1],order.by=search_dollar$Month,frequency=4)

colors=c("darkred","blueviolet","aquamarine4")



par(mfrow=c(2,1))
plot(data_ts$USD,col=colors[1],main="USD/TRY Exchange Rate")
lines(predict(lm(data_ts$USD~data_all$Tarih)))
plot(dollar_ts,col=colors[1],main="Google Search Index for 'dolar'")

par(mfrow=c(2,1))
plot(data_ts$CPI,col=colors[2],main="CPI values of Turkey")
plot(inflation_ts,col=colors[2],main="Google Search Index for 'enflasyon'")

par(mfrow=c(2,1))
plot(data_ts$Card.Payment.Index,col=colors[3],main="Card Payment Index of Turkey")
plot(card_ts,col=colors[3],main="Google Search Index for 'temassız ödeme'")

plot(zoo(data_ts),main="Card Payment Index, CPI & USD Exchange Rates vs. Time",col=colors[c(3,1,2)],lwd = c(2,2,2))


dens3<-
  ggplot(data_all,aes(x=USD))+
  geom_density(color="darkgrey",fill=colors[1],adjust=1.5, alpha=.4) +
  ggtitle("Distribution of USD/TRY")

dens3
dens1<-
  ggplot(data=data_all,aes(x=CPI))+
  geom_density(color="darkgrey",fill=colors[2],adjust=1.5, alpha=.5) +
  ggtitle("Distribution of CPI")
dens1



dens2<-
  ggplot(data_all,aes(x=Card.Payment.Index))+
  geom_density(color="darkgrey",fill=colors[3],adjust=1.5, alpha=.6) +
  ggtitle("Distribution of Card Payment Index")
dens2

boxplot(USD~as.yearqtr(Tarih),data=data_all,col=colors[1],xlab="Quarter",main="Boxplot of USD/TRY Exchange Rate")
boxplot(CPI~as.yearqtr(Tarih),data=data_all,col=colors[2],xlab="Quarter",main="Boxplot of CPI values of Turkey")
boxplot(Card.Payment.Index~as.yearqtr(Tarih),data=data_all,col=colors[3],xlab="Quarter",main="Boxplot of Card Payment Index values of Turkey")



corVals <- cor(data_all[,c(2,3,4)], use="pairwise.complete.obs") 

corrplot.mixed(corVals, tl.col="black", tl.pos = "lt")



knitr::purl(input = "IE360 HW1.Rmd", output = "IE360 HW1.R",documentation = 0)
