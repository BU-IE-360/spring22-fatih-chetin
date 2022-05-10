library(lubridate)  #for date manipulation
library(xts) # for time series operations
library(corrplot) # for correlation analysis
library(forecast) # for residual analysis


data_all<-read.csv("IE360_Spring22_HW2_data.csv")


data_all$Quarter<-gsub('_',' ',data_all$Quarter)

data_all$Quarter<-as.yearqtr(data_all$Quarter)


data_all$Unleaded.Gasoline.Sale..UGS.<-as.numeric(gsub(' ','',data_all$Unleaded.Gasoline.Sale..UGS.))
data_all$X..LPG.Vehicles..NLPG.<-as.numeric(gsub(' ','',data_all$X..LPG.Vehicles..NLPG.))
data_all$X..Unleaded.Gasoline.Vehicles..NUGV.<-as.numeric(gsub(' ','',data_all$X..Unleaded.Gasoline.Vehicles..NUGV.))
data_all$GNP.Agriculture<-as.numeric(gsub(' ','',data_all$GNP.Agriculture))
data_all$GNP.Commerce<-as.numeric(gsub(' ','',data_all$GNP.Commerce))
data_all$GNP.Total<-as.numeric(gsub(' ','',data_all$GNP.Total))


data_ts<-xts(x=data_all[-1],order.by=data_all$Quarter,frequency=4)

data_ts$trend<-1:32


data_ts
storage.mode(data_ts)<-"double"




plot(data_ts$Unleaded.Gasoline.Sale..UGS.)


corVals <- cor(data_ts, use="pairwise.complete.obs") 

corrplot.mixed(corVals, tl.col="black", tl.pos = "lt")

acf(as.numeric(data_ts$Unleaded.Gasoline.Sale..UGS.),na.action = na.pass)


#only with trend

model1<-lm(Unleaded.Gasoline.Sale..UGS.~trend,data=data_ts)

summary(model1)

#adding seasonality

data_ts$quart<-rep(seq(1:4),8)

model2<-lm(Unleaded.Gasoline.Sale..UGS.~trend+X..of.Diesel.Gasoline.Vehicles..NDGV.+GNP.Agriculture+as.factor(quart),data=data_ts)

summary(model2)

#trying the ind. variables with high correlation

model3<-lm(Unleaded.Gasoline.Sale..UGS.~trend+as.factor(quart)+X..Unleaded.Gasoline.Vehicles..NUGV.,data=data_ts)

summary(model3)


model4<-lm(Unleaded.Gasoline.Sale..UGS.~trend+as.factor(quart)+X..LPG.Vehicles..NLPG.,data=data_ts)

summary(model4)


model5<-lm(Unleaded.Gasoline.Sale..UGS.~trend+as.factor(quart)+X..LPG.Vehicles..NLPG.+Price.of.Diesel.Gasoline..PG.,data=data_ts)

summary(model5)


model6<-lm(Unleaded.Gasoline.Sale..UGS.~trend+as.factor(quart)+X..LPG.Vehicles..NLPG.+Price.of.Diesel.Gasoline..PG.+X..of.Diesel.Gasoline.Vehicles..NDGV.,data=data_ts)

summary(model6)  


model7<-lm(Unleaded.Gasoline.Sale..UGS.~trend+as.factor(quart)+X..LPG.Vehicles..NLPG.+Price.of.Diesel.Gasoline..PG.+X..of.Diesel.Gasoline.Vehicles..NDGV.+Price.of.Unleaded.Gasoline..PU.,data=data_ts)

summary(model7)


model8<-lm(Unleaded.Gasoline.Sale..UGS.~trend+as.factor(quart)+X..LPG.Vehicles..NLPG.+Price.of.Diesel.Gasoline..PG.+X..of.Diesel.Gasoline.Vehicles..NDGV.+Price.of.Unleaded.Gasoline..PU.+GNP.Agriculture,data=data_ts)


summary(model8)



#adding lagged variable

data_all$residual_lag1<-c(0,model8$residuals,0,0,0)

data_ts<-xts(x=data_all[-1],order.by=data_all$Quarter,frequency=4)

data_ts$trend<-1:32

data_ts$quart<-rep(seq(1:4),8)

storage.mode(data_ts)<-"double"

model9<-lm(Unleaded.Gasoline.Sale..UGS.~trend+as.factor(quart)+X..LPG.Vehicles..NLPG.+Price.of.Diesel.Gasoline..PG.+X..of.Diesel.Gasoline.Vehicles..NDGV.+Price.of.Unleaded.Gasoline..PU.+residual_lag1,data=data_ts)

summary(model9)

checkresiduals(model9)

final <- xts(x = data.frame(data_ts$Unleaded.Gasoline.Sale..UGS.[1:32],predict(model9, data_ts)), order.by = data_all$Quarter[1:32], frequency = 4)
colnames(final) <- c("Real", "Predicted")
plot(final,
     legend.loc = "topleft",
     main = "Unleaded Gasoline Sale vs. Time",
     minor.ticks = "years",
     grid.ticks.on = "years",
     yaxis.right = FALSE, col = c("black","magenta"),
     grid.ticks.lty = 3)


prediction<-predict(model9, data_ts)
prediction[29:32]
