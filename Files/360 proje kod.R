# 1. Introduction
Renewable energy is becoming more and more popular. The efficiency of solar panels is dependent on the weather, which can make them less efficient during certain times of the day. Wind turbines are also dependent on wind speed. The amount of power generated is related to the speed at which the blades rotate. If there is not enough wind then the turbine will not generate any power at all. A similar thing applies for solar power. Solar resources are highly dependent on meteorological parameters such as temperature, humidity, wind speed, cloud cover etc. The solar radiation is a function of the time of day, time of year, geographical location etc.
Solar power forecasting is challenging because of the unique features of solar radiation time series. The data exhibits a high degree of non-linearity and dependence on the previous values in the series. There are also significant variations in the amplitude and phase of each cycle, which makes it difficult to predict future cycles. On the other hand, it is very important to be able to generate robust forecasts because of the domain of our problem: energy. Solar energy cannot be stored, or compensated. Therefore the magnitude of our errors will be very significant in many aspects such as financially and in terms of sustainability. 
In this paper, methods and techniques that can be used to improve this process will be discussed.
# 1.1 Problem Description
This Project was done with the aim of forecasting the solar power production of KIVANC 2 GES for the day after. There will be a total number of 24 predictions, one for each hour of the day. Some important variables in the domain of weather are the temperature (TEMP), downward shortwave radiation flux (DSWRF), total cloud cover percentage (CLOUD_LOW_LAYER) and relative humidity (REL_HUMIDITY). Three days of test data will be used to predict the values of a week.
# 1.2 Summary of the Proposed Approach
We decided to move forward with a linear regression model since linear regression can be one of the most robust methods of forecast, although it being simpler than many. We put all of the original variables in our first model. As we moved forward, we tried to decompose the features of the data, and modified our model according to our findings. We have tried a total of 5 models, and compared the results to choose the best.
```{r}

require(data.table)
require(forecast)
library(urca)
library(MuMIn)
library(MLmetrics)
library(Metrics)
library(GGally)

todays_date=Sys.Date()-30
forecast_date=todays_date

weather<-fread("long_weather.csv")
production<-fread("production.csv")

weather$date<-as.Date(weather$date)
production$date<-as.Date(production$date)
production[, quarter:= as.factor(quarter(date))]

production[, quarter:= as.factor(quarter(date))]

daily_prod=production[,list(mean_production=mean(production,na.rm=T)),by=list(date)]

```



# 1.3 Descriptive Analysis of the Given Data
The plot of the actual data is very hard to interpret. Therefore it is reasonable to plot and observe the mean consumption. As it can be seen, a visible trend isn’t present. However, seasonality can be seen that is quarterly. When we plot a smaller range of our data, we can also see the daily seasonality.
There is not any unusual movement that cannot be understood, except a huge spike after the start of 2022. We interpreted this spike as an increase in the capacity. That is, the plant started to use additional solar panels.



# 2 Related Literature
There are two approaches that are dominant in the literature in forecasting solar power. These two approaches are the normalized clear sky model such as the one developed by Bacher et al.(2009) and neural networks with different inputs. The rationale behind the clear sky model is to create a stationary time series so that working with the data becomes easier and easier to interpret.
The neural networks approach is used to predict the solar power directly. However, our approach is more similar to the clear-sky-normalized.

```{r}
plot(daily_prod,type="l")

windows()
plot(production$production[],type="l")

windows()
```


# 3 Approach
The plot of the raw data is hard to interpret. So, we looked at the mean daily production plot.

```{r}
decomposed<-decompose(ts(daily_prod$mean_production,freq=24))
plot(decomposed)

decomposed<-decompose(ts(daily_prod$mean_production,freq=24))
plot(decomposed)


```

Here we identify the number of days to forecast.
We get the latest n_days and modify for forecasting. It
creates date,hour and production columns for the upcoming days. Then,
we match the actual data with forecasts.

```{r}
latest_available_prod_date=as.Date(max(production$date))
n_days=as.numeric(forecast_date-latest_available_prod_date)
forecasted_production=tail(production,n_days*24)
forecasted_production[,date:=date+n_days]
forecasted_production[,production:=NA]


production_with_forecast=rbind(production,forecasted_production)

forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)

production_with_forecast=production_with_forecast[order(date,hour)]
production_series=ts(production_with_forecast[!is.na(production)]$production,frequency=24)

wide_weather=dcast(weather,date+hour~variable+lat+lon,value.var='value')

production_with_weather=merge(production_with_forecast,wide_weather,by=c('date','hour'))

production_with_weather[,hour:=as.character(hour)]
train_data=production_with_weather[!is.na(production)]
test_data=production_with_weather[is.na(production)]

```




# 3.1 Linear Regression Model 1
We added all of the variables when establishing our first model.

```{r}
lm_model=lm(production~.,train_data[,-c('date'),with=F])
summary(lm_model)
checkresiduals(lm_model)

lm_forecast=predict(lm_model,test_data)
test_data[,forecasted:=as.numeric(lm_forecast)]

daily_max_production=production_with_forecast[,list(max_prod=max(production)),by=list(date)]
daily_max_production[,rolling_max:=frollapply(max_prod,30,max,na.rm=T)]

production_with_weather_capacity=merge(production_with_weather,daily_max_production,by=c('date'))
production_with_weather_capacity[,normalized_production:=production/rolling_max]

production_with_weather_capacity[,hour:=as.character(hour)]
train_data=production_with_weather_capacity[!is.na(production)]
test_data=production_with_weather_capacity[is.na(production)]
```


As can be seen from the outputs, there is a big jump starting in 2022.In order to make the data stationary, we take the highest production rate in the data, and normalize according to it, similar to a clear-sky-normalized approach.



# 3.2 Model 2
Our model 2 is the normalized version of the first model. 

```{r}
lm_model2=lm(normalized_production~.,production_with_weather_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model2)
checkresiduals(lm_model2)

```


When we look at the ACF plot, it is evident that the first lag is very dominant.
Therefore, it is worth adding lag 1 to our model to see how it responds.


# 3.3 Model 3
```{r}
production_with_weather_capacity[,lag1:=shift(normalized_production,1)]

lm_model3<-lm(normalized_production~.,production_with_weather_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model3)
checkresiduals((lm_model3))
pacf(lm_model3$residuals)
```


We can see that the value of the Adjusted R-Squared has increased significantly.
Therefore model 3 is an improvement over the second model.



# 3.4 Model 4
In a similar manner, the PACF plot of model 3 shows that the 24th lag is
significant. Therefore, we add it to our model to see how it changes the results.

```{r}
production_with_weather_capacity[,lag24:=shift(normalized_production,24)]

lm_model5<-lm(normalized_production~.,production_with_weather_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model5)
checkresiduals((lm_model5))
pacf(lm_model5$residuals)
```


There is a small improvement in the model in terms of Adjusted R-Squared and F-Statistic.
So, it is still an improvement, however a small one.


# 3.5 Model 5

Lastly, as we can see in the PACF plot of model 4, the 26th residual is the most significant.
Therefore, it can be used to explain another feature of our model, so we add it to model 4.

```{r}
production_with_weather_capacity[,lag26:=shift(normalized_production,26)]


lm_model6<-lm(normalized_production~.,production_with_weather_capacity[,-c('date','rolling_max','production','max_prod'),with=F])
summary(lm_model6)
checkresiduals((lm_model6))

pacf(lm_model5$residuals)
```


Based on different metrics, we have mixed results.
There is an improvement in terms of Adjusted R-Squared, but a decline in 
the success of our model in terms of its F-Statistic.

# 4. Results
In order to pick the best model, we need to choose a metric. We believe that MAPE and RMSE are great in determining the success of linear regression models. Based on the results of MAPE and RMSE of our models, model 5 is the best model we have.
```{r}
accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}


forecast_with_lr=function(fmla, data,forecast_data){
  fitted_lm=lm(as.formula(fmla),data)
  forecasted=predict(fitted_lm,forecast_data)
  return(list(forecast=as.numeric(forecasted),model=fitted_lm))
}


test_start=as.Date('2022-05-01')
test_end=as.Date('2022-05-31')

test_dates=seq(test_start,test_end,by='day')
test_dates

forecast_ahead=1

results=vector('list',length(test_dates))
i=1
for(i in 1:length(test_dates)){
  current_date=test_dates[i]-forecast_ahead
  
  past_data=production_with_weather_capacity[date<='2022-04-30']
  past_data=past_data[,-c('date','rolling_max','production','max_prod'),with=F]
  forecast_data=production_with_weather_capacity[date==test_dates[i]]
  
  # first lm models
  fmla='normalized_production~-1'
  forecasted=forecast_with_lr(fmla,past_data,forecast_data)
  forecast_data[,lm_prediction:=forecasted$forecast]
  
  
  results[[i]]=forecast_data
}
```



# 5. Conclusion
In this project, we have manipulated the data into something
that we could play with, and tried to find the best model that explains the behaviour
of the given data. We created, iterated and tested 5 models and chosen the best one
among all of them. All of our models were linear regression models.
Although we tried an ARIMAX (Arima with regressors) model, surprisingly it had a very low success
as opposed to the linear regression models we used. 
It can be interpreted that the complexity of a model is not directly correlated with its robustness
and prediction quality.


# 6.Citations
Bacher, P. , Madsen, H. , Nielsen, H. (2009). Online short-term solar power forecasting, 10.1016/j.solener.2009.05.016.
Hammer, A. , Heinemann, D. Lorenz, E. , Lückehe, B. (1999) Short-term forecasting of solar radiation: a statistical approach using satellite data, 10.1016/S0038-092X(00)00038-4.
Ghofrani, M. , Musaad. A, (2018). Time Series and Renewable Energy Forecasting. 10.5772/intechopen.70845

