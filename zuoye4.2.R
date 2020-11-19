#2
library(dplyr)
install.packages("lubridate")
library(lubridate)
install.packages("forecast")
library(forecast)
library(tidyr)
library(dplyr)
library(ggplot2)
monthly_temp<-read.csv("D:disk/ESE5023/2281305.csv",header=T)
monthly_temp_1<-as_tibble(monthly_temp)
monthly_temp_2<-monthly_temp_1 %>%
  filter(substr(TMP,7,7)=="1") %>%
  mutate(Tem=(as.numeric(substr(TMP,3,5)))/10) %>% 
  mutate(yue=as.numeric(paste(substr(DATE,1,4),substr(DATE,6,7),sep=""))) %>% 
  group_by(yue) %>% 
  summarise(mean_yue_T=mean(Tem))
monthly_temp_3<-monthly_temp_2 %>% 
  filter(yue<"202009")
plot(monthly_temp_3$yue,monthly_temp_3$mean_yue_T,type="l",xlab="Date",ylab="Tem")
tem<-ts(monthly_temp_3$mean_yue_T,start=c(2010,01),end=c(2020,08),frequency=12)
plot(tem,type="l")
str(tem)
#2.2
tem_components <- decompose(tem)
plot(tem_components)
hist(tem_components$random,prob=TRUE)
curve(dnorm(x,mean=mean(tem_components$random,na.rm=T),sd=sd(tem_components$random,na.rm=T)),
      add=TRUE, col="red")
#2.3
acf(tem)
pacf(tem)
tem_1 <- diff(tem)
plot(tem_1)
acf(tem_1)
pacf(tem_1)
model_1<- auto.arima(tem)
summary(model_1)
#2.4
month_forecast  <- 2
month_in_plot   <- 30
forecast_2month <- forecast(model_1, month_forecast)
plot(forecast(model_1, month_forecast), include = month_in_plot, xlab="Time", 
     ylab="temp",type="o",lwd=2)
#预测下一月
forecast_2month$mean[1]
forecast_2month$lower[1,2]
forecast_2month$upper[1,1]
#预测下下一月
forecast_2month$mean[2]
forecast_2month$lower[2,2]
forecast_2month$upper[2,1]
#bias从monthly_temp_2看到202009的温度为29.45206。预测值为29.04255,偏差为1.39%
bias<-(29.45206-29.04255)/29.45206*100
bias
