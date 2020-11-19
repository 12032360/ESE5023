library(tidyr)
library(dplyr)
library(ggplot2)
load_data <- read.csv("D:disk/ESE5023/load.csv", header=T)
load_data_1<-as_tibble(load_data)
#1.1
boxplot(January~Hour,data=load_data_1,
        xlab = "Hour",
        ylab = "January",
        main = "Hourly J",
        cex = 2,
        col = "orange",
        border = "darkgreen")
1.2
J<-ts(load_data_1$June,start =0,end=23,frequency = 24)
plot(J,type="l",
     xlab = "hour",
     ylab = "J",
     main="hour-J",
     cex=2,
     col="green",)
#1.3
January_1<-load_data_1 %>% 
  pull(January)
hist(January_1,
        xlab = "data",
        breaks=20,
        main = "Number of observations",
        col = "dodgerblue",
        border = "red")
box(lwd=2,col="green")
#1.4
plot(January~Hour,data=load_data_1,
     xlab="Hour",
     ylab="data",
     main="Hour vs data",
     pch="+",
     cex=2,
     col="green")
load_data_1 %>% 
ggplot( aes(x=Hour, y=January) ) + 
  geom_point()+geom_smooth()+
  labs(title="Hour vs data",x="Hour",y="January")+
  theme_bw()+
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20)) + 
  scale_color_discrete(name="C")
#1.5
library(fields)
library(maps)
library(RNetCDF)
ex.nc<-open.nc("D:disk/ESE5023/air.mon.ltm.nc")
print.nc(ex.nc)
Lat<-var.get.nc(ex.nc, "lat")
Lon       <- var.get.nc(ex.nc, "lon")
Air_T     <- var.get.nc(ex.nc, "air") 
Lat <- rev(Lat)
Air_T_May <- array(NA,dim=c(length(Lon), length(Lat)))
for(row in 1:length(Lat)){
  Air_T_May[,row] <- Air_T[, (length(Lat)+1-row),1 ]
}
par(mar=c(4.5,3,2,1))
image.plot(Lon,Lat,Air_T_May,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="Surface Temperature [degC]",cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("Long term (1800-2020) mean surface temperature in May."),
      cex.main=1,font.main=2)
map('world',add=T,lwd=0.75,col="black")
box(lwd=2)