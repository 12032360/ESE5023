a <- read.csv(file = "123456.csv", header = T,encoding = "UTF-8")
b<-a$Dissolved.Oxygen..mg.L.
b<-as.numeric(b)
b[which(b>9999)]<-9999
c<-a$Dates
date<-as.Date(c)
plot(date,b,lwd=0.05,type="l",col="red")
mean(b)
summary(b)
var(b)