Met_Data       <- read.csv(file = "2281305.csv", header = T)
BaoAn_T        <- Met_Data$VIS
Obs_Time       <- Met_Data$DATE
BaoAn_T_value  <- substr(BaoAn_T,1,6)
BaoAn_T_flag   <- substr(BaoAn_T,8,12)
BaoAn_T_value2 <- as.numeric(BaoAn_T_value)
BaoAn_T_value2[which(BaoAn_T_value2>160000)] <- NA
BaoAn_T_value2[which(BaoAn_T_flag!="1,N,1")] <-NA
Obs_Time2      <- as.Date(Obs_Time)
plot(Obs_Time2, BaoAn_T_value2, lwd=0.5, type="l", col="blue")


Daily <- unique(Obs_Time2)
Daily_a <-c()
  for(i in Daily){
    id<-which(Obs_Time2==i)
    Daily_max <- max(BaoAn_T_value2[id],na.rm =T)
    Daily_a <- c(Daily_a,Daily_max)
  }
Years <- substr(Daily,1,4)
Years1 <-as.numeric(Years)
Years2 <-unique(Years1)
for(j in Years2){
  Year_vis <-Daily_a[which(j==Years1)]
  hist(Year_vis,breaks = c(0,5000,10000,15000,20000,25000,30000))
}