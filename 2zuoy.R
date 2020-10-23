install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyr)
library(dplyr)
library(ggplot2)
a <- read.csv("D:/disk/ESE5023/signif.txt",sep = "\t",header = T)
Sign <- as_tibble(a)
Sign %>% 
  group_by(COUNTRY) %>% 
  select(YEAR,DEATHS) %>% 
  summarise(b=sum(DEATHS))%>% 
  arrange(totaldeaths)

#1.3
Sign %>% 
  select(YEAR,EQ_PRIMARY) %>% 
  filter(EQ_PRIMARY>6.0) %>% 
  group_by(YEAR) %>% 
  summarise(earthquake_num=n())%>% 
  ggplot(aes(x=YEAR,y=earthquake_num)) + geom_line()
#1.4
CountEq_LargestEq<- function(k){
  Sign %>% 
    group_by(COUNTRY) %>%
    filter(COUNTRY=="k"&EQ_PRIMARY!="NA") %>%
    mutate(newdate=paste(YEAR,MONTH,DAY,sep='-')) %>% 
    summarise(eqnum=n(),max=newdate[which(EQ_PRIMARY==max(EQ_PRIMARY))])
}
Sign %>% 
  filter(EQ_PRIMARY!="NA") -> Sign_1

i<-1
amatrix <- matrix(ncol=3,nrow =length(unique(Sign_1$COUNTRY)))
for(k in unique(Sign_1$COUNTRY)){
  amatrix[i,]<-c(as.character(k),as.numeric(CountEq_LargestEq(k)[1,1]),as.character(CountEq_LargestEq(k)[1,2]))
  i=i+1
}
#