install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyr)
library(dplyr)
library(ggplot2)
b <- read.csv("D:/disk/ESE5023/2281305.csv",header = T)
boom <- as_tibble(b)
# 下面这段程序有点问题，还在思考中
#boom %>% 
  #mutate(yue=as.Date(substr(DATE,1,10))) %>% 
  #mutate(format(yue,format="%Y-%m"))
  #mutate(speed=as.numeric(substr(WND,9,12))) %>% 
  #mutate(yue=as.Date(substr(DATE,1,10))) %>%
  #mutate(yu=format(yue,format="%Y%m")) %>% 
  #group_by(yu) %>% 
  #summarise(aspeed=mean(speed)) %>% 
  #ggplot(aes(x=yu,y=aspeed))+geom_line()
  


boom %>% 
  filter(substr(WND,1,3)!="999"&substr(WND,7,7)=="V") %>% 
  filter(substr(WND,5,5)=="1"&substr(WND,9,12)!="9999") %>% 
  filter(substr(WND,14,14)=="1") %>% 
  mutate(yue=as.numeric(paste(substr(DATE,1,4),substr(DATE,6,7),sep="")))%>% 
  mutate(speed=as.numeric(substr(WND,9,12))) %>% 
  group_by(yue) %>% 
  summarise(aspeed=mean(speed)) %>% 
  ggplot(aes(x=yue,y=aspeed))+geom_line()