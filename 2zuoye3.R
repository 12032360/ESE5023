install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyr)
library(dplyr)
library(ggplot2)
wd <- read.csv("D:/disk/ESE5023/123456.csv",header = T,encoding="UTF-8")
wdd <- as_tibble(wd)
names(wdd)
wdd %>% 
  select(Dates,Dissolved.Oxygen..mg.L.) %>% 
  filter(Dissolved.Oxygen..mg.L.!="N/A") %>% 
  mutate(aaaa=as.Date(Dates)) %>% 
  #mutate(dd=format(aaaa,format="%Y-%m")) %>% 
  #select(dd,Dissolved.Oxygen..mg.L.) %>% 
  
  ggplot(aes(x=aaaa,y=as.numeric(Dissolved.Oxygen..mg.L.)))+geom_line()