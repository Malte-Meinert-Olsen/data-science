test <- read.csv("data/data_hour.csv")


test <- data_hour %>% 
  select(time_hour,rainfall_mm, flow_effluent_m3_h)


test <- test %>% 
  filter_index("2020-02")
test <- test %>% 
  mutate(test_rain=rollapplyr(rainfall_mm, width=1*24, FUN=sum, partial=T))


test %>% autoplot(rainfall_mm)+
  geom_line(aes(y=test_rain,col="RED"))



#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)


test<- test %>% 
  select(-day) %>% 
  mutate(time_hour=ymd_hms(time_hour)) %>% 
  as_tsibble(index=time_hour)



test %>% 
  autoplot(flow_effluent_m3_h) +
  geom_line(aes(y=rainfall_mm),col="Red", alpha=0.5)




test_fcm <- function(df,av=0.5){
  df %>% 
    autoplot(flow_effluent_m3_h)+
    geom_line(aes(rainfall_mm), col="Red", alpha=av)
}

test %>% test_fcm()


