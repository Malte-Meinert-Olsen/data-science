test <- read.csv("data/data_hour.csv")


test <- test %>% 
  select(time_hour,rainfall_mm)


test <- test %>% 
  mutate(time_hour=ymd_hms(time_hour)) %>% 
  as_tsibble() %>% 
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
  filter_index("2020-04-01") %>% 
  autoplot(flow_effluent_m3_h) +
  geom_line(aes(y=flow_influent_m3_h),col="Red", alpha=0.5)




