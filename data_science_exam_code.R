

#Load the data and packages
library(tidyverse)
library(lubridate)
library(fpp3)





data_hour <- read_csv("data_hour.csv")%>% 
  select(-day) %>% 
  as_tsibble()


values_to_replace_NA_flow <- data_hour %>% 
  filter_index("2018-01-01"~"2018-01-14") %>% 
  select(flow_effluent_m3_h) %>% 
  mutate(time_hour=time_hour-24*60*60)

values_to_replace_NA_flow <- values_to_replace_NA_flow %>% 
  rename(flow_new=flow_effluent_m3_h)

#Replace the missing values

data_hour <- data_hour %>% 
  left_join(values_to_replace_NA_flow) %>%  
  mutate(flow_effluent_m3_h=if_else(is.na(flow_effluent_m3_h), 
                                    flow_new, 
                                    flow_effluent_m3_h)) %>% 
  select(-flow_new)

data_hour <- data_hour %>% 
  mutate(flow_effluent_m3_h=na.approx(flow_effluent_m3_h))



