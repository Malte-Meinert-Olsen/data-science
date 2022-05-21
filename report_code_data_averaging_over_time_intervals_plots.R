#Setup 

sources("report_code_setup.R")


#Plotting data from 2021-01

#Data one minute
data_one_min %>% 
  filter_index("2021-01") %>% 
  autoplot(flow_effluent_m3_h)+
  labs(x="Days [d]",
       y='Flow of effluent ['~m^3~'/h]')

#Data five minutes
data_five_min %>% 
  filter_index("2021-01") %>% 
  autoplot(flow_effluent_m3_h)+
  labs(x="Days [d]",
       y='Flow of effluent ['~m^3~'/h]')


#Data fifteen minutes
data_fifteen_min %>% 
  filter_index("2021-01") %>% 
  autoplot(flow_effluent_m3_h)+
  labs(x="Days [d]",
       y='Flow of effluent ['~m^3~'/h]')


#Data thirty minutes
data_thirty_min %>% 
  filter_index("2021-01") %>% 
  autoplot(flow_effluent_m3_h)+
  labs(x="Days [d]",
       y='Flow of effluent ['~m^3~'/h]')



#Data hourly
data_hour %>% 
  filter_index("2021-01") %>% 
  autoplot(flow_effluent_m3_h)+
  labs(x="Days [d]",
       y='Flow of effluent ['~m^3~'/h]')





