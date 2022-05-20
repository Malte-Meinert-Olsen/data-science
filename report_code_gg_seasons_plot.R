#making seasonal plot

#Setup

source("report_code_setup.R")


#Plotting the seasonallity of the day
data_hour %>% 
  fill_gaps() %>% 
  gg_season(flow_effluent_m3_h, "day")+
  labs(x="Hours [h]",
       y='Flow of effluent ['~m^3~'/h]')


#Plotting the seasonallity of the week
data_hour %>% 
  fill_gaps() %>% 
  gg_season(flow_effluent_m3_h, "week")+
  labs(x="Days [d]",
       y='Flow of effluent ['~m^3~'/h]')


#Plotting the seasonallity of the year
data_hour %>% 
  fill_gaps() %>% 
  gg_season(flow_effluent_m3_h, "year")+
  labs(x="Months",
       y='Flow of effluent ['~m^3~'/h]')


