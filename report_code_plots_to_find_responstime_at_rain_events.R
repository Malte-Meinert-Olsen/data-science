#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Plots to find time in sewers and to ammonium and nitrate peak
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Setup 

source("report_code_setup.R")

#Overview of the rain fall

data_hour %>% autoplot(rainfall_mm)

#To overview the data and find suitable periods

gridExtra::grid.arrange(data_hour %>% 
                          filter_index("2018") %>% 
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          filter_index("2019") %>% 
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          filter_index("2020") %>% 
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          filter_index("2021") %>% 
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          filter_index("2022") %>% 
                          autoplot(rainfall_mm)
)



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Rain in 2018
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#To find days of interrest
gridExtra::grid.arrange(data_hour %>% 
                          filter_index("2018-07") %>% 
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          filter_index("2018-08") %>% 
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          filter_index("2018-09") %>% 
                          autoplot(rainfall_mm)
)


#Zooming in on the day
data_hour %>% 
  filter_index("2018-07"~"2018-09") %>% 
  autoplot(rainfall_mm)


#28th august 2018
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2018-07-28 20:00"~"2018-07-29 00:00") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2018-07-28 20:00"~"2018-07-29 00:00") %>%
                          autoplot(flow_effluent_m3_h),
                        data_one_min %>% 
                          filter_index("2018-07-28 20:00"~"2018-07-29 20:00") %>%
                          ggplot(aes(time_one_min)) + 
                          geom_line(aes(y = ammonium_effluent_mg_L)) +
                          theme(legend.position="none")
)

#Appendix picture
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2018-07-28 20:00"~"2018-07-28 21:00") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2018-07-28 20:00"~"2018-07-28 22:00") %>%
                          autoplot(flow_effluent_m3_h)
)



#9th of september 2018

data_hour %>% 
  filter_index("2018-09-07") %>% 
  autoplot(rainfall_mm)


gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2018-09-07") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2018-09-07") %>%
                          autoplot(flow_effluent_m3_h),
                        data_one_min %>% 
                          filter_index("2018-09-07") %>%
                          ggplot(aes(time_one_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          theme(legend.position="none")
)

#Appendix picture
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2018-09-07 10:00"~"2018-09-07 14:00") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2018-09-07 10:00"~"2018-09-07 14:00") %>%
                          autoplot(flow_effluent_m3_h)
                        )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Rain in 2019
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


#2019-08-27
data_hour %>% 
  filter_index("2019-08-27"~"2019-08-28") %>% 
  autoplot(rainfall_mm)

#Appendix picture
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2019-08-27 17:00"~"2019-08-27 19:00") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2019-08-27 17:00"~"2019-08-27 19:00") %>%
                          autoplot(flow_effluent_m3_h)
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Rain in 2020
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#19th June 2020
data_hour %>% 
  filter_index("2020-06-19") %>% 
  autoplot(rainfall_mm)


#Appendix picture
gridExtra::grid.arrange(data_five_min %>% 
                          filter_index("2020-06-19 11:00"~"2020-06-19 14:00") %>% 
                          autoplot(rainfall_mm),
                        data_five_min %>% 
                          filter_index("2020-06-19 11:00"~"2020-06-19 14:00") %>%
                          autoplot(flow_effluent_m3_h)
)



#18th of august 2020
data_hour %>% 
  filter_index("2020-08-18") %>% 
  autoplot(rainfall_mm)

#Appendix picture
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2020-08-18 11:00"~"2020-08-18 14:00") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2020-08-18 11:00"~"2020-08-18 14:00") %>%
                          autoplot(flow_effluent_m3_h)
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Rain in 2021
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#20th of June 2021
data_hour %>% 
  filter_index("2021-06-20") %>% 
  autoplot(rainfall_mm)

#Appendix picture
gridExtra::grid.arrange(data_five_min %>% 
                          filter_index("2021-06-20 10:00"~"2021-06-20 13:00") %>% 
                          autoplot(rainfall_mm),
                        data_five_min %>% 
                          filter_index("2021-06-20 10:00"~"2021-06-20 13:00") %>%
                          autoplot(flow_effluent_m3_h)
)
