#load packages
library(tidyverse)
library(fpp3)
library(lubridate)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Load the data from the csv file
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#One minute data
data_one_min <- read_csv("data_one_min.csv")
data_one_min <- data_one_min %>% 
  as_tsibble()

#Five minute data
data_five_min <- read_csv("data_five_min.csv") %>% 
  as_tsibble()

#Fifteen minute data
data_fifteen_min <- read_csv("data_fifteen_min.csv") %>% 
  as_tsibble()

#Thirty minute data
data_thirty_min <- read_csv("data_thirty_min.csv") %>% 
  as_tsibble()

#Hourly data
data_hour <- read_csv("data_hour.csv") %>% 
  as_tsibble()


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Justify the problem ammonium in the effluent
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Just the effluent concentration of ammonium
data_hour %>% autoplot(ammonium_effluent_mg_L)

#Just the effluent concentration of nitrate
data_hour %>% 
  #Removing the outliers over 250 mg/L
  filter(nitrate_effluent_mg_L<250) %>%  
  autoplot(nitrate_effluent_mg_L)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Plotting the ammonium concentration, rain fall, and flow for all times
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Hour based ammonium all times + rain
gridExtra::grid.arrange(data_hour %>% 
                          autoplot(ammonium_effluent_mg_L)+
                            geom_hline(yintercept = 2, 
                                       col="Red"),
                        data_hour %>% 
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          autoplot(flow_effluent_m3_h)
                        )
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Plotting the ammonium concentration, rain fall, and flow zoom 2021
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Hour based ammonium zoom 2021 + rain + flow
gridExtra::grid.arrange(data_hour %>%
                          filter_index("2021") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_hour %>% 
                          filter_index("2021") %>%
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          filter_index("2021") %>%
                          autoplot(flow_effluent_m3_h)
)




#Hour based ammonium zoom 2021-09 + rain + flow
gridExtra::grid.arrange(data_hour %>%
                          filter_index("2021-09") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_hour %>% 
                          filter_index("2021-09") %>%
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          filter_index("2021-09") %>%
                          autoplot(flow_effluent_m3_h)
)


gridExtra::grid.arrange(data_hour %>%
                          filter_index("2021-09") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_hour %>% 
                          filter_index("2021-09") %>%
                          autoplot(ammonium_AN_mg_L),
                        data_hour %>% 
                          filter_index("2021-09") %>%
                          autoplot(flow_effluent_m3_h)
)


gridExtra::grid.arrange(data_hour %>%
                              filter_index("2021-09-15"~"2021-09-16") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_hour %>% 
                          filter_index("2021-09-15"~"2021-09-16") %>%
                          autoplot(ammonium_AN_mg_L),
                        data_hour %>% 
                          filter_index("2021-09-15"~"2021-09-16") %>%
                          autoplot(flow_effluent_m3_h)
)



gridExtra::grid.arrange(data_hour %>%
                          filter_index("2021-09-09"~"2021-09-16") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_hour %>% 
                          filter_index("2021-09-09"~"2021-09-16") %>%
                          autoplot(ammonium_AN_mg_L),
                        data_hour %>% 
                          filter_index("2021-09-09"~"2021-09-16") %>%
                          autoplot(flow_effluent_m3_h)
)

gridExtra::grid.arrange(data_hour %>%
                          filter_index("2021-09-11"~"2021-09-16") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_hour %>% 
                          filter_index("2021-09-11"~"2021-09-16") %>%
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          filter_index("2021-09-11"~"2021-09-16") %>%
                          autoplot(flow_effluent_m3_h)
)


gridExtra::grid.arrange(data_hour %>%
                          filter_index("2021-09-11") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_hour %>% 
                          filter_index("2021-09-11") %>%
                          autoplot(rainfall_mm),
                        data_hour %>% 
                          filter_index("2021-09-11") %>%
                          autoplot(flow_effluent_m3_h)
)


gridExtra::grid.arrange(data_hour %>%
                          filter_index("2021-09-09"~"2021-09-16") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_hour %>% 
                          filter_index("2021-09-09"~"2021-09-16") %>%
                          autoplot(flow_AN_m3_h),
                        data_hour %>% 
                          filter_index("2021-09-09"~"2021-09-16") %>%
                          autoplot(flow_effluent_m3_h)
                        )



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Plotting the ammonium concentration, rain fall, and flow 
#zoom 2021-07 to 2021-09
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Minute based ammonium zom on 2021-07 to 2021-09 + rain + flow
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2021-07"~"2021-09") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_one_min %>% 
                          filter_index("2021-07"~"2021-09") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2021-07"~"2021-09") %>% 
                          autoplot(flow_effluent_m3_h)
                        )

#Thirty based ammonium zom on 2021-07 to 2021-09 + rain + flow
gridExtra::grid.arrange(data_thirty_min %>% 
                          filter_index("2021-07"~"2021-09") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_thirty_min %>% 
                          filter_index("2021-07"~"2021-09") %>% 
                          autoplot(rainfall_mm),
                        data_thirty_min %>% 
                          filter_index("2021-07"~"2021-09") %>% 
                          autoplot(flow_effluent_m3_h)
                        
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Plotting the ammonium concentration, rain fall, and flow 
#zoom 2021-08-15 to 2021-09-03
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Minute based ammonium zom on 2021-08-15 to 2021-09-03 + rain + flow
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2021-08-15"~"2021-09-03") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_one_min %>% 
                          filter_index("2021-08-15"~"2021-09-03") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2021-08-15"~"2021-09-03") %>% 
                          autoplot(flow_effluent_m3_h)
)

#Thirty based ammonium zom on 2021-08-15 to 2021-09-03+ rain + flow
gridExtra::grid.arrange(data_thirty_min %>% 
                          filter_index("2021-08-15"~"2021-09-03") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_thirty_min %>% 
                          filter_index("2021-08-15"~"2021-09-03") %>% 
                          autoplot(rainfall_mm),
                        data_thirty_min %>% 
                          filter_index("2021-08-15"~"2021-09-03") %>% 
                          autoplot(flow_effluent_m3_h)
)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Plotting the ammonium concentration, rain fall, and flow 
#zoom 2021-08-25 to 2021-08-28
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Minute based ammonium zom on 2021-08-25 to 2021-08-28 + rain + flow
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_one_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(flow_effluent_m3_h)
)


#Five based ammonium zom on 2021-08-25 to 2021-08-28 + rain + flow
gridExtra::grid.arrange(data_five_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_five_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(rainfall_mm),
                        data_five_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(flow_effluent_m3_h)
)

#Fifteen based ammonium zom on 2021-08-25 to 2021-08-28 + rain + flow
gridExtra::grid.arrange(data_fifteen_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_fifteen_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(rainfall_mm),
                        data_fifteen_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(flow_effluent_m3_h)
)


#Thirty based ammonium zom on 2021-08-25 to 2021-08-28 + rain + flow
gridExtra::grid.arrange(data_thirty_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(ammonium_effluent_mg_L) +
                          geom_hline(yintercept = 2, 
                                     col="Red"),
                        data_thirty_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(rainfall_mm),
                        data_thirty_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(flow_effluent_m3_h)
)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Plotting the ammonium and nitrate concentration, rain fall, and flow 
#zoom 2021-08-25 to 2021-08-28
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Minute based ammonium zom on 2021-08-25 to 2021-08-28 + rain + flow
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          filter(nitrate_effluent_mg_L<250) %>% 
                          ggplot(aes(time_one_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_one_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(flow_effluent_m3_h)
)


#Five based ammonium zom on 2021-08-25 to 2021-08-28 + rain + flow
gridExtra::grid.arrange(data_five_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          filter(nitrate_effluent_mg_L<250) %>% 
                          ggplot(aes(time_five_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_five_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(rainfall_mm),
                        data_five_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(flow_effluent_m3_h)
)

#Fifteen based ammonium zom on 2021-08-25 to 2021-08-28 + rain + flow
gridExtra::grid.arrange(data_fifteen_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          filter(nitrate_effluent_mg_L<250) %>% 
                          ggplot(aes(time_fifteen_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_fifteen_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(rainfall_mm),
                        data_fifteen_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(flow_effluent_m3_h)
)

#Thirty based ammonium zom on 2021-08-25 to 2021-08-28 + rain + flow
gridExtra::grid.arrange(data_thirty_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          filter(nitrate_effluent_mg_L<250) %>% 
                          ggplot(aes(time_thirty_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_thirty_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(rainfall_mm),
                        data_thirty_min %>% 
                          filter_index("2021-08-25"~"2021-08-28") %>% 
                          autoplot(flow_effluent_m3_h)
)




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Plotting the ammonium concentration, rain fall, and flow 
#zoom 2019-06 to 2021-11
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Minute based ammonium zom on 2019-06 to 2021-11 + rain + flow
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2019-06"~"2019-11") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Blue"),
                        data_one_min %>% 
                          filter_index("2019-06"~"2019-11") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2019-06"~"2019-11") %>% 
                          autoplot(flow_effluent_m3_h)
)

#Thirty based ammonium zom on 2019-06 to 2021-11 + rain + flow
gridExtra::grid.arrange(data_thirty_min %>% 
                          filter_index("2019-06"~"2019-11") %>% 
                          filter(nitrate_effluent_mg_L<20) %>% 
                          ggplot(aes(time_thirty_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_thirty_min %>% 
                          filter_index("2019-06"~"2019-11") %>% 
                          autoplot(rainfall_mm),
                        data_thirty_min %>% 
                          filter_index("2019-06"~"2019-11") %>% 
                          autoplot(flow_effluent_m3_h)
                        
)





#Minute based ammonium zom on 2019-06  + rain + flow
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2019-06") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Blue"),
                        data_one_min %>% 
                          filter_index("2019-06") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2019-06") %>% 
                          autoplot(flow_effluent_m3_h)
)

#Thirty based ammonium zom on 2019-06  + rain + flow
gridExtra::grid.arrange(data_thirty_min %>% 
                          filter_index("2019-06") %>% 
                          filter(nitrate_effluent_mg_L<20) %>% 
                          ggplot(aes(time_thirty_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_thirty_min %>% 
                          filter_index("2019-06") %>% 
                          autoplot(rainfall_mm),
                        data_thirty_min %>% 
                          filter_index("2019-06") %>% 
                          autoplot(flow_effluent_m3_h)
                        
)






#Minute based ammonium zom on 2019-07  + rain + flow
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2019-07") %>% 
                          autoplot(ammonium_effluent_mg_L)+
                          geom_hline(yintercept = 2, 
                                     col="Blue"),
                        data_one_min %>% 
                          filter_index("2019-07") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2019-07") %>% 
                          autoplot(flow_effluent_m3_h)
)

#Thirty based ammonium zom on 2019-07  + rain + flow
gridExtra::grid.arrange(data_thirty_min %>% 
                          filter_index("2019-07") %>% 
                          filter(nitrate_effluent_mg_L<20) %>% 
                          ggplot(aes(time_thirty_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_thirty_min %>% 
                          filter_index("2019-07") %>% 
                          autoplot(rainfall_mm),
                        data_thirty_min %>% 
                          filter_index("2019-07") %>% 
                          autoplot(flow_effluent_m3_h)
                        
)













#Thirty based ammonium zom on 2021-08-27  + rain + flow
gridExtra::grid.arrange(data_thirty_min %>% 
                          filter_index("2021-08-27") %>% 
                          filter(nitrate_effluent_mg_L<250) %>% 
                          ggplot(aes(time_thirty_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_thirty_min %>% 
                          filter_index("2021-08-27") %>% 
                          autoplot(rainfall_mm),
                        data_thirty_min %>% 
                          filter_index("2021-08-27") %>% 
                          autoplot(flow_effluent_m3_h)
)



#Thirty based ammonium zom on 2021-08-16  + rain + flow
gridExtra::grid.arrange(data_thirty_min %>% 
                          filter_index("2021-08-16") %>% 
                          filter(nitrate_effluent_mg_L<250) %>% 
                          ggplot(aes(time_thirty_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_thirty_min %>% 
                          filter_index("2021-08-16") %>% 
                          autoplot(rainfall_mm),
                        data_thirty_min %>% 
                          filter_index("2021-08-16") %>% 
                          autoplot(flow_effluent_m3_h)
)



#Thirty based ammonium zom on 2021-08-15 to 2021-08-16  + rain + flow
gridExtra::grid.arrange(data_thirty_min %>% 
                          filter_index("2021-08-15"~"2021-08-16") %>% 
                          filter(nitrate_effluent_mg_L<250) %>% 
                          ggplot(aes(time_thirty_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_thirty_min %>% 
                            filter_index("2021-08-15"~"2021-08-16") %>% 
                          autoplot(rainfall_mm),
                        data_thirty_min %>% 
                          filter_index("2021-08-15"~"2021-08-16") %>% 
                          autoplot(flow_effluent_m3_h),
                        data_thirty_min %>% 
                          filter_index("2021-08-15"~"2021-08-16") %>% 
                          autoplot(flow_influent_m3_h)
)


#Minute based ammonium zom on 2021-08-15 to 2021-08-16  + rain + flow
gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2021-08-15"~"2021-08-16") %>% 
                          filter(nitrate_effluent_mg_L<250) %>% 
                          ggplot(aes(time_one_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none"),
                        data_one_min %>% 
                          filter_index("2021-08-15"~"2021-08-16") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2021-08-15"~"2021-08-16") %>% 
                          autoplot(flow_effluent_m3_h)
)






data_thirty_min %>% ggplot(aes(flow_effluent_m3_h))+
  geom_point(aes(y=ammonium_effluent_mg_L))

data_thirty_min %>% 
  ggplot(aes(flow_effluent_m3_h))+
  geom_point(aes(y=nitrate_effluent_mg_L))

data_thirty_min %>% 
  filter(nitrate_effluent_mg_L<200)%>% 
  ggplot(aes(flow_effluent_m3_h))+
  geom_point(aes(y=nitrate_effluent_mg_L))






#Finding HRT
HRT <- data_one_min %>% 
  as_tibble() %>% 
  mutate(day=floor_date(time_one_min, "day")) %>% 
  select(-time_one_min) %>% 
  group_by(day) %>% 
  summarise(sum_rain=sum(rainfall_mm), 
            mean_flow=mean(flow_AN_m3_h)) %>% 
  filter(sum_rain<2) %>% 
  filter(!is.na(mean_flow))

HRT %>% select(mean_flow) %>% mean()



# 
# 
# #Plotting ammonium to AN and ammonium AN 
# 
# 
# gridExtra::grid.arrange(
#   data_hour %>% autoplot(ammonium_to_AN_mg_L),
#   data_hour %>% autoplot(ammonium_AN_mg_L)
# )
# 
# gridExtra::grid.arrange(
#   data_hour %>% 
#     filter(ammonium_to_AN_mg_L<200) %>% 
#     autoplot(ammonium_to_AN_mg_L),
#   data_hour %>% 
#     filter(ammonium_AN_mg_L<200) %>% 
#     autoplot(ammonium_AN_mg_L)
# )
# 
# 
# #Plotting the flow
# gridExtra::grid.arrange(
#   data_hour %>% autoplot(flow_influent_m3_h),
#   data_hour %>% autoplot(flow_AN_m3_h),
#   data_hour %>% autoplot(flow_effluent_m3_h)
# )
# 


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Plots to find time in sewers and to ammonium and nitrate peak
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Overview of the rain fall

data_hour %>% autoplot(rainfall_mm)


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


data_hour %>% 
  filter_index("2018-07"~"2018-09") %>% 
  autoplot(rainfall_mm)


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
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none")
)


gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2018-07-28 20:00"~"2018-07-28 21:00") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2018-07-28 20:00"~"2018-07-28 22:00") %>%
                          autoplot(flow_effluent_m3_h),
                        data_fifteen_min %>% 
                          filter_index("2018-07-28 20:00"~"2018-07-29 20:00") %>%
                          ggplot(aes(time_fifteen_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none")
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
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none")
)


gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2018-09-07 10:00"~"2018-09-07 14:00") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2018-09-07 10:00"~"2018-09-07 14:00") %>%
                          autoplot(flow_effluent_m3_h),
                        data_fifteen_min %>% 
                          filter_index("2018-09-07 10:00"~"2018-09-07 23:00") %>%
                          ggplot(aes(time_fifteen_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none")
)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Rain in 2019
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#13th of march 2019
data_hour %>% 
  filter_index("2019-03-12"~"2019-03-13") %>% 
  autoplot(rainfall_mm)

gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2019-03-13 10:00"~"2019-03-13 12:00") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2019-03-13 10:00"~"2019-03-13 12:00") %>%
                          autoplot(flow_effluent_m3_h),
                        data_one_min %>% 
                          filter_index("2019-03-13 11:00"~"2019-03-14 00:00") %>%
                          ggplot(aes(time_one_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none")
)

#2019-08-27
data_hour %>% 
  filter_index("2019-08-27"~"2019-08-28") %>% 
  autoplot(rainfall_mm)

gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2019-08-27 17:00"~"2019-08-27 19:00") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2019-08-27 17:00"~"2019-08-27 19:00") %>%
                          autoplot(flow_effluent_m3_h),
                        data_fifteen_min %>% 
                          filter_index("2019-08-27 13:00"~"2019-08-28 04:00") %>%
                          ggplot(aes(time_fifteen_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none")
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


gridExtra::grid.arrange(data_five_min %>% 
                          filter_index("2020-06-19 11:00"~"2020-06-19 14:00") %>% 
                          autoplot(rainfall_mm),
                        data_five_min %>% 
                          filter_index("2020-06-19 11:00"~"2020-06-19 14:00") %>%
                          autoplot(flow_effluent_m3_h),
                        data_fifteen_min %>% 
                          filter_index("2020-06-19 08:00"~"2020-06-19 23:00") %>%
                          ggplot(aes(time_fifteen_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none")
)



#18th of august 2020
data_hour %>% 
  filter_index("2020-08-18") %>% 
  autoplot(rainfall_mm)


gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2020-08-18 11:00"~"2020-08-18 14:00") %>% 
                          autoplot(rainfall_mm),
                        data_one_min %>% 
                          filter_index("2020-08-18 11:00"~"2020-08-18 14:00") %>%
                          autoplot(flow_effluent_m3_h),
                        data_fifteen_min %>% 
                          filter_index("2020-08-18 11:00"~"2020-08-18 23:00") %>%
                          ggplot(aes(time_fifteen_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none")
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


gridExtra::grid.arrange(data_five_min %>% 
                          filter_index("2021-06-20 10:00"~"2021-06-20 13:00") %>% 
                          autoplot(rainfall_mm),
                        data_five_min %>% 
                          filter_index("2021-06-20 10:00"~"2021-06-20 13:00") %>%
                          autoplot(flow_effluent_m3_h),
                        data_fifteen_min %>% 
                          filter_index("2021-06-20 10:00"~"2021-06-21 12:00") %>%
                          ggplot(aes(time_fifteen_min)) + 
                          geom_line(aes(y =ammonium_effluent_mg_L)) +
                          geom_line(aes(y = nitrate_effluent_mg_L, 
                                        col="Red", 
                                        alpha=0.6))+
                          geom_hline(yintercept = 2, 
                                     col="Blue")+
                          theme(legend.position="none")
)






