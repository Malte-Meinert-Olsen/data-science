#STL decomposition

#Setup 

source("report_code_setup.R")



#Making the decomposition

#Removing the time scale and replacing with observation
flow <- data_hour %>%
  mutate(t = row_number()) %>%
  update_tsibble(index = t, regular = TRUE)

#Plotting the STL decomposition with all season being periodic
flow %>%
  model(
    STL(flow_effluent_m3_h ~ season(period = 24, 
                                    window="periodic") +
          season(period = 7*24, 
                 window="periodic")+
          season(period = 365.25*24, 
                 window="periodic"),
        robust = TRUE)) %>%
  components() %>%
  filter_index(0~1000) %>% 
  autoplot() +
  labs(x = "Hours after 2018-01-01 00:00:00")

#Plotting STL decomposition with windows of 93, 51 and no yearly seasonality
flow %>%
  model(
    STL(flow_effluent_m3_h ~ season(period = 24, 
                                    window=93) +
          season(period = 7*24, 
                 window=51),
        robust = TRUE)) %>%
  components() %>%
  autoplot() +
  labs(x = "Hours after 2018-01-01 00:00:00")

#Plotting STL decomposition with windows of 93, and no weekly and yearly seasonality
flow %>%
  model(
    STL(flow_effluent_m3_h ~ season(period = 24, 
                                    window=93),
        robust = TRUE)) %>%
  components() %>%
  autoplot() +
  labs(x = "Hours after 2018-01-01 00:00:00")



