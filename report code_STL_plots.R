#STL decomposition

#Setup 

source("report_code_setup.R")



#Making the decomposition


flow <- data_hour %>%
  mutate(t = row_number()) %>%
  update_tsibble(index = t, regular = TRUE)

flow %>%
  model(
    STL(flow_effluent_m3_h ~ season(period = 24) +
          season(period = 7*24)+
          season(period = 365.25*24),
        robust = TRUE)) %>%
  components() %>%
  rename("Orginal timeseries"=flow_effluent_m3_h,
         "Trend component"=trend,
         "Daily seasonality"=season_24,
         "Weekly seasonality"=season_168,
         "Yealy seasonality"=season_8766,
         "Remainder component"=remainder) %>% 
  ggplot(gather(dat, component, value, -Time), aes(Time, value)) +
  facet_grid(component ~ ., scales="free_y") +
  geom_line()+ labs(x = "Observation")



STL_comp <- flow %>%
  model(
    STL(flow_effluent_m3_h ~ season(period = 24) +
          season(period = 7*24)+
          season(period = 365.25*24),
        robust = TRUE)) %>%
  components() %>% 
  autoplot()+
  labs(x = "Hours after 2018-01-01 00:00:00")

