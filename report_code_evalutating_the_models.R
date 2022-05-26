#setup

source("report_code_setup.R")

#Load the models
full_TSLM <- readRDS("models/TSLM_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")
full_ARIMA <- readRDS("models/ARIMA_d_0_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")

#specify the rain in the future
future_rain <- data_hour %>% 
  filter_index("2022-01-01"~.) %>%
  select(-flow_effluent_m3_h) %>% 
  mutate(DIxR1=drought*rain_one_day_accumulated) %>% 
  fill_gaps()

#Make a forecast
fc_full_TSLM <- forecast(full_TSLM, 
                         new_data = future_rain)

fc_full_ARIMA <- forecast(full_ARIMA, 
                          new_data = future_rain)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Accuracy
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
accuracy(fc_full_TSLM, data_hour)

accuracy(fc_full_ARIMA, data_hour)


#Residuals

full_TSLM %>% gg_tsresiduals()
full_ARIMA %>% gg_tsresiduals()

#-------------------
#Plot for full TSLM
#-------------------

fc_full_TSLM %>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022")
  )+ theme(legend.position="none")

plot_full_TSLM_J <- fc_full_TSLM %>% 
  filter_index("2022-01")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-01")
  )+ theme(legend.position="none")+
  labs(x="Days",
       y='Flow of effluent ['~m^3~'/h]')

plot_full_TSLM_F <-fc_full_TSLM %>% 
  filter_index("2022-02")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-02")
  )+ theme(legend.position="none")+
  labs(x="Days",
       y='Flow of effluent ['~m^3~'/h]')
           
plot_full_TSLM_M <-fc_full_TSLM %>% 
  filter_index("2022-03")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-03")
  )+ theme(legend.position="none")+
  labs(x="Days",
       y='Flow of effluent ['~m^3~'/h]')


gridExtra::grid.arrange(plot_full_TSLM_J,
                        plot_full_TSLM_F,
                        plot_full_TSLM_M
                        )

full_TSLM %>% gg_tsresiduals()


#-------------------
#Plot for full ARIMA
#-------------------

fc_full_ARIMA %>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022")
  )+ theme(legend.position="none")+
  labs(x="Days",
       y='Flow of effluent ['~m^3~'/h]')


plot_full_ARIMA_J <- fc_full_ARIMA %>% 
  filter_index("2022-01")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-01")
  )+ theme(legend.position="none")+
  labs(x="Days",
       y='Flow of effluent ['~m^3~'/h]')

plot_full_ARIMA_F <-fc_full_ARIMA %>% 
  filter_index("2022-02")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-02")
  )+ theme(legend.position="none")+
  labs(x="Days",
       y='Flow of effluent ['~m^3~'/h]')

plot_full_ARIMA_M <- fc_full_ARIMA %>% 
  filter_index("2022-03")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-03")
  )+ theme(legend.position="none")+
  labs(x="Days",
       y='Flow of effluent ['~m^3~'/h]')


gridExtra::grid.arrange(plot_full_ARIMA_J,
                        plot_full_ARIMA_F,
                        plot_full_ARIMA_M
)

full_ARIMA %>% gg_tsresiduals()
