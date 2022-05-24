#setup

source("report_code_setup.R")

full_TSLM <- readRDS("models/TSLM_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")
full_ARIMA <- readRDS("models/ARIMA_d_0_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")
reduced_TSLM <- readRDS("models/TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")
reduced_ARIMA <- readRDS("models/ARIMA_d_0_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#plots
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Plot all model in one model
full_TSLM_data <- augment(full_TSLM) %>% 
  filter_index("2020-03")

full_ARIMA_data <- augment(full_ARIMA) %>% 
  filter_index("2020-03")

reduced_TSLM_data <- augment(reduced_TSLM) %>% 
  filter_index("2020-03")

reduced_ARIMA_data <- augment(reduced_ARIMA) %>% 
  filter_index("2020-03")

data_hour %>% 
  filter_index("2020-03") %>% 
  autoplot(flow_effluent_m3_h) +
  geom_line(aes(y = .fitted), col="Red", alpha=0.5, size = 1,
            data = full_TSLM_data)+
  geom_line(aes(y = .fitted), col="Blue", alpha=0.5, size = 1,
            data = full_ARIMA_data)+
  geom_line(aes(y = .fitted), col="Green", alpha=0.5, size = 1,
            data = reduced_TSLM_data)+
  geom_line(aes(y = .fitted), col="Orange", alpha=0.5, size = 1,
            data = reduced_ARIMA_data)+
  labs(x="Days [d]",
       y='Flow of effluent ['~m^3~'/h]')


#Plot all model in one model

data_hour %>% 
  filter_index("2021-12-25"~"2022-01-01") %>% 
  autoplot(flow_effluent_m3_h) 

future_rain <- data_hour %>% 
  filter_index("2022-01-01"~.) %>%
  select(-flow_effluent_m3_h) %>% 
  mutate(DIxR1=drought*rain_one_day_accumulated) %>% 
  fill_gaps()


fc_full_TSLM <- forecast(full_TSLM, 
                         new_data = future_rain)

fc_full_ARIMA <- forecast(full_ARIMA, 
                         new_data = future_rain)

fc_reduced_TSLM <- forecast(reduced_TSLM, 
                         new_data = future_rain)

fc_reduced_ARIMA <- forecast(reduced_ARIMA, 
                         new_data = future_rain)





fc_full_TSLM <- forecast(full_TSLM, 
                         new_data = future_rain) %>% 
  filter_index("2022-01-01")

fc_full_ARIMA <- forecast(full_ARIMA, 
                          new_data = future_rain) %>% 
  filter_index("2022-01-01")

fc_reduced_TSLM <- forecast(reduced_TSLM, 
                            new_data = future_rain) %>% 
  filter_index("2022-01-01")

fc_reduced_ARIMA <- forecast(reduced_ARIMA, 
                             new_data = future_rain) %>% 
  filter_index("2022-01-01")


train <- data_hour %>% 
  filter_index("2021-12-25"~"2021-12-31")

measured <- data_hour %>% 
  select(flow_effluent_m3_h) %>% 
  filter_index("2022-01-01")


fc_full_TSLM %>%
  autoplot(train, level = NULL) +
  autolayer(measured,
    colour = "black"
  )

fc_full_ARIMA %>%
  autoplot(train, level = NULL) +
  autolayer(measured,
            colour = "black"
  )

fc_reduced_TSLM %>%
  autoplot(train, level = NULL) +
  autolayer(measured,
            colour = "black"
  )

fc_reduced_ARIMA %>%
  autoplot(train, level = NULL) +
  autolayer(measured,
            colour = "black"
  )

all_fc <- bind_rows(fc_reduced_ARIMA,fc_reduced_TSLM,fc_full_ARIMA,fc_full_TSLM)

all_fc %>%
  autoplot(train, level = NULL) +
  autolayer(measured,
            colour = "black"
  )



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Accuracy
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

full_TSLM <- readRDS("models/TSLM_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")
full_ARIMA <- readRDS("models/ARIMA_d_0_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")
reduced_TSLM <- readRDS("models/TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")
reduced_ARIMA <- readRDS("models/ARIMA_d_0_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")



fc_full_TSLM <- forecast(full_TSLM, 
                         new_data = future_rain)

fc_full_ARIMA <- forecast(full_ARIMA, 
                          new_data = future_rain)

fc_reduced_TSLM <- forecast(reduced_TSLM, 
                            new_data = future_rain)

fc_reduced_ARIMA <- forecast(reduced_ARIMA, 
                             new_data = future_rain)


accuracy(fc_full_TSLM, data_hour)

accuracy(fc_full_ARIMA, data_hour)

acaccuracy(fc_reduced_TSLM, data_hour)

accuracy(fc_reduced_ARIMA, data_hour)

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

#-------------------
#Plot for reduced TSLM
#-------------------

fc_reduced_TSLM %>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022")
  )+ theme(legend.position="none")


plot_reduced_TSLM_J <- fc_reduced_TSLM %>% 
  filter_index("2022-01")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-01")
  )+ theme(legend.position="none")

plot_reduced_TSLM_F <-fc_reduced_TSLM %>% 
  filter_index("2022-02")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-02")
  )+ theme(legend.position="none")

plot_reduced_TSLM_M <- fc_reduced_TSLM %>% 
  filter_index("2022-03")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-03")
  )+ theme(legend.position="none")


gridExtra::grid.arrange(plot_reduced_TSLM_J,
                        plot_reduced_TSLM_F,
                        plot_reduced_TSLM_M
)
reduced_TSLM %>% gg_tsresiduals()


#-------------------
#Plot for reduced ARIMA
#-------------------

fc_reduced_ARIMA %>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022")
  )+ theme(legend.position="none")


plot_reduced_ARIMA_J <- fc_reduced_ARIMA %>% 
  filter_index("2022-01")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-01")
  )+ theme(legend.position="none")

plot_reduced_ARIMA_F <-fc_reduced_ARIMA %>% 
  filter_index("2022-02")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-02")
  )+ theme(legend.position="none")

plot_reduced_ARIMA_M <- fc_reduced_ARIMA %>% 
  filter_index("2022-03")%>%
  autoplot(
    data_hour %>% 
      select(flow_effluent_m3_h)%>% 
      filter_index("2022-03")
  )+ theme(legend.position="none")


gridExtra::grid.arrange(plot_reduced_ARIMA_J,
                        plot_reduced_ARIMA_F,
                        plot_reduced_ARIMA_M
)

reduced_ARIMA %>% gg_tsresiduals()


 