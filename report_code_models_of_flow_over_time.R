#Setup

source("report_code_setup.R")


#Splitting data
training_data <- data_hour %>% 
  filter_index(~"2021") %>% 
  fill_gaps()


test_data <- data_hour %>% 
  filter_index("2022")

#Reporting function
fit_report <- function(df){
  df %>% report()

  df %>% 
    glance() %>% 
    select(AIC,BIC)
  
}



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Making models of flow over time
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#Linear model with the TSLM function
#-------------------------------------------------------------------------------


#----------------------------------
#TSLM(R)
#----------------------------------
TSLM_R <- training_data %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm)
  )

TSLM_R %>% 
  fit_report() 

TSLM_R %>% 
  saveRDS("models/TSLM_R.rds")



#----------------------------------
#TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI
#----------------------------------
TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI <- training_data %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           drought)
  )

TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI %>% 
  fit_report() 

TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI %>% 
  saveRDS("models/TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI.rds")



#----------------------------------
#TSLM_R_R1_R4_R7_DI
#----------------------------------
TSLM_R_R1_R4_R7_DI <- training_data %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_four_day_accumulated+
           rain_seven_day_accumulated+
           drought)
  )

TSLM_R_R1_R4_R7_DI %>% 
  fit_report() 

TSLM_R_R1_R4_R7_DI %>% 
  saveRDS("models/TSLM_R_R1_R4_R7_DI.rds")


#----------------------------------
#TSLM_R_R1_R7_DI
#----------------------------------
TSLM_R_R1_R7_DI <- training_data %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           drought)
  )

TSLM_R_R1_R7_DI %>% 
  fit_report() 

TSLM_R_R1_R7_DI %>% 
  saveRDS("models/TSLM_R_R1_R7_DI.rds")


#----------------------------------
#TSLM_R_R1_R7_DIxR1
#----------------------------------
TSLM_R_R1_R7_DIxR1 <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1)
  )

TSLM_R_R1_R7_DIxR1 %>% 
  fit_report() 

TSLM_R_R1_R7_DIxR1 %>% 
  saveRDS("models/TSLM_R_R1_R7_DIxR1.rds")

#----------------------------------
#TSLM_R_R1_R7_DIxR1_DI
#----------------------------------
TSLM_R_R1_R7_DIxR1_DI <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1+
           drought)
  )

TSLM_R_R1_R7_DIxR1_DI %>% 
  fit_report() 

TSLM_R_R1_R7_DIxR1_DI %>% 
  saveRDS("models/TSLM_R_R1_R7_DIxR1_DI.rds")


#----------------------------------
#TSLM_R_lag1R_R1_R7_DIxR1_DI
#----------------------------------
TSLM_R_lag1R_R1_R7_DIxR1_DI <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           lag(rainfall_mm,n=1L)+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1+
           drought)
  )

TSLM_R_lag1R_R1_R7_DIxR1_DI %>% 
  fit_report() 

TSLM_R_lag1R_R1_R7_DIxR1_DI %>% 
  saveRDS("models/TSLM_R_lag1R_R1_R7_DIxR1_DI.rds")



#----------------------------------
#TSLM_R_lag1R_lag2R_R1_R7_DIxR1_DI
#----------------------------------
TSLM_R_lag1R_lag2R_R1_R7_DIxR1_DI <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           lag(rainfall_mm,n=1L)+
           lag(rainfall_mm,n=2L)+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1+
           drought)
  )

TSLM_R_lag1R_lag2R_R1_R7_DIxR1_DI %>% 
  fit_report() 

TSLM_R_lag1R_lag2R_R1_R7_DIxR1_DI %>% 
  saveRDS("models/TSLM_R_lag1R_lag2R_R1_R7_DIxR1_DI.rds")

#----------------------------------
#TSLM_R_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI
#----------------------------------
TSLM_R_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           lag(rainfall_mm,n=1L)+
           lag(rainfall_mm,n=2L)+
           lag(rainfall_mm,n=3L)+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1+
           drought)
  )

TSLM_R_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI %>% 
  fit_report() 

TSLM_R_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI %>% 
  saveRDS("models/TSLM_R_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI.rds")

#----------------------------------
#TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI
#----------------------------------
TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~ lag(rainfall_mm,n=1L)+
           lag(rainfall_mm,n=2L)+
           lag(rainfall_mm,n=3L)+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1+
           drought)
  )

TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI %>% 
  fit_report() 

TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI %>% 
  saveRDS("models/TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI.rds")



#----------------------------------
#TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK10_FWK5_FYK3
#----------------------------------
TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK10_FWK5_FYK3 <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~ lag(rainfall_mm,n=1L)+
           lag(rainfall_mm,n=2L)+
           lag(rainfall_mm,n=3L)+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1+
           drought+
           fourier(period = "day", K = 10) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK10_FWK5_FYK3 %>% 
  fit_report() 

TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK10_FWK5_FYK3 %>% 
  saveRDS("models/TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK10_FWK5_FYK3.rds")


#----------------------------------
#TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~ lag(rainfall_mm,n=1L)+
           lag(rainfall_mm,n=2L)+
           lag(rainfall_mm,n=3L)+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1+
           drought+
           fourier(period = "day", K = 7) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  fit_report() 

TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  saveRDS("models/TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")


#----------------------------------
#TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK5_FWK3_FYK1
#----------------------------------
TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK5_FWK3_FYK1 <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~ lag(rainfall_mm,n=1L)+
           lag(rainfall_mm,n=2L)+
           lag(rainfall_mm,n=3L)+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1+
           drought+
           fourier(period = "day", K = 5) +
           fourier(period = "week", K = 3) +
           fourier(period = "year", K = 1))
  )

TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK5_FWK3_FYK1 %>% 
  fit_report() 

TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK5_FWK3_FYK1 %>% 
  saveRDS("models/TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK5_FWK3_FYK1.rds")



#----------------------------------
#TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7
#----------------------------------
TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7 <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~ lag(rainfall_mm,n=1L)+
           lag(rainfall_mm,n=2L)+
           lag(rainfall_mm,n=3L)+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1+
           drought+
           fourier(period = "day", K = 7)) 
    )

TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7 %>% 
  fit_report() 

TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7 %>% 
  saveRDS("models/TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7.rds")




#----------------------------------
#TSLM_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
TSLM_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~ rainfall_mm+
            lag(rainfall_mm,n=1L)+
            lag(rainfall_mm,n=2L)+
            lag(rainfall_mm,n=3L)+
            rain_one_day_accumulated+
            rain_two_day_accumulated+
            rain_three_day_accumulated+
            rain_four_day_accumulated+
            rain_five_day_accumulated+
            rain_six_day_accumulated+
            rain_seven_day_accumulated+
            DIxR1+
            drought+
            fourier(period = "day", K = 7) +
            fourier(period = "week", K = 5) +
            fourier(period = "year", K = 3))
  )

TSLM_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  fit_report() 

TSLM_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  saveRDS("models/TSLM_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")


#-------------------------------------------------------------------------------
#Linear model with ARIMA errors using the ARIMA function
#-------------------------------------------------------------------------------


#----------------------------------
#ARIMA(d=0)
#----------------------------------
ARIMA_d_0 <- training_data %>% 
  fill_gaps() %>% 
  model(
    ARIMA(flow_effluent_m3_h ~ PDQ(0, 0, 0) + pdq(d = 0))
  )



ARIMA_d_0 %>% 
  fit_report() 

ARIMA_d_0 %>% 
  saveRDS("models/ARIMA_d_0.rds")


#----------------------------------
#ARIMA(d=1)
#----------------------------------
ARIMA_d_1 <- training_data %>% 
  fill_gaps() %>% 
  model(
    ARIMA(flow_effluent_m3_h ~ PDQ(0, 0, 0) + pdq(d = 1))
  )



ARIMA_d_1 %>% 
  fit_report() 

ARIMA_d_1 %>% 
  saveRDS("models/ARIMA_d_1.rds")


#----------------------------------
#ARIMA(d=2)
#----------------------------------
ARIMA_d_2 <- training_data %>% 
  fill_gaps() %>% 
  model(
    ARIMA(flow_effluent_m3_h ~ PDQ(0, 0, 0) + pdq(d = 2))
  )



ARIMA_d_2 %>% 
  fit_report() 

ARIMA_d_2 %>% 
  saveRDS("models/ARIMA_d_2.rds")


#----------------------------------
#ARIMA_d_0_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
ARIMA_d_0_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  fill_gaps() %>% 
  model(
    ARIMA(flow_effluent_m3_h~ pdq(d=0)+PDQ(0, 0, 0)+lag(rainfall_mm,n=1L)+
           lag(rainfall_mm,n=2L)+
           lag(rainfall_mm,n=3L)+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIxR1+
           drought+
           fourier(period = "day", K = 7) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

ARIMA_d_0_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  fit_report() 

ARIMA_d_0_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  saveRDS("models/ARIMA_d_0_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")




#----------------------------------
#ARIMA_d_1_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
ARIMA_d_1_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  fill_gaps() %>% 
  model(
    ARIMA(flow_effluent_m3_h~ pdq(d=1)+PDQ(0, 0, 0)+lag(rainfall_mm,n=1L)+
            lag(rainfall_mm,n=2L)+
            lag(rainfall_mm,n=3L)+
            rain_one_day_accumulated+
            rain_seven_day_accumulated+
            DIxR1+
            drought+
            fourier(period = "day", K = 7) +
            fourier(period = "week", K = 5) +
            fourier(period = "year", K = 3))
  )

ARIMA_d_1_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  fit_report() 

ARIMA_d_1_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  saveRDS("models/ARIMA_d_1_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")


#----------------------------------
#ARIMA_d_0_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
ARIMA_d_0_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  fill_gaps() %>% 
  model(
    ARIMA(flow_effluent_m3_h~ pdq(d=0)+PDQ(0, 0, 0)+rainfall_mm+
            lag(rainfall_mm,n=1L)+
            lag(rainfall_mm,n=2L)+
            lag(rainfall_mm,n=3L)+
            rain_one_day_accumulated+
            rain_two_day_accumulated+
            rain_three_day_accumulated+
            rain_four_day_accumulated+
            rain_five_day_accumulated+
            rain_six_day_accumulated+
            rain_seven_day_accumulated+
            DIxR1+
            drought+
            fourier(period = "day", K = 7) +
            fourier(period = "week", K = 5) +
            fourier(period = "year", K = 3))
  )

ARIMA_d_0_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  fit_report() 

ARIMA_d_0_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  saveRDS("models/ARIMA_d_0_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")

#----------------------------------
#ARIMA_d_1_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
ARIMA_d_1_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 <- training_data %>% 
  mutate(DIxR1=rain_one_day_accumulated*drought) %>% 
  fill_gaps() %>% 
  model(
    ARIMA(flow_effluent_m3_h~ pdq(d=1)+PDQ(0, 0, 0)+rainfall_mm+
            lag(rainfall_mm,n=1L)+
            lag(rainfall_mm,n=2L)+
            lag(rainfall_mm,n=3L)+
            rain_one_day_accumulated+
            rain_two_day_accumulated+
            rain_three_day_accumulated+
            rain_four_day_accumulated+
            rain_five_day_accumulated+
            rain_six_day_accumulated+
            rain_seven_day_accumulated+
            DIxR1+
            drought+
            fourier(period = "day", K = 7) +
            fourier(period = "week", K = 5) +
            fourier(period = "year", K = 3))
  )

ARIMA_d_1_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  fit_report() 

ARIMA_d_1_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
  saveRDS("models/ARIMA_d_1_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3.rds")







