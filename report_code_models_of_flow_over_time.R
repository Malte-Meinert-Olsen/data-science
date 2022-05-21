#Setup

source("report_code_setup.R")


#Splitting data
training_data <- data_hour %>% 
  filter_index(~"2021")


test_data <- data_hour %>% 
  filter_index("2022")

#Reporting function
fit_report <- function(df){
  df %>% report()

  df %>% 
    glance() %>%
    select(adj_r_squared, AIC, BIC)
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
TSLM_R <- data_hour %>% 
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
TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI <- data_hour %>% 
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
#TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI_FDK10_FWK5_FYK3
#----------------------------------
TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI_FDK10_FWK5_FYK3 <- data_hour %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           drought+
           fourier(period = "day", K = 10) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI_FDK10_FWK5_FYK3 %>% 
  fit_report() 

TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI_FDK10_FWK5_FYK3 %>% 
  saveRDS("models/TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI_FDK10_FWK5_FYK3.rds")


#----------------------------------
#TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI_FDK5_FWK5_FYK3
#----------------------------------
TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI_FDK5_FWK5_FYK3 <- data_hour %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_two_day_accumulated+
           rain_three_day_accumulated+
           rain_four_day_accumulated+
           rain_five_day_accumulated+
           rain_six_day_accumulated+
           rain_seven_day_accumulated+
           drought+
           fourier(period = "day", K = 5) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI_FDK5_FWK5_FYK3 %>% 
  fit_report() 

TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI_FDK5_FWK5_FYK3 %>% 
  saveRDS("models/TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI_FDK5_FWK5_FYK3.rds")




#----------------------------------
#TSLM_R_R1_R4_R7_DI_FDK5_FWK5_FYK3
#----------------------------------
TSLM_R_R1_R4_R7_DI_FDK5_FWK5_FYK3 <- data_hour %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_four_day_accumulated+
           rain_seven_day_accumulated+
           drought+
           fourier(period = "day", K = 5) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

TSLM_R_R1_R4_R7_DI_FDK5_FWK5_FYK3 %>% 
  fit_report() 

TSLM_R_R1_R4_R7_DI_FDK5_FWK5_FYK3 %>% 
  saveRDS("models/TSLM_R_R1_R4_R7_DI_FDK5_FWK5_FYK3.rds")


#----------------------------------
#TSLM_R_R1_R7_DIxR1_FDK5_FWK5_FYK3
#----------------------------------
TSLM_R_R1_R7_DIxR1_FDK5_FWK5_FYK3 <- data_hour %>% 
  mutate(DIXR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           DIXR1+
           fourier(period = "day", K = 5) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

TSLM_R_R1_R7_DIxR1_FDK5_FWK5_FYK3 %>% 
  fit_report() 

TSLM_R_R1_R7_DIxR1_FDK5_FWK5_FYK3 %>% 
  saveRDS("models/TSLM_R_R1_R7_DIxR1_FDK5_FWK5_FYK3.rds")


#----------------------------------
#TSLM_R_R1_R7_FDK5_FWK5_FYK3
#----------------------------------
TSLM_R_R1_R7_FDK5_FWK5_FYK3 <- data_hour %>% 
  mutate(DIXR1=rain_one_day_accumulated*drought) %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           fourier(period = "day", K = 5) +
           fourier(period = "week", K = 5) +
           fourier(period = "year", K = 3))
  )

TSLM_R_R1_R7_FDK5_FWK5_FYK3 %>% 
  fit_report() 

TSLM_R_R1_R7_FDK5_FWK5_FYK3 %>% 
  saveRDS("models/TSLM_R_R1_R7_FDK5_FWK5_FYK3.rds")




#----------------------------------
#TSLM_R_R1_R7_FDK5_FWK3_FYK1
#----------------------------------
TSLM_R_R1_R7_FDK5_FWK3_FYK1 <- data_hour %>% 
  model(
    TSLM(flow_effluent_m3_h~rainfall_mm+
           rain_one_day_accumulated+
           rain_seven_day_accumulated+
           fourier(period = "day", K = 5) +
           fourier(period = "week", K = 3) +
           fourier(period = "year", K = 1))
  )

TSLM_R_R1_R7_FDK5_FWK3_FYK1 %>% 
  fit_report() 

TSLM_R_R1_R7_FDK5_FWK3_FYK1 %>% 
  saveRDS("models/TSLM_R_R1_R7_FDK5_FWK3_FYK1.rds")






