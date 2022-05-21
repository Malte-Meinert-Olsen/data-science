#setup

source("report_code_setup.R")


#Need load training data and all the model to run the code agian
print("STOP")




plot_model_fit <- function(df){
  
  fitted_data <- augment(df) %>% 
    filter_index("2020")
  
  training_data %>% 
    filter_index("2020") %>% 
    autoplot(flow_effluent_m3_h) +
    geom_line(aes(y = .fitted), col="Red", alpha=0.5,
              data = fitted_data)+
    labs(x="Days [d]",
         y='Flow of effluent ['~m^3~'/h]')
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
TSLM_R %>% 
  plot_model_fit()

#----------------------------------
#TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI
#----------------------------------
TSLM_R_R1_R2_R3_R4_R5_R6_R7_DI %>% 
  plot_model_fit()

#----------------------------------
#TSLM_R_R1_R4_R7_DI
#----------------------------------
TSLM_R_R1_R4_R7_DI %>% 
plot_model_fit()

#----------------------------------
#TSLM_R_R1_R7_DI
#----------------------------------
TSLM_R_R1_R7_DI  %>% 
plot_model_fit()

#----------------------------------
#TSLM_R_R1_R7_DIxR1
#----------------------------------
TSLM_R_R1_R7_DIxR1 %>%
plot_model_fit()

#----------------------------------
#TSLM_R_R1_R7_DIxR1_DI
#----------------------------------
TSLM_R_R1_R7_DIxR1_DI %>%
plot_model_fit()

#----------------------------------
#TSLM_R_lag1R_R1_R7_DIxR1_DI
#----------------------------------
TSLM_R_lag1R_R1_R7_DIxR1_DI %>%
plot_model_fit()

#----------------------------------
#TSLM_R_lag1R_lag2R_R1_R7_DIxR1_DI
#----------------------------------
TSLM_R_lag1R_lag2R_R1_R7_DIxR1_DI %>%
plot_model_fit()

#----------------------------------
#TSLM_R_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI
#----------------------------------
TSLM_R_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI %>%
plot_model_fit()

#----------------------------------
#TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI
#----------------------------------
TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI %>%
plot_model_fit()

#----------------------------------
#TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK10_FWK5_FYK3
#----------------------------------
TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK10_FWK5_FYK3 %>%
plot_model_fit()

#----------------------------------
#TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>%
plot_model_fit()

#----------------------------------
#TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK5_FWK3_FYK1
#----------------------------------
TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK5_FWK3_FYK1 %>%
plot_model_fit()

#----------------------------------
#TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7
#----------------------------------
TSLM_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7 %>%
plot_model_fit()

#----------------------------------
#TSLM_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
TSLM_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>% 
plot_model_fit()

#-------------------------------------------------------------------------------
#Linear model with ARIMA errors using the ARIMA function
#-------------------------------------------------------------------------------

#----------------------------------
#ARIMA(d=0)
#----------------------------------
ARIMA_d_0 %>%
plot_model_fit()

#----------------------------------
#ARIMA(d=1)
#----------------------------------
ARIMA_d_1 %>%
plot_model_fit()

#----------------------------------
#ARIMA(d=2)
#----------------------------------
ARIMA_d_2 %>%
plot_model_fit()

#----------------------------------
#ARIMA_d_0_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
ARIMA_d_0_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>%
plot_model_fit()

#----------------------------------
#ARIMA_d_1_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
ARIMA_d_1_lag1R_lag2R_lag3R_R1_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>%
plot_model_fit()

#----------------------------------
#ARIMA_d_0_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
ARIMA_d_0_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>%
plot_model_fit()

#----------------------------------
#ARIMA_d_1_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3
#----------------------------------
ARIMA_d_1_R_lag1R_lag2R_lag3R_R1_R2_R3_R4_R5_R6_R7_DIxR1_DI_FDK7_FWK5_FYK3 %>%
plot_model_fit()




