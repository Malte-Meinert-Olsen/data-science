test <- read.csv("data/data_hour.csv")


test <- data_hour %>% 
  select(time_hour,rainfall_mm, flow_effluent_m3_h)


test <- test %>% 
  filter_index("2020-02")
test <- test %>% 
  mutate(test_rain=rollapplyr(rainfall_mm, width=1*24, FUN=sum, partial=T))


test %>% autoplot(rainfall_mm)+
  geom_line(aes(y=test_rain,col="RED"))



#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)


test<- test %>% 
  select(-day) %>% 
  mutate(time_hour=ymd_hms(time_hour)) %>% 
  as_tsibble(index=time_hour)



test %>% 
  autoplot(flow_effluent_m3_h) +
  geom_line(aes(y=rainfall_mm),col="Red", alpha=0.5)




test_fcm <- function(df,av=0.5){
  df %>% 
    autoplot(flow_effluent_m3_h)+
    geom_line(aes(rainfall_mm), col="Red", alpha=av)
}

test %>% test_fcm()




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

#Test on saving function


test <- data_hour %>% 
  filter_index("2020")

test %>% autoplot(flow_effluent_m3_h)


test_fit <- test %>% 
  model(TSLM(flow_effluent_m3_h~ rainfall_mm+drought))

test %>% autoplot(flow_effluent_m3_h) +
  geom_line(aes(y = .fitted), col="Red",
            data = augment(test_fit))


saveRDS(test_fit, "test_fit.rds")
my_model <- readRDS("test_fit.rds")

test %>% autoplot(flow_effluent_m3_h) +
  geom_line(aes(y = .fitted), col="Red",
            data = augment(my_model))


if(as.Date(data_hour$time_hour)=="2020-01-01"){
  test <- test %>% 
    mutate(drought=10000)
}

test <- test %>% 
  mutate(day=as.Date(time_hour))

test$drought[test$day=="2020-01-01"] <- 1000
