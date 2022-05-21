#Setup

source("report_code_setup.R")


#finding the mean flow of dry weather operation, meaning days where the rain was under 2 mm

HRT1 <- data_one_min %>% 
  as_tibble() %>% 
  mutate(day=as.Date(time_one_min)) %>% 
  select(-time_one_min) %>% 
  group_by(day) %>% 
  summarise(sum_rain=sum(rainfall_mm),
            mean_flow=mean(flow_AN_m3_h)) %>% 
  filter(sum_rain<2) %>% 
  filter(!is.na(mean_flow))



mean(HRT1$mean_flow)

