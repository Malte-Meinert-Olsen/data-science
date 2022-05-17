#Load the data and packages
library(tidyverse)
library(lubridate)
library(fpp3)
library(zoo)


data_one_min <- read_csv("data_one_min.csv")

data_one_min <- data_one_min %>% 
  select(-day) %>% 
  as_tsibble()

data_five_min <- read_csv("data_five_min.csv")%>% 
  select(-day) %>% 
  as_tsibble()

data_fifteen_min <- read_csv("data_fifteen_min.csv")%>% 
  select(-day) %>% 
  as_tsibble()

data_thirty_min <- read_csv("data_thirty_min.csv")%>% 
  select(-day) %>% 
  as_tsibble()

data_hour <- read_csv("data_hour.csv")%>% 
  select(-day) %>% 
  as_tsibble()



#Log transforming flow in 30 minute data
data_thirty_min <- data_thirty_min %>% 
  mutate(log_ammnonium=log10(ammonium_effluent_mg_L))


#Box Cox transform the flow

lambda <- data_thirty_min %>%
  features(ammonium_effluent_mg_L, features = guerrero) %>%
  pull(lambda_guerrero)


#square root
data_thirty_min <- data_thirty_min %>% 
  mutate(sqrt_ammonium=sqrt(ammonium_effluent_mg_L))






#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Plotting data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#---------------------
#Plotting ammonium
#---------------------

#raw ammonium values
data_thirty_min %>% 
  autoplot(ammonium_effluent_mg_L)

#log transformed ammonium values
data_thirty_min %>% 
  autoplot(log_ammnonium)


#Box cox transformed ammonium values
data_thirty_min %>%
  autoplot(box_cox(ammonium_effluent_mg_L, lambda))

#Sqrt transformed ammonium values
data_thirty_min %>% 
  autoplot(sqrt_ammonium)



#plotting seasons (year)
data_thirty_min %>% 
  select(ammonium_effluent_mg_L) %>% 
  fill_gaps() %>% 
  gg_season()


#plotting seasons (day)
data_thirty_min %>% 
  select(ammonium_effluent_mg_L) %>% 
  fill_gaps() %>% 
  gg_season(period = "day")


#plotting seasons (week)
data_thirty_min %>% 
  select(ammonium_effluent_mg_L) %>% 
  fill_gaps() %>% 
  gg_season(period = "week")



#plotting seasons (year) log data
data_thirty_min %>% 
  select(log_ammnonium) %>% 
  fill_gaps() %>% 
  gg_season()


#plotting seasons (day) log data
data_thirty_min %>% 
  select(log_ammnonium) %>% 
  fill_gaps() %>% 
  gg_season(period = "day")


#plotting seasons (week) log data
data_thirty_min %>% 
  select(log_ammnonium) %>% 
  fill_gaps() %>% 
  gg_season(period = "week")



#flow and rain in one plot
coeff=1

data_thirty_min %>% 
  ggplot(aes(x=time_thirty_min)) +
  
  geom_line( aes(y=ammonium_effluent_mg_L)) + 
  geom_line( aes(y=rainfall_mm * coeff), col="Red", alpha=0.5) + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Flow in the effluent [m3/h]",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Rain [mm]")
  )

#Plot of flow and ammonium concentration
coeff=400

data_thirty_min %>% 
  ggplot(aes(x=time_thirty_min)) +
  geom_line( aes(y=flow_effluent_m3_h)) + 
  geom_line( aes(y=ammonium_effluent_mg_L * coeff), 
             col="Red", 
             alpha=0.5) + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Flow in the effluent [m3/h]",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Rain [mm]")
  )



#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Missing values
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

ammonium_values_to_replace <- data_thirty_min %>% 
  select(ammonium_effluent_mg_L) %>% 
  filter(is.na(ammonium_effluent_mg_L))


#Periode of 2018-01-07 to 2018-01-08
#rain and ammonium
coeff=1

data_thirty_min %>% 
  filter_index("2018-01-01"~"2018-01-14") %>%  
  ggplot(aes(x=time_thirty_min)) +
  geom_line( aes(y=ammonium_effluent_mg_L)) + 
  geom_line( aes(y=rainfall_mm * coeff), 
             col="Red", 
             alpha=0.5) 


#Periode of 2018-07-07 to 2018-07-08
#rain and ammonium
data_thirty_min %>% 
  filter_index("2018-07-07"~"2018-07-08") %>%  
  ggplot(aes(x=time_thirty_min)) +
  geom_line( aes(y=ammonium_effluent_mg_L)) + 
  geom_line( aes(y=rainfall_mm), 
             col="Red", 
             alpha=0.5)


#Periode of 2018-10-26 to 2018-10-30
#rain and ammonium
data_thirty_min %>% 
  filter_index("2018-10-26"~"2018-10-30") %>%  
  ggplot(aes(x=time_thirty_min)) +
  geom_line( aes(y=ammonium_effluent_mg_L)) + 
  geom_line( aes(y=rainfall_mm), 
             col="Red", 
             alpha=0.5)

#Periode of 2019-01-09 to 2019-01-15
#rain and ammonium
data_thirty_min %>% 
  filter_index("2019-01-09"~"2019-01-15") %>%  
  ggplot(aes(x=time_thirty_min)) +
  geom_line( aes(y=ammonium_effluent_mg_L)) + 
  geom_line( aes(y=rainfall_mm), 
             col="Red", 
             alpha=0.5)


#Periode of 2020-11-03
#rain and ammonium
data_thirty_min %>% 
  filter_index("2020-11-03") %>%  
  ggplot(aes(x=time_thirty_min)) +
  geom_line( aes(y=ammonium_effluent_mg_L)) + 
  geom_line( aes(y=rainfall_mm), 
             col="Red", 
             alpha=0.5)


#Periode of 2021-12-29 to 2022-01-02
#rain and ammonium
data_thirty_min %>% 
  filter_index("2021-12-29"~"2022-01-02") %>%  
  ggplot(aes(x=time_thirty_min)) +
  geom_line( aes(y=ammonium_effluent_mg_L)) + 
  geom_line( aes(y=rainfall_mm), 
             col="Red", 
             alpha=0.5)

#Replacing the values
data_thirty_min <- data_thirty_min %>% 
  mutate(ammonium_effluent_mg_L=na.approx(ammonium_effluent_mg_L))


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Decomposition
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


#ammonium 
data_day %>% 
  fill_gaps() %>% 
  model(
    STL(ammonium_effluent_mg_L~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)
  ) %>% 
  components() %>%
  autoplot()



STL_thirty_min_data <- data_thirty_min %>%
  mutate(t = row_number()) %>%
  update_tsibble(index = t, 
                 regular = TRUE)

STL_thirty_min_data %>%
  model(
    STL(sqrt(ammonium_effluent_mg_L) ~ season(period = 24*2) +
          season(period = 24*2*7)+
          season(period = 24*2*365.25),
        robust = TRUE)
  ) %>%
  components() %>%
  autoplot() + labs(x = "Observation")


STL_thirty_min_data %>%
  model(
    STL(sqrt(ammonium_effluent_mg_L),
        robust = TRUE)
  ) %>%
  components() %>%
  autoplot() + labs(x = "Observation")




STL_thirty_min_data %>%
  model(
    STL(sqrt(flow_effluent_m3_h) ~ season(period = 24*2,
                                          window = "periodic") +
          season(period = 24*2*7,
                 window = "periodic")+
          season(period = 24*2*365.25,
                 window = "periodic"),
        robust = TRUE)
  ) %>%
  components() %>%
  autoplot() + labs(x = "Observation")

STL_thirty_min_data %>%
  model(
    STL(sqrt(flow_effluent_m3_h) ~ season(period = 24*2,
                                          window = 19) +
          season(period = 24*2*7,
                 window = 19)+
          season(period = 24*2*365.25,
                 window = 19),
        robust = TRUE)
  ) %>%
  components() %>%
  autoplot() + labs(x = "Observation")




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Modelling
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#plotting one day accumulated rain
data_thirty_min %>% 
  autoplot(rain_one_day_accumulated)


#training data
training <- data_thirty_min %>% 
  filter_index(~"2021") %>% 
  as_tsibble()


#test data
test <- data_thirty_min %>% 
  filter_index("2022"~.)



#------------------
#Simple linear regression
#------------------

#Define the model
simpel_TSLM <- training %>%
  model(TSLM(ammonium_effluent_mg_L ~ rainfall_mm)) 

#report the model
simpel_TSLM %>%
  report()

#plot the model
training %>%
  autoplot(ammonium_effluent_mg_L) +
  geom_line(data = fitted(simpel_TSLM),
            aes(y = .fitted, colour = .model)) +
  labs(y = "Ammonium concentration [mg/L]",
       title = "Simpel TSLM")+
  theme(legend.position="none")







#------------------
#multiple linear regression
#------------------

#Define the model
multi_TSLM <- training %>%
  model(TSLM(ammonium_effluent_mg_L~ rainfall_mm + 
               rain_two_day_accumulated + 
               rain_three_day_accumulated + 
               rain_four_day_accumulated + 
               rain_five_day_accumulated + 
               rain_six_day_accumulated + 
               rain_seven_day_accumulated +
               drought)) 

#report the model
multi_TSLM %>%
  report()

#plot the model
training %>%
  autoplot(ammonium_effluent_mg_L) + 
  geom_line(data = fitted(multi_TSLM),
            aes(y = .fitted, colour = .model)) +
  labs(y = "Ammonium concentration [mg/L]",
       title = "Multiple TSLM") +
  theme(legend.position="none")





#ARIMA without additional parameters
ARIMA_simpel <- training %>%
  fill_gaps() %>% 
  model(ARIMA(ammonium_effluent_mg_L~ rainfall_mm, stepwise=FALSE, approximation=FALSE, max.order=2))


#report the model
ARIMA_simpel %>%
  report()

#plot the model
training %>%
  autoplot(ammonium_effluent_mg_L) + 
  geom_line(data = fitted(ARIMA_simpel),
            aes(y = .fitted, colour = .model)) +
  labs(y = "Ammonium concentration [mg/L]",
       title = "ARIMA") +
  theme(legend.position="none")


#



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#bottom code
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Replacing the 3 month of data missing of the drought index
temp <- training %>% 
  as_tibble() %>% 
  filter(is.na(drought)) %>% 
  select(drought, time_thirty_min) %>% 
  mutate(count=1) %>% 
  mutate(day=as.Date(time_thirty_min,"day")) %>% 
  select(-time_thirty_min) %>% 
  group_by(day) %>% 
  summarise(sum_count=sum(count))
#Loading the three csv files containing the effluent data
temp1 <- read_delim(here::here("temp/aarhus-kommune-april-2018.csv"), 
                    delim=";",
                    col_types = cols(.default = "c"))
temp2 <- read_delim(here::here("temp/aarhus-kommune-juli-2021.csv"), 
                    delim=";",
                    col_types = cols(.default = "c"))

temp3 <- read_delim(here::here("temp/aarhus-kommune-oktober-2020.csv"), 
                    delim=";",
                    col_types = cols(.default = "c"))

#Creating the data frame containing the three csv file
extra_data_drought <- bind_rows(temp1,temp2,temp3) %>%
  as_tibble()


extra_data_drought <- extra_data_drought %>% 
  #Renaming the columns
  rename(day=DateTime, 
         drought_new="Tørkeindex") %>% 
  #Converting the time column to the correct format
  mutate(day=as.Date(day)) %>% 
  #Replacing comma with dot and converting to numeric
  mutate(drought_new= as.numeric(gsub(",", ".",  as.character(drought_new))))



data_thirty_min <- data_thirty_min%>% 
  mutate(day=as.Date(time_thirty_min)) %>% 
  left_join(extra_data_drought) %>%  
  mutate(drought=if_else(is.na(drought), 
                         drought_new, 
                         drought)) %>% 
  select(-drought_new,-day)


# 
# temp <- data_thirty_min %>%
#   select(rainfall_mm,
#          rain_two_day_accumulated,
#          rain_three_day_accumulated,
#          rain_four_day_accumulated,
#          rain_five_day_accumulated,
#          rain_six_day_accumulated,
#          rain_seven_day_accumulated) 
# 
# temp %>% 
#   GGally::ggpairs(columns = 1:7)


















