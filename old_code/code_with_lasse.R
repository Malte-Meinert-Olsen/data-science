#Load the data and packages
library(tidyverse)
library(lubridate)
library(fpp3)



# meget vigtig ædnring 
data_one_min <- read_csv("data_one_min.csv")


#En ændring mere
data_one_min <- data_one_min %>% 
  select(-day) %>% 
  as_tsibble()

data_five_min <- read_csv("data_five_min.csv")%>% 
  select(-day) %>% 
  as_tsibble()

data_fifteen_min <- read_csv("data_fifteen_min.csv")%>% 
  select(-day) %>% 
  as_tsibble()

data_thirty_min <- read_csv("data/data_thirty_min.csv")%>% 
  select(-day) %>% 
  as_tsibble()

data_hour <- read_csv("data_hour.csv")%>% 
  select(-day) %>% 
  as_tsibble()



#Log transforming flow in 30 minute data
data_thirty_min <- data_thirty_min %>% 
  mutate(log_flow=log10(flow_effluent_m3_h))

#Box Cox transform the flow

lambda <- data_thirty_min %>%
  features(flow_effluent_m3_h, features = guerrero) %>%
  pull(lambda_guerrero)
data_thirty_min %>%
  autoplot(box_cox(flow_effluent_m3_h, lambda))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Plotting data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#---------------------
#Plotting flow
#---------------------

#raw flow values
data_thirty_min %>% 
  autoplot(flow_effluent_m3_h)


#log transformed flow values
data_thirty_min %>% 
  autoplot(log_flow)


#Box cox transformed flow values
data_thirty_min %>%
  autoplot(box_cox(flow_effluent_m3_h, lambda))




#plotting seasons (year)
data_thirty_min %>% 
  select(flow_effluent_m3_h) %>% 
  fill_gaps() %>% 
  gg_season()


#plotting seasons (day)
data_thirty_min %>% 
  select(flow_effluent_m3_h) %>% 
  fill_gaps() %>% 
  gg_season(period = "day")


#plotting seasons (week)
data_thirty_min %>% 
  select(flow_effluent_m3_h) %>% 
  fill_gaps() %>% 
  gg_season(period = "week")



#plotting seasons (year) log data
data_thirty_min %>% 
  select(log_flow) %>% 
  fill_gaps() %>% 
  gg_season()


#plotting seasons (day) log data
data_thirty_min %>% 
  select(log_flow) %>% 
  fill_gaps() %>% 
  gg_season(period = "day")


#plotting seasons (week) log data
data_thirty_min %>% 
  select(log_flow) %>% 
  fill_gaps() %>% 
  gg_season(period = "week")



#Investigate wierd things in the flow
na_values <- data_thirty_min %>% 
  filter(is.na(flow_effluent_m3_h)) %>% 
  select(flow_effluent_m3_h)

values_which_is_zero <- data_thirty_min %>% 
  filter(flow_effluent_m3_h==0) %>% 
  select(flow_effluent_m3_h)


#flow and rain in one plot
coeff=400

data_thirty_min %>% 
  ggplot(aes(x=time_thirty_min)) +
  
  geom_line( aes(y=flow_effluent_m3_h)) + 
  geom_line( aes(y=rainfall_mm * coeff), col="Red", alpha=0.5) + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Flow in the effluent [m3/h]",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Rain [mm]")
  )

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#graphically checking rain and flow and sum of rain from 1 to 7 days
#to see if they are correlated
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#flow and rain
data_thirty_min %>% 
  ggplot(aes(x=flow_effluent_m3_h, 
             y=rainfall_mm))+
  geom_point()

#flow and sum of two days
data_thirty_min %>% 
  ggplot(aes(x=rain_two_day_accumulated,
             y=flow_effluent_m3_h))+
  geom_point()

#flow and sum of three days
data_thirty_min %>% 
  ggplot(aes(x=rain_three_day_accumulated, 
             y=flow_effluent_m3_h))+
  geom_point()

#flow and sum of four days
data_thirty_min %>% 
  ggplot(aes(x=rain_four_day_accumulated, 
             y=flow_effluent_m3_h))+
  geom_point()

#flow and sum of five days
data_thirty_min %>% 
  ggplot(aes(x=rain_five_day_accumulated, 
             y=flow_effluent_m3_h))+
  geom_point()

#flow and sum of six days
data_thirty_min %>% 
  ggplot(aes(x=rain_six_day_accumulated, 
             y=flow_effluent_m3_h))+
  geom_point()

#flow and sum of seven days
data_thirty_min %>% 
  ggplot(aes(x=rain_seven_day_accumulated, 
             y=flow_effluent_m3_h))+
  geom_point()

#flow and drought index
data_thirty_min %>% 
  ggplot(aes(x=drought, 
             y=flow_effluent_m3_h))+
  geom_point()

#rain and drought index
data_thirty_min %>% 
  ggplot(aes(x=drought, 
             y=rainfall_mm))+
  geom_point()

#sum of seven days of rain and drought index
data_thirty_min %>%
  ggplot(aes(x=drought, 
           y=rain_seven_day_accumulated))+
  geom_point()


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


#Creating a column with the differenced flow
data_thirty_min <- data_thirty_min %>% 
  mutate(diff_flow=difference(flow_effluent_m3_h))

#plotting different flow and ammonium
coeff=200

data_thirty_min %>% 
  ggplot(aes(x=time_thirty_min)) +
  geom_line( aes(y=diff_flow)) + 
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





#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#nitrate plots
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#nitrate plot
data_thirty_min %>% 
  autoplot(nitrate_effluent_mg_L)

data_thirty_min %>% 
  filter(nitrate_effluent_mg_L<200) %>% 
  autoplot(nitrate_effluent_mg_L)



#nitrate and flow
coeff=50

data_thirty_min %>% 
  filter(nitrate_effluent_mg_L<200) %>% 
  ggplot(aes(x=time_thirty_min)) +
  geom_line( aes(y=flow_effluent_m3_h)) + 
  geom_line( aes(y=nitrate_effluent_mg_L * coeff), 
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


data_thirty_min %>% 
  gg_lag(ammonium_effluent_mg_L)

data_day %>% 
  gg_lag(ammonium_effluent_mg_L)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#ACF plots
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

data_thirty_min %>%
  select(ammonium_effluent_mg_L) %>% 
  fill_gaps() %>% 
  ACF(lag_max = 48*2) %>% 
  autoplot()

data_thirty_min %>%
  select(flow_effluent_m3_h) %>% 
  fill_gaps() %>% 
  ACF(lag_max = 48*2) %>% 
  autoplot()

data_thirty_min %>%
  select(nitrate_effluent_mg_L) %>% 
  fill_gaps() %>% 
  ACF(lag_max = 48*2) %>% 
  autoplot()


data_day %>%
  select(ammonium_effluent_mg_L) %>% 
  fill_gaps() %>% 
  ACF(lag_max = 48*2) %>% 
  autoplot()

data_day %>%
  select(flow_effluent_m3_h) %>% 
  fill_gaps() %>% 
  ACF(lag_max = 48*2) %>% 
  autoplot()

data_day %>%
  select(nitrate_effluent_mg_L) %>% 
  fill_gaps() %>% 
  ACF(lag_max = 48*2) %>% 
  autoplot()



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Graphical importance of filtering
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#flow

gridExtra::grid.arrange(
  data_one_min %>% 
    autoplot(flow_effluent_m3_h),
  data_five_min %>% 
    autoplot(flow_effluent_m3_h),
  data_fifteen_min %>% 
    autoplot(flow_effluent_m3_h),
  data_thirty_min %>% 
    autoplot(flow_effluent_m3_h),
  data_hour %>% 
    autoplot(flow_effluent_m3_h),
  data_day %>% 
    autoplot(flow_effluent_m3_h),
  data_week %>% 
    autoplot(flow_effluent_m3_h)
)



#































df1 <- data_thirty_min %>% 
  #Converting to tibble to make the functions work better
  as_tibble() %>% 
  #Selecting the time column and the rain_fall column, as all other columns should be subjected to averaging
  select(time_thirty_min,rainfall_mm)%>% 
  #Defining a new time column where the time are round to down to the nearest time interval
  mutate(temp_name1=
           as.Date(time_thirty_min))%>% 
  #Removing the old time column
  select(-time_thirty_min) %>% 
  #Grouping by the new time column 
  group_by(temp_name1) %>% 
  #Creating a new rain column which is the sum in each time interval
  summarise(sum_rain= sum(rainfall_mm)) %>% 
  #Converting the data back to a tsibble
  as_tsibble()

#Converting all other columns expect the rain column to a average over the time interval
df2 <- data_thirty_min %>% 
  #Converting to tibble to make the functions work better
  as_tibble() %>% 
  #Remove the rain fall column as this column has been subjected to summation in the early df
  select(-rainfall_mm) %>% 
  #Creating a new time column with values round down to the nearest time interval
  mutate(temp_name2=
           as.Date(time_thirty_min))%>% 
  #Removing the old time column
  select(-time_thirty_min) %>% 
  #Grouping by the new time column
  group_by(temp_name2) %>% 
  #Overwrite all the existing columns with the average over the given time interval
  #this is done for all columns which are numeric, the na.rm=T makes the averaging robust to missing values
  summarise(across(where(is.numeric), mean, na.rm=T)) %>%
  #Convert the back to a tsibble
  as_tsibble()

#Combine the to data frames by the columns
data_day <- bind_cols(df1,df2) %>% 
  #Remove one of the time columns
  select(-temp_name2)


data_day <- average_over_time_interval(data_thirty_min,
                                       "day",
                                       time_thirty_min,
                                       rainfall_mm)

data_day %>% 
  ggplot(aes(x=flow_effluent_m3_h, 
             y=sum_rain))+
  geom_point()

data_day %>% 
  ggplot(aes(x=flow_effluent_m3_h, 
             y=rain_seven_day_accumulated))+
  geom_point()

data_day %>% 
  ggplot(aes(x=flow_effluent_m3_h, 
             y=ammonium_effluent_mg_L))+
  geom_point()

data_day %>% 
  ggplot(aes(x=sum_rain, 
             y=ammonium_effluent_mg_L))+
  geom_point()




#Creating a column with the differenced flow
data_day <- data_day %>% 
  mutate(diff_flow=difference(flow_effluent_m3_h))

#plotting different flow and ammonium
coeff=200

data_day %>% 
  ggplot(aes(x=temp_name1)) +
  geom_line( aes(y=diff_flow)) + 
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







df1 <- data_thirty_min %>% 
  #Converting to tibble to make the functions work better
  as_tibble() %>% 
  #Selecting the time column and the rain_fall column, as all other columns should be subjected to averaging
  select(time_thirty_min,rainfall_mm)%>% 
  #Defining a new time column where the time are round to down to the nearest time interval
  mutate(temp_name1=
           yearweek(time_thirty_min))%>% 
  #Removing the old time column
  select(-time_thirty_min) %>% 
  #Grouping by the new time column 
  group_by(temp_name1) %>% 
  #Creating a new rain column which is the sum in each time interval
  summarise(sum_rain= sum(rainfall_mm)) %>% 
  #Converting the data back to a tsibble
  as_tsibble()

#Converting all other columns expect the rain column to a average over the time interval
df2 <- data_thirty_min %>% 
  #Converting to tibble to make the functions work better
  as_tibble() %>% 
  #Remove the rain fall column as this column has been subjected to summation in the early df
  select(-rainfall_mm) %>% 
  #Creating a new time column with values round down to the nearest time interval
  mutate(temp_name2=
           yearweek(time_thirty_min))%>% 
  #Removing the old time column
  select(-time_thirty_min) %>% 
  #Grouping by the new time column
  group_by(temp_name2) %>% 
  #Overwrite all the existing columns with the average over the given time interval
  #this is done for all columns which are numeric, the na.rm=T makes the averaging robust to missing values
  summarise(across(where(is.numeric), mean, na.rm=T)) %>%
  #Convert the back to a tsibble
  as_tsibble()

#Combine the to data frames by the columns
data_week <- bind_cols(df1,df2) %>% 
  #Remove one of the time columns
  select(-temp_name2)


data_week %>% 
  ggplot(aes(x=flow_effluent_m3_h, 
             y=sum_rain))+
  geom_point()




data_week %>% 
  ggplot(aes(x=flow_effluent_m3_h, 
             y=rain_seven_day_accumulated))+
  geom_point()

data_week %>% 
  ggplot(aes(x=flow_effluent_m3_h, 
             y=ammonium_effluent_mg_L))+
  geom_point()
data_week %>% 
  ggplot(aes(x=sum_rain, 
             y=ammonium_effluent_mg_L))+
  geom_point()

