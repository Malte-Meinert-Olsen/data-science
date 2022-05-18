#All comments refer to the code line below the comment
#Loading packages
library(tidyverse)
library(lubridate)
library(fpp3)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)



rename_data <- function(df){
  #Load the new names from a separate csv file
  name_of_columns <- read_delim(here::here("Name_converter.csv"), delim=";")
  #Defining the old names as the column "Sensors"
  oldnames <- name_of_columns$Sensors
  #Defining the new names as the column NEW !!!!!! NAMES 
  newnames <- name_of_columns$`Old Names`
  
  #Matching the column names in the data frame with the sensors names in the csv file,
  #this creates a new vector of the position of all the matches, and returns NA
  #if a column is not matched with the sensors names in the csv file
  existing <- match(oldnames,names(df))
  
  #The if sentence is a security statement, that returns a warring if one or more
  #of the coulmns is not matched with the sensor names in the csv file
  if(sum(is.na(existing)) > 0)
    #If all the names are matches the sum of NA would be 0, therefore a warring message
    #is printed if the sum is largere than 0
  {
    #The warring message prints both the indices number and the name of the sensor
    print("The following indices did not match any columns:")
    print(which(is.na(existing)))
    print("Corresponding to these columns in oldnames")
    print(oldnames[which(is.na(existing))])
  }
  
  
  df %>% 
    rename_with(~ newnames,
                all_of(oldnames))
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#load all the data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


data_hourly <- read_csv("Data_hourly.csv") %>% 
  #rename_data()%>% 
  distinct(hourly,
           .keep_all = T) %>% 
  as_tsibble()%>% 
  select(
    where(
      ~!all(is.na(.x))
    )
  )



data_one_min <- read_csv("Data_one_min.csv") #%>% 
  

data_one_min <- data_one_min %>% distinct(Time_one_min,
                                          .keep_all = T) %>% 
  as_tsibble()

 #%>% 
 #rename_data()

data_five_minutes <- read_csv("Data_five_min.csv") %>% 
  distinct(fiveMin,
           .keep_all = T) %>% 
  as_tsibble() %>% 
  rename_data()%>% 
  select(
    where(
      ~!all(is.na(.x))
    )
  )


data_fifteen_minutes <- read_csv("Data_fifteen_min.csv") %>% 
  distinct(fifteenMin,
           .keep_all = T) %>% 
  as_tsibble() %>% 
  rename_data()%>% 
  select(
    where(
      ~!all(is.na(.x))
    )
  )
  
data_thirty_minutes <- read_csv("Data_thirty_min.csv") %>% 
  distinct(thirtyMin,
           .keep_all = T) %>% 
  as_tsibble() %>% 
  rename_data()%>% 
  select(
    where(
      ~!all(is.na(.x))
    )
  )


data_daily <- aggregate(data_hourly[], 
                         list(daily=cut(data_hourly$hourly, 
                                        "1 day")),
                         mean, na.rm=T) %>% 
  select(-hourly) %>% 
  mutate(daily=ymd(as.character(daily))) %>% 
  as_tsibble() 



data_month <- aggregate(data_daily[], 
                        list(monthly=cut(data_daily$daily, 
                                         "1 month")),
                        mean, na.rm=T) %>% 
  select(-daily) %>% 
  mutate(montly=yearmonth(as.character(monthly))) %>% 
  as_tsibble() 

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#summing the data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

test <- data_one_min %>% filter_index("2020-05")
str(test)


test_fcn <- function(df,
                     time_interval,
                     old_time_column_name,
                     rain_fall_column){

   df1 <- df %>% 
     as_tibble() %>% 
    select({{old_time_column_name}},{{rain_fall_column}})%>% 
    mutate(temp_name1=
             floor_date({{old_time_column_name}}, 
                        unit=time_interval))%>% 
    select(-{{old_time_column_name}}) %>% 
    group_by(temp_name1) %>% 
    summarise(sum_rain= sum({{rain_fall_column}})) %>% 
    as_tsibble()
 
 df2 <- df %>% 
   as_tibble() %>% 
   select(-{{rain_fall_column}}) %>% 
   mutate(temp_name2=
            floor_date({{old_time_column_name}},
                       unit=time_interval))%>% 
   select(-{{old_time_column_name}}) %>% 
   group_by(temp_name2) %>% 
   summarise(across(where(is.numeric), mean, na.rm=T)) %>% 
   as_tsibble()
  
 df <- bind_cols(df1,df2) %>% 
   select(-temp_name2)
}


test <- data_hourly %>% test_fcn("week", hourly, `EGA.RE-BL750-M_ANT_MM` )

test_2 <- test_fcn(test, 
                   "5 mins",
                   Time_one_min,
                   Rainfall_mm) 
#%>% 
 # rename(Date_time_min=temp_name1)



test_3 <- test_fcn(test_2,
                   "1 hour",
                   Date_time_min, 
                   sum_rain)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Plotting hourly data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#making the data frame
data_hourly <- data_hourly %>% 
  mutate(diff_rain1=c(NA, diff(`Nedbør Egå Rens. ÅTD (SVK-5180)`))) %>% 
  mutate(diff_rain1=if_else(diff_rain1<0,0,diff_rain1))
  

data_one_min <- data_one_min %>% 
  mutate(diff_rain1=c(NA, diff(`Nedbør Egå Rens. ÅTD (SVK-5180)`)))%>% 
  mutate(diff_rain1=if_else(diff_rain1<0,0,diff_rain1))

data_daily <- data_daily %>% 
  mutate(diff_rain1=c(NA, diff(`Nedbør Egå Rens. ÅTD (SVK-5180)`))) %>% 
  mutate(diff_rain1=if_else(diff_rain1<0,0,diff_rain1))

#rain
data_hourly %>%  autoplot(diff_rain1)
data_hourly %>% 
  filter_index("2019") %>%
  autoplot(diff_rain1)

data_one_min %>% 
  filter_index("2019") %>%
  autoplot(diff_rain1)



#Flow
coeff <- 0.01
data_daily %>% ggplot(aes(x=daily)) +
  
  geom_bar( aes(y=diff_rain1/coeff), stat="identity",  color="black") + 
  geom_line( aes(y=`Flow Udløb`), size=.5, alpha=.5, color="Red")+
  
  scale_y_continuous(sec.axis = sec_axis(~.*coeff, name="rain in mm")) 


coeff <- 0.004
data_hourly %>% filter(diff_rain1<20) %>% 
  ggplot(aes(x=hourly)) +
  
  geom_bar( aes(y=diff_rain1/coeff), stat="identity",  color="black") + 
  geom_line( aes(y=`Flow Udløb`), size=.5, alpha=.5, color="Red")+
  
  scale_y_continuous(sec.axis = sec_axis(~.*coeff, name="rain in mm")) 


data_hourly %>% filter(diff_rain1>20) %>% select(hourly)

temp <- data_hourly %>% filter_index("2018-08-21") %>% select(diff_rain1)
data_hourly %>% filter_index("2018-06-25"~"2018-12-15") %>% autoplot


data_hourly %>% autoplot(`Flow Udløb`)

data_hourly %>%  select(`Flow Udløb`) %>% fill_gaps() %>% 
  gg_season()

data_hourly %>%  select(`Flow Udløb`) %>% fill_gaps() %>% 
  gg_season(period = "day")

data_hourly %>%  select(`Flow Udløb`) %>% fill_gaps() %>% 
  gg_season(period = "week")


data_hourly %>%  select(`Flow Udløb`) %>% fill_gaps() %>% 
  gg_season(period = "month")

data_dayily %>% autoplot(`Flow Udløb`)

data_dayily %>% gg_season(`Flow Udløb`)



data_month %>% autoplot(`Flow Udløb`)

data_month %>% gg_subseries(`Flow Udløb`)

data_month %>% gg_season(`Flow Udløb`)


#Ammonium in pt4 tank

data_hourly  %>% autoplot(`Ammonium PT4`)
data_hourly %>% fill_gaps() %>%  gg_season(`Ammonium PT4`)
data_hourly %>% fill_gaps() %>%  gg_season(`Ammonium PT4`, period = "day")





data_hourly %>% 
  select(`Flow til AN`,`Ammonium PT4`) %>%
  as.numeric() %>% 
  cor()
cor(data_hourly$`Ammonium PT4`,data_hourly$`Flow til AN`)


data_hourly %>% ggplot(aes(x=`Flow til AN`,y=`Ammonium PT4`))+
  geom_point()

data_dayily %>% ggplot(aes(x=`Flow til AN`,y=`Ammonium PT4`))+
  geom_point()



data_dayily <- aggregate(data_hourly[], 
                              list(daily=cut(data_hourly$hourly, 
                                                  "1 day")),
                              mean, na.rm=T) %>% 
  select(-hourly) %>% 
  mutate(daily=ymd(as.character(daily))) %>% 
  as_tsibble()



data_month <- aggregate(data_dayily[], 
                         list(monthly=cut(data_dayily$daily, 
                                        "1 month")),
                         mean, na.rm=T) %>% 
  select(-daily) %>% 
  mutate(montly=yearmonth(as.character(monthly))) %>% 
  as_tsibble()



temp <- data_dayily %>% filter_index("2021-07")
temp2 <- data_hourly %>% filter_index("2021-07")
temp3 <- data_one_min %>% filter_index("2021-07")

temp %>% autoplot(`Flow til AN`)
temp2 %>% autoplot(`Flow til AN`)
temp3 %>% autoplot(`Flow til AN`)
temp3 %>% autoplot(`Ammonium PT4`)
str(data_hourly)

temp3 %>% autoplot(`Nedbør Egå Rens. ÅTD (SVK-5180)`,`Flow til AN`,`Ammonium PT4`)

gridExtra::grid.arrange(temp3 %>% 
                          autoplot(`Nedbør Egå Rens. ÅTD (SVK-5180)`),
                        temp3 %>% 
                          autoplot(`Flow til AN`),
                        temp3 %>% 
                          autoplot(`Ammonium PT4`)
                        )



gridExtra::grid.arrange(temp3 %>% 
                          filter_index("2021-07-17"~"2021-07-19") %>% 
                          autoplot(`Flow til AN`),
                        temp3 %>% 
                          filter_index("2021-07-17"~"2021-07-19") %>% 
                          autoplot(`Ammonium PT4`)
)


gridExtra::grid.arrange(temp3 %>% 
                          filter_index("2021-07-17") %>% 
                          autoplot(`Flow til AN`),
                        temp3 %>% 
                          filter_index("2021-07-17") %>% 
                          autoplot(`Ammonium PT4`)
)

 


data_one_min %>% 
  filter_index("2021-07-25"~"2021-07-31") %>% 
  autoplot(`Flow til AN`)

gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2021-07-25"~"2021-07-31") %>% 
                          autoplot(`Nedbør Egå Rens. ÅTD (SVK-5180)`),
                        data_one_min %>% 
                          filter_index("2021-07-25"~"2021-07-31") %>% 
                          autoplot(`Flow til AN`),
                        data_one_min %>% 
                          filter_index("2021-07-25"~"2021-07-31") %>% 
                          autoplot(`Ammonium AN-tank`)
  
)

data_one_min %>% 
  filter_index("2021-07-25"~"2021-07-31") %>% 
  autoplot(`Ammonium AN-tank`)
data_one_min %>% 
  filter_index("2021-07-17"~"2021-07-19") %>% 
  autoplot(`Ammonium AN-tank`)



#In may now

gridExtra::grid.arrange(data_one_min %>% 
                          filter_index("2021-05") %>% 
                          autoplot(`Flow til AN`),
                        data_one_min %>% 
                          filter_index("2021-05") %>% 
                          autoplot(`Ammonium PT4`),
                        data_one_min %>% 
                          filter_index("2021-05") %>% 
                          autoplot(`Nedbør Egå Rens. ÅTD (SVK-5180)`),
                        data_one_min %>% 
                          filter_index("2021-05") %>% 
                          autoplot(`Ammonium AN-tank`)
)



gridExtra::grid.arrange(hour %>% 
                          autoplot(`Vandtemperatur Procestank 1`),
                        data_one_min %>% 
                          autoplot(`Vandtemperatur Procestank 2`),
                        data_one_min %>% 
                          autoplot(`Vandtemperatur Procestank 3`),
                        data_one_min %>% 
                          autoplot(`Temperatur T4`)
                        
)
















data_hourly %>% duplicates()


temp <- data_hourly %>% duplicates()
data_one_min <- read_csv("Data_1min.csv") 
data_one_min %>% as_tsibble()

data_one_min <- data_one_min %>% 
  distinct(Date_time_min,
           .keep_all = T) %>% 
  as_tsibble()


#for fun make a model of the system

#decomposition
data_daily %>%
  model(
    STL(`Flow til AN` ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


stl_dcmp <- data_daily %>% 
  model(
    STL(`Flow til AN` ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE))
components(stl_dcmp)

components(stl_dcmp) %>%
  as_tsibble() %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2")

#forecast that no rain will come the next 2 weeks
rain_future <- new_data(data_daily,14) %>% 
  mutate(diff_rain1=0)

#ARIMA + Linear model
data_daily %>%
  pivot_longer(c(diff_rain1,
                 `Flow til AN`),
               names_to = "var", values_to = "value") %>%
  ggplot(aes(x = daily, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y")

fit <- data_daily %>%
  model(ARIMA(`Flow til AN` ~ diff_rain1))
report(fit)

fit %>% gg_tsresiduals()

fc <- fit %>% forecast(new_data=rain_future)

fc %>%
  autoplot(data_daily) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit))





fit2 <- data_daily %>%
  model(TSLM(`Flow til AN` ~ diff_rain1))
report(fit2)

fit2 %>% gg_tsresiduals()

fc2 <- fit2 %>% forecast(new_data=rain_future)


fc2 %>%
  autoplot(data_daily) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit2))




fit3 <- data_daily %>%
  model(TSLM(`Flow til AN` ~ diff_rain1 + season()))
report(fit3)

fit3 %>% gg_tsresiduals()

fc3 <- fit3 %>% forecast(new_data=rain_future)


fc3 %>%
  autoplot(data_daily) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit3))


fit4 <- data_daily %>%
  model(TSLM(`Flow til AN` ~ diff_rain1 + season_year()+trend()))
report(fit4)

fit4 %>% gg_tsresiduals()

fc4 <- fit4 %>% forecast(new_data=rain_future)


fc4 %>%
  autoplot(data_daily) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit4))


fit5 <- data_daily %>%
  model(TSLM(`Flow til AN` ~ diff_rain1 + trend() + fourier(K=2)))
report(fit5)

fit5 %>% gg_tsresiduals()

fc5 <- fit5 %>% forecast(new_data=rain_future)


fc5 %>%
  autoplot(data_daily) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit5))
