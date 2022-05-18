#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Setup of different helping tools 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#All comments refer to the code line below the comment
#Loading packages
library(tidyverse)
library(lubridate)
library(fpp3)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

#Making a list of all the csv file names that should be used
list_of_filenames <- list.files( here::here(getwd(), "Data"))

#Removing the folder with the effluent data
list_of_filenames <- list_of_filenames[-1]

#Defining a list of all the months of data
months_meausred <- c("2018-02","2018-03","2018-04",
                     "2018-05","2018-06","2018-07",
                     "2018-08","2018-09","2018-10",
                     "2018-11","2018-12",
                     "2019-01","2019-02","2019-03",
                     "2019-04","2019-05","2019-06",
                     "2019-07","2019-08","2019-09",
                     "2019-10","2019-11","2019-12",
                     "2020-01","2020-02","2020-03",
                     "2020-04","2020-05","2020-06",
                     "2020-07","2020-08","2020-09",
                     "2020-10","2020-11","2020-12",
                     "2021-01","2021-02","2021-03",
                     "2021-04","2021-05","2021-06",
                     "2021-07","2021-08","2021-09",
                     "2021-10","2021-11","2021-12",
                     "2022-01","2022-02","2022-03",
                     "2022-04"
)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Defining the functions to be used later 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------
#Loading the data and data pre-processing function
#-------------------------------------------

#Defining the function
load_and_preprocess_dataframe <- function(path){
  #Loading the csv file in to the function defined data frame
  #using the read_delim to specify that the file is separated by ;
  #converting all the data columns to charters, to prevent R form automatically converting the columns
  df <- read_delim(here::here("Data",path), 
                   delim=";",
                   col_types = cols(.default = "c"))
  
  # Change "," to "." and converting all columns to numeric, expect in the time column
  df[,-1] <- lapply(df[,-1], 
                    function(x) 
                      as.numeric(gsub(",", ".",  as.character(x))))
 
   # Change time format to the year-month-day hour-minute-second format
  df <- df %>% 
    mutate(DATETIME=ymd_hms(DATETIME))
}

#-------------------------------------------
#Averaging over time intervals function
#-------------------------------------------

#Define the function and specify the variables that shall be used
#df= data frame with smaller timer intervals, time_interval=the time interval that the new data frame should have
#old_time_column_name= the name of the time column in the df
#rain_fall_column= the name of the rain column of the df
average_over_time_interval <-function(df,
                                      time_interval,
                                      old_time_column_name,
                                      rain_fall_column){
  #As the rain is measured as accumulated rain over a year the new rain column
  #Should be based the sum of rain, not the average as with the other columns
  df1 <- df %>% 
    #Converting to tibble to make the functions work better
    as_tibble() %>% 
    #Selecting the time column and the rain_fall column, as all other columns should be subjected to averaging
    select({{old_time_column_name}},{{rain_fall_column}})%>% 
    #Defining a new time column where the time are round to down to the nearest time interval
    mutate(temp_name1=
             floor_date({{old_time_column_name}}, 
                        unit=time_interval))%>% 
    #Removing the old time column
    select(-{{old_time_column_name}}) %>% 
    #Grouping by the new time column 
    group_by(temp_name1) %>% 
    #Creating a new rain column which is the sum in each time interval
    summarise(sum_rain= sum({{rain_fall_column}})) %>% 
    #Converting the data back to a tsibble
    as_tsibble()
  
  #Converting all other columns expect the rain column to a average over the time interval
  df2 <- df %>% 
    #Converting to tibble to make the functions work better
    as_tibble() %>% 
    #Remove the rain fall column as this column has been subjected to summation in the early df
    select(-{{rain_fall_column}}) %>% 
    #Creating a new time column with values round down to the nearest time interval
    mutate(temp_name2=
             floor_date({{old_time_column_name}},
                        unit=time_interval))%>% 
    #Removing the old time column
    select(-{{old_time_column_name}}) %>% 
    #Grouping by the new time column
    group_by(temp_name2) %>% 
    #Overwrite all the existing columns with the average over the given time interval
    #this is done for all columns which are numeric, the na.rm=T makes the averaging robust to missing values
    summarise(across(where(is.numeric), mean, na.rm=T)) %>%
    #Convert the back to a tsibble
    as_tsibble()
  
  #Combine the to data frames by the columns
  df <- bind_cols(df1,df2) %>% 
    #Remove one of the time columns
    select(-temp_name2)
}


#-------------------------------------------
#Renaming the the columns
#-------------------------------------------

#Name structure: chemical_place at WWTP_type of data_unit

#Place at WWTP can be blank if the sensor is not measured at any specific location
#Type of data could be online sensor data, set point e.g. if blank assumed to be a online sensor

#Abbreviations: PT = process tank, SP = set point, AN = Anoxic tank, SS = suspended solids
#Flow is assumed to be flow of waste water, if the flow is of air it is specified
#HT = hydrolysis tank, COD = chemical oxygen demand, TN = total Nitrogen
#DO = dissolved oxygen, T = wastewater temperature, C = degrees Celsius
#SC = secondary clarifies, EU = electricity use, SF= Salsnes filter
#RAS= return activated sludge, ES = Excess sludge, CP = control parameter
#EL = External lab analysis, BOD = biological oxygen demand

rename_data <- function(df){
  #Load the new names from a separate csv file
  name_of_columns <- read_delim(here::here("Name_converter.csv"), delim=";")
  #Defining the old names as the column "Sensors"
  oldnames <- name_of_columns$Sensors
  #Defining the new names as the column New_names 
  newnames <- name_of_columns$New_names
  
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
  #using the dplyr rename_with function to rename comparing the column name
  #with the new names form csv file and replace them.
  df %>% 
    rename_with(~ newnames,
                all_of(oldnames))
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Merging all the data to one data frame 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Defining the first month in the list of month manually 
data_one_min <- load_and_preprocess_dataframe("Malte_apr2018.csv")
i=0
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_filenames){
  #Loading and pre-processing the data
  df1 <- load_and_preprocess_dataframe(file)
  
  #Skip the first manually assigned file 
  if (i>0) {
    data_one_min <- bind_rows(df1, data_one_min)
  }
  i=i+1
}

# #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# #-------------------------------------------
# #Merging the effluent csv files to the data frame
# #-------------------------------------------
# 
# 
# temp1 <- load_and_preprocess_dataframe("Effluent_data/Malte_effluent.csv")
# temp2 <- load_and_preprocess_dataframe("Effluent_data/Malte_effluent2.csv")
# temp3 <- load_and_preprocess_dataframe("Effluent_data/Malte_effluent3.csv")
# 
# temp4 <- bind_rows(temp1,temp2,temp3)
# 
# temp4 <- temp4 %>% 
#   as_tsibble() %>% 
#   filter_index(.~"2022-04-19 00:00:00")
# 
# 
# data_one_min <- bind_cols(data_one_min,temp4)
# 
rain_fall_data <- data_one_min %>% 
  select(Rainfall_mm,Time_one_min)

missing_rain_values <- rain_fall_data %>% 
  filter(is.na(Rainfall_mm)) %>% 
  mutate(conuter=1)

count_missing_values <- missing_rain_values %>% 
  mutate(Hour_time=floor_date(Time_one_min,"hour")) %>% 
  select(-Time_one_min, Rainfall_mm) %>%
  group_by(Hour_time) %>% 
  summarise(hour_count=sum(conuter))

count_missing_values %>% ggplot(aes(x=hour_count))+geom_density()

over_an_hour <- count_missing_values %>% filter(hour_count>49)

count_missing_values_days <- count_missing_values %>% 
  mutate(counter=1)%>% 
  mutate(Day_time=floor_date(Hour_time,"day")) %>% 
  select(-Hour_time, hour_count) %>%
  group_by(Day_time) %>% 
  summarise(day_count=sum(counter))
full_days_without_measurements <- count_missing_values_days %>% 
  filter(day_count==24)


#Mine forsøg hvor intet virkede
data_one_min <- data_one_min %>%  as_tsibble()


test2 <- data_one_min %>% filter_index("2018-05-12") %>% 
  select(Rainfall_mm,Time_one_min) %>% 
  mutate(Rainfall_mm=0)


test[match(test$Time_one_min,test2$Time_one_min),'Rainfall_mm'] <-test2$Rainfall_mm

data_one_min <- data_one_min %>% 
  mutate(Rainfall_mm=case_when(Time_one_min=="2018-05-12 00:00:00")~0)


list_of_days ="2018-05-12"

for (days in list_of_days){
  test <- test %>% 
    mutate(Rainfall_mm=0)
}

test <- data_one_min %>% 
  as_tsibble()

test <- test %>% 
  filter_index("2018-05-12") 

test %>% ggplot(aes(x=`Ammonium_AN_mg/L`,Ammonium_to_AN))+
  geom_point()

if (test$Time_one_min==is.period(as.Date("2018-05-12"))){
  print("YES")
}


test$Rainfall_mm[test$Rainfall_mm] <- 0

test_date <- floor_date(test$Time_one_min, "day") %>% 
  as.character()

index_test <- test_date=="2018-05-12"

index_test <- index_test*1

sum(index_test)

test3 <- test %>% 
  as_tibble() %>% 
  select(Time_one_min) %>% 
  mutate(rownum=row_number()) %>% 
  select(-Time_one_min)

index_test <- bind_cols(index_test,test3)

index_test <- index_test %>% 
  summarise(row_to_replace=...1*rownum)

test4 <- test 

test4$Rainfall_mm[which(as.character(test4$Time_one_min)=="2018-05-12")] <- 0

for (i in index_test){
    if(index_test==1){
      test[i,2] <- 0
  }
}
test %>% 
  filter_index("2018-05-12")
sum(test$Rainfall_mm)


test4[test3,"Rainfall_mm"] <- 0

test4 %>% as_tsibble() %>%
  filter_index("2018-05-12") 


#Lasses kode 
test_merge <-tibble(day = ymd(c("2018-05-12")), Rainfall_new = c(0))

test <- test %>% 
  mutate(day=floor_date(Time_one_min,"day"))

y <- test %>% select(Time_one_min,day,Rainfall_mm) %>% left_join(test_merge)

y <- y %>% 
  mutate(test_rain=if_else(is.na(Rainfall_mm), Rainfall_new, Rainfall_mm))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Cleaning of data frame
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------
#Finding all the column where all the values are NA
#-------------------------------------------
data_one_min <- data_one_min %>% 
  #Renaming the data
  rename_data()  


#-------------------------------------------
#Finding all the column where all the values are NA
#-------------------------------------------

#Overwriting the data frame selecting all the columns where all the data is NA
sensors_with_no_measurements <- data_one_min %>% 
    select(
    where(
        ~all(is.na(.x))
      )
    )  
  
#Saving this columns as a csv file
write_csv(sensors_with_no_measurements, "sensors_with_no_measurements.csv")


#-------------------------------------------
#Removing the columns where there are only NA values
#-------------------------------------------

#Overwriting the data frame selecting all the columns where all the data is not NA
data_one_min <- data_one_min %>% 
  select(
    where(
      ~!all(is.na(.x))
    )
  ) 


#-------------------------------------------
#Handling duplicates 
#-------------------------------------------

#Finding the duplicated values 
duplicated_time_values <- data_one_min[
  duplicated(
  data_one_min$DATETIME),
  ]

#Saving the duplicated values
write_csv(duplicated_time_values, "duplicated_time_values.csv")
  
#Removing the duplicated values from the data frame, with the dplyr distinct function
data_one_min <- data_one_min %>% 
  distinct(DATETIME,
           .keep_all = T)


#Renaming the time column
data_one_min <- data_one_min%>% 
  rename(Time_one_min=DATETIME)

#Taking the difference of the accumulated rain
data_one_min <- data_one_min %>% 
  mutate(Rainfall_mm=difference(Rainfall_mm))

#Analyzing the data, removing negative values and outliers
#Finding the negative values
rain_negative_values <- data_one_min %>% 
  filter(Rainfall_mm<0)

#Saving the data in a csv file
write_csv(rain_negative_values, "rain_negative_values.csv")

#Replacing the the negative values with zero
data_one_min$Rainfall_mm[data_one_min$Rainfall_mm<0] <- 0

#Creating a density plot of the rain values
data_one_min %>% ggplot(aes(x=Rainfall_mm))+geom_density()

data_one_min %>% filter(Rainfall_mm>10) %>%  
  ggplot(aes(x=Rainfall_mm))+geom_density()

data_one_min %>% filter(Rainfall_mm<10) %>%  
  ggplot(aes(x=Rainfall_mm))+geom_density()

#Peaks give large values in negative and positive direction
#As it is unlikely to have 10 mm rain in one minute this is the value treshold
rain_large_values <- data_one_min %>% 
  filter(Rainfall_mm>10)

#Saving the data in a csv file
write_csv(rain_large_values, "rain_large_values.csv")

#Replacing the the values over 10 to zero
data_one_min$Rainfall_mm[data_one_min$Rainfall_mm>10] <- 0

#Finding all the values where the rain is NA
rain_which_is_NA <- data_one_min %>% 
  filter(is.na(Rainfall_mm))

#Replacing all NA values with zero
data_one_min$Rainfall_mm[is.na(data_one_min$Rainfall_mm)] <- 0

#Saving the data in a csv file
write_csv(data_one_min, "data_one_min.csv")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Splitting the data in other time intervals
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#As the data frame with one minutes data is quite large, the data frame with the largest
#time interval is use to create the next one; one minute data is used to create five minutes data
#five minutes data is used to create fifteen minutes data and so on.
#As all of the data frames report the mean of the time interval, averaging the means of the 
#five minutes data is mathematically equivalent to averaging the one minte data

#-------------------------------------------
#Creating the a data frame with 5 minutes interval
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_one_min <- data_one_min %>% 
  as_tsibble()


#Finding the system time
t0 <- Sys.time()

data_five_min <- data_one_min %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("5 mins", 
                             Time_one_min, 
                             Rainfall_mm)

#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_one_min %>% 
    filter_index(months)%>% 
    average_over_time_interval("5 mins", 
                               Time_one_min, 
                               Rainfall_mm)
  
    data_five_min <- bind_rows(df1, data_five_min)
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
data_five_min <- data_five_min %>% 
  rename(Time_five_min=temp_name1)

data_five_min <- data_five_min %>% 
  rename(Rainfall_mm=sum_rain)


#Save the data as a csv file
write_csv(data_five_min, "data_five_min.csv")


#-------------------------------------------
#Creating the a data frame with 15 minutes interval
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_five_min <- data_five_min %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_fifteen_min <- data_five_min %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("15 mins", 
                             Time_five_min, 
                             Rainfall_mm)

#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_five_min %>% 
    filter_index(months)%>% 
    average_over_time_interval("15 mins", 
                               Time_five_min, 
                               Rainfall_mm)
  
    data_fifteen_min <- bind_rows(df1, data_fifteen_min)
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
data_fifteen_min <- data_fifteen_min %>% 
  rename(Time_fifteen_min=temp_name1)

data_fifteen_min <- data_fifteen_min %>% 
  rename(Rainfall_mm=sum_rain)

#Save the data as a csv file
write_csv(data_fifteen_min, "data_fifteen_min.csv")


#-------------------------------------------
#Creating the a data frame with 30 minutes interval
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_fifteen_min <- data_fifteen_min %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_thirty_min <- data_fifteen_min %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("30 mins", 
                             Time_fifteen_min, 
                             Rainfall_mm)

#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_fifteen_min %>% 
    filter_index(months)%>% 
    average_over_time_interval("30 mins", 
                               Time_fifteen_min, 
                               Rainfall_mm)
  
  #Skip the first manually assigned file 

    data_thirty_min <- bind_rows(df1, data_thirty_min)

}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
data_thirty_min <- data_thirty_min %>% 
  rename(Time_thirty_min=temp_name1)

data_thirty_min <- data_thirty_min %>% 
  rename(Rainfall_mm=sum_rain)


#Save the data as a csv file
write_csv(data_thirty_min, "data_thirty_min.csv")


#-------------------------------------------
#Creating the a data frame with 1 hour interval
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_thirty_min <- data_thirty_min %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_hour <- data_thirty_min %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("hour", 
                             Time_thirty_min, 
                             Rainfall_mm)
#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_thirty_min %>% 
    filter_index(months)%>% 
    average_over_time_interval("hour", 
                               Time_thirty_min, 
                               Rainfall_mm)

    data_hour <- bind_rows(df1, data_hour)

}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
data_hour <- data_hour %>% 
  rename(Time_hour=temp_name1)

data_hour <- data_hour %>% 
  rename(Rainfall_mm=sum_rain)

#Save the data as a csv file
write_csv(data_hour, "data_hour.csv")
 

#-------------------------------------------
#Creating the a data frame with 1 day interval
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_hour <- data_hour %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_day <- data_hour %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("day", 
                             Time_hour, 
                             Rainfall_mm)
#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_hour %>% 
    filter_index(months)%>% 
    average_over_time_interval("day", 
                               Time_hour, 
                               Rainfall_mm)
  
    data_day <- bind_rows(df1, data_day)

}

#Rename the time column
data_day <- data_day %>% 
  rename(Time_day=temp_name1)

data_day <- data_day %>% 
  rename(Rainfall_mm=sum_rain)

#Save the data as a csv file
write_csv(data_day, "data_day.csv")

#-------------------------------------------
#Creating the a data frame with 1 week interval
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_day <- data_day %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_week <- data_day %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("week", 
                             Time_day, 
                             Rainfall_mm)
#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_day %>% 
    filter_index(months)%>% 
    average_over_time_interval("week", 
                               Time_day, 
                               Rainfall_mm)
  
  data_week <- bind_rows(df1, data_week)
  
}

#Rename the time column
data_week <- data_week %>% 
  rename(Time_week=temp_name1)

data_week <- data_week %>% 
  rename(Rainfall_mm=sum_rain)

#Save the data as a csv file
write_csv(data_week, "data_week.csv")


#-------------------------------------------
#Creating the a data frame with 1 month interval
#-------------------------------------------


#Turn the data_one_min to a tsibble
data_week <- data_week %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_month <- data_week %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("month", 
                             Time_week, 
                             Rainfall_mm)
#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_week %>% 
    filter_index(months)%>% 
    average_over_time_interval("month", 
                               Time_week, 
                               Rainfall_mm)
  
  data_month <- bind_rows(df1, data_month)
  
}

#Rename the time column
data_month <- data_month %>% 
  rename(Time_day=temp_name1)

data_month <- data_month %>% 
  rename(Rainfall_mm=sum_rain)

#Save the data as a csv file
write_csv(data_month, "data_month.csv")