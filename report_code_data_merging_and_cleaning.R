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


#Load the functions
source("report_code_function_for_data_merging_and_cleaning.R")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Merging all the data to one data frame 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Defining the first month in the list of month manually 
data_one_min <- load_and_preprocess_dataframe("raw_data/Malte_apr2018.csv")

#Defining the counter
i=0

#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_filenames){
  #Loading and pre-processing the data
  df1 <- load_and_preprocess_dataframe(file)
  
  #Skip the first manually assigned file 
  if (i>0) {
    data_one_min <- bind_rows(df1, 
                              data_one_min)
  }
  
  #Adding one to the counter
  i=i+1
  #printing the counter to follow the progress of the code
  print(i)
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Cleaning of data frame
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------
#-------------------------------------------
#Renaming the data frame according to a manually specified csv file 
#-------------------------------------------
#-------------------------------------------

data_one_min <- data_one_min %>% 
  #Renaming the data
  rename_data()  


#-------------------------------------------
#-------------------------------------------
#Finding the column where all the values are NA
#-------------------------------------------
#-------------------------------------------

#Overwriting the data frame selecting all the columns where all the data is NA
sensors_with_no_measurements <- data_one_min %>% 
  select(
    where(
      ~all(is.na(.x))
    )
  )  

#Saving this columns as a csv file
write_csv(sensors_with_no_measurements, 
          "data/sensors_with_no_measurements.csv")


#-------------------------------------------
#-------------------------------------------
#Removing the columns where there are only NA values
#-------------------------------------------
#-------------------------------------------

#Overwriting the data frame selecting all the columns where all the data is not NA
data_one_min <- data_one_min %>% 
  select(
    where(
      ~!all(is.na(.x))
    )
  ) 


#-------------------------------------------
#-------------------------------------------
#Handling duplicates 
#-------------------------------------------
#-------------------------------------------

#Finding the duplicated values 
duplicated_time_values <- data_one_min[
  duplicated(
    data_one_min$DATETIME),
]

#Saving the duplicated values
write_csv(duplicated_time_values, 
          "data/duplicated_time_values.csv")

#Removing the duplicated values from the data frame, with the dplyr distinct function
data_one_min <- data_one_min %>% 
  distinct(DATETIME,
           .keep_all = T)

#Renaming the time column
data_one_min <- data_one_min%>% 
  rename(time_one_min=DATETIME)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Merging effluent data into the data frame
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Loading the three csv files containing the effluent data
temp1 <- load_and_preprocess_dataframe("Effluent_data/Malte_effluent.csv")
temp2 <- load_and_preprocess_dataframe("Effluent_data/Malte_effluent2.csv")
temp3 <- load_and_preprocess_dataframe("Effluent_data/Malte_effluent3.csv")

#Creating the data frame containing the three csv file
data_effluent <- bind_rows(temp1,temp2,temp3) %>%
  #Removing data after the 2022-04-19 as this data was extracted later than the other data
  as_tsibble() %>% 
  #removing data after 2022-04-19
  filter_index(.~"2022-04-19") %>%
  #Convert to tibble
  as_tibble()

#Renaming all the columns
data_effluent <- data_effluent%>% 
  rename(time_one_min=DATETIME,
         ammonium_effluent_mg_L=`EGA.AM-FI734-M_NH4N`,
         nitrate_effluent_mg_L=`EGA.NM-FI735-M_NO3N`)


#joining the data by the time column
data_one_min <- data_one_min %>% 
  left_join(data_effluent)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Rain fall data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------
#-------------------------------------------
#Cleaning and removing outliers in rain data
#-------------------------------------------
#-------------------------------------------

#Taking the difference of the accumulated rain, to get actual rain fall
data_one_min <- data_one_min %>% 
  mutate(rainfall_mm=difference(rainfall_mm))

#Analyzing the data, removing negative values and outliers
#Finding the negative values
rain_negative_values <- data_one_min %>% 
  filter(rainfall_mm<0)

#Saving the data in a csv file
write_csv(rain_negative_values, 
          "data/rain_negative_values.csv")

#Replacing the the negative values with zero
data_one_min$rainfall_mm[data_one_min$rainfall_mm<0] <- 0

#Peaks give large values in negative and positive direction
#As it is unlikely to have 5.5 mm rain in one minute this is the value threshold
#due to the danish record of rain is 5.4 mm/min
rain_large_values <- data_one_min %>% 
  filter(rainfall_mm>5.5)

#Saving the data in a csv file
write_csv(rain_large_values, 
          "data/rain_large_values.csv")

#Replacing the the values over 10 to zero
data_one_min$rainfall_mm[data_one_min$rainfall_mm>5.5] <- 0


#-------------------------------------------
#-------------------------------------------
#Finding periods with missing data
#-------------------------------------------
#-------------------------------------------

#Finding periods with NA, and overwriting them based on DMI weather archive 

#Finding the missing data in the rainfall column hour based
rain_missing_hour <- counting_minutes_missing_in_an_hour(data_one_min, 
                                                         rainfall_mm)
#Converting the missing data from the rain column to day based 
rain_missing_day <- counting_hours_with_a_NA_in_a_day(rain_missing_hour)


#Making a list of all the csv file names that should be used
list_of_file_DMI_data <- list.files( here::here(getwd(), "DMI_rain_data"))

#Defining the first month in the list of month manually 
data_DMI_rain <- read_delim(here::here("DMI_rain_data",
                                       "aarhus-kommune-1.-februar-2018.csv"), 
                            delim=";",
                            col_types = cols(.default = "c"))

#Resetting the counter i to 0
i=0
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_file_DMI_data){
  #Loading and pre-processing the data
  df1 <- read_delim(here::here("DMI_rain_data",file), 
                    delim=";",
                    col_types = cols(.default = "c"))
  
  #Skip the first manually assigned file 
  if(i>0){
    data_DMI_rain <- bind_rows(df1, data_DMI_rain)
  }
  #Add one to the counter
  i=i+1  
  
}

#Creating a data frame on the data extracted from DMI
data_DMI_rain <- data_DMI_rain %>% 
  #Renaming the columns
  rename(hour=DateTime,rainfall_new=Nedbør) %>% 
  #Converting the time column to the correct format
  mutate(hour=ymd_hms(hour)) %>% 
  #Replacing comma with dot and converting to numeric
  mutate(rainfall_new= as.numeric(gsub(",",
                                       ".", 
                                       as.character(rainfall_new)))) %>% 
  #Converting the rain data from rain pr. hour to rain pr. minute
  mutate(rainfall_new=rainfall_new/60) %>%  
  distinct(hour,
           .keep_all = T)


#overwriting the NA rain values
data_one_min <- data_one_min %>% 
  #convert the time to a date instead of a ymd_hms
  mutate(hour=floor_date(time_one_min,
                         "hour")) %>% 
  #Join the data from DMI's weather archives to the data frame
  left_join(data_DMI_rain) %>%  
  #Replace the NA values in the rain column and leave values which is not NA
  mutate(rainfall_mm=if_else(is.na(rainfall_mm), 
                             rainfall_new, 
                             rainfall_mm)) %>%
  #remove the hourly time column and the DMI's weather archive data
  select(-hour,-rainfall_new)

#Manually overwriting two values which was not converted properly by the last code
data_one_min[2213191,2] <- 0 
data_one_min[2213192,2] <- 0 


#Creating the hours with missing values after replacing some NA with 0
rain_new_missing_values_hour <- counting_minutes_missing_in_an_hour(data_one_min, 
                                                                    rainfall_mm) 
#Creating the days with missing values after replacing some NA with 0
rain_new_missing_values_day <- counting_hours_with_a_NA_in_a_day(rain_new_missing_values_hour)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Drought index data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Making a list of all the csv file names that should be used
list_of_file_DMI_drought_data <- list.files( here::here(getwd(), 
                                                        "DMI_drought_data"))

#Defining the first month in the list of month manually 
data_DMI_drought <- read_delim(here::here("DMI_drought_data",
                                          "aarhus-kommune-april-2018.csv"), 
                               delim=";",
                               col_types = cols(.default = "c"))

#Resetting the counter i to 0
i=0
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_file_DMI_drought_data){
  #Loading and pre-processing the data
  df1 <- read_delim(here::here("DMI_drought_data",file), 
                    delim=";",
                    col_types = cols(.default = "c"))
  
  #Skip the first manually assigned file 
  if(i>0){
    data_DMI_drought <- bind_rows(df1,
                                  data_DMI_drought)
  }
  #Add one to the counter
  i=i+1  
  
}

#Creating a data frame on the data extracted from DMI
data_DMI_drought <- data_DMI_drought %>% 
  #Renaming the columns
  rename(day=DateTime, 
         drought="Tørkeindex") %>% 
  #Converting the time column to the correct format
  mutate(day=as.Date(day)) %>% 
  #Replacing comma with dot and converting to numeric
  mutate(drought= as.numeric(gsub(",", 
                                  ".",  
                                  as.character(drought)))) %>% 
  #Removing duplicates based on time
  distinct(day,
           .keep_all = T) %>% 
  #Convert to tsibble
  as_tsibble(index=day) %>% 
  #Removing data after 2022-04-19
  filter_index(~"2022-04-19") %>% 
  #Convert to tibble
  as_tibble()

#Adding the drought data to the data frame
data_one_min <- data_one_min %>% 
  #Creating a column with time in a day formate
  mutate(day=as.Date(time_one_min,"day")) %>% 
  #Joining drought index to the data frame
  left_join(data_DMI_drought) %>% 
  #Removing the day time column 
  select(-day)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Summing and saving the one minute data frame
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------
#-------------------------------------------
#Sum over 1 to 7 days
#-------------------------------------------
#-------------------------------------------

#Converting the data frame to a tsibble
data_one_min <- data_one_min %>% 
  as_tsibble()

#Saving the data frame as a temp data frame, for reference
temp <- data_one_min

#Finding the system time
t0 <- Sys.time()

#Resetting the counter
i=0

#Splitting the data frame into month, enable the PC to work with the data, without 
#crashing do to lost working memory
data_one_min <- temp %>% 
  #Selecting the first month with data measured 
  filter_index("2018-01") %>%
  #Applying the summing function (1440 is minutes pr. day)
  sum_over_rain_data(rainfall_mm, 
                     1440)

#Splitting the data frame into months and preform the rain summation
for(months in months_meausred){
  
  df1 <- temp %>% 
    #Spilt the data into all the months in the list of months
    filter_index(months)%>%
    #Apply the summing function (1440 is minutes pr. day)
    sum_over_rain_data(rainfall_mm, 
                       1440)
  
  #Bind all the data frame together in one data frame again
  data_one_min <- bind_rows(df1, 
                            data_one_min)
  
  #Add one to the counter
  i=i+1
  
  #Print the counter to follow the process of the code
  print(i)
  
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Removing duplicates based on time values, by keeping the first 
#observation and removing the rest
data_one_min <- data_one_min%>%  
  distinct(time_one_min,
           .keep_all = T)


#-------------------------------------------
#-------------------------------------------
#Saving the final data frame with one minut data
#-------------------------------------------
#-------------------------------------------

#Saving the data in a csv file
write_csv(data_one_min, 
          "data/data_one_min.csv")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Averaging the data over other time intervals
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#As the data frame with one minutes data is quite large, the data frame with the largest
#time interval is use to create the next one; one minute data is used to create five minutes data
#five minutes data is used to create fifteen minutes data and so on.
#As all of the data frames report the mean of the time interval, averaging the means of the 
#five minutes data is mathematically equivalent to averaging the one minute data
#the rainfall column is however summed not averaged

#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 5 minutes interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_one_min <- data_one_min %>%
  #Removing the accumulated rain columns
  select(-rain_one_day_accumulated, 
         -rain_two_day_accumulated, 
         -rain_three_day_accumulated, 
         -rain_four_day_accumulated,
         -rain_five_day_accumulated, 
         -rain_six_day_accumulated, 
         -rain_seven_day_accumulated) %>% 
  #Converting to a tsibble
  as_tsibble(index=time_one_min)


#Finding the system time
t0 <- Sys.time()

#Create the five minute interval for the first month of the data frame, and 
#creating the accumulated rain columns
data_five_min <- data_one_min %>% 
  #Select the first month measures
  filter_index("2018-01") %>% 
  #Average (and sum) over 5 minute time interval
  average_over_time_interval("5 mins", 
                             time_one_min, 
                             rainfall_mm) %>% 
  #Create the columns with accumulated rain (288 5-min pr. day)
  sum_over_rain_data(sum_rain, 
                     288)

#Create the five minute interval for the rest of the months of the data frame
for(months in months_meausred){
  
  df1 <- data_one_min %>% 
    #Select the month
    filter_index(months)%>% 
    #Average (and sum) over 5 minute time interval
    average_over_time_interval("5 mins", 
                               time_one_min, 
                               rainfall_mm) %>% 
    #Create the columns with accumulated rain (288 5-min pr. day)
    sum_over_rain_data(sum_rain, 
                       288)
  
  #Bind all the months together agian in one data frame
  data_five_min <- bind_rows(df1, 
                             data_five_min)
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
data_five_min <- data_five_min %>% 
  rename(time_five_min=temp_name1)

#Renaming the rain column
data_five_min <- data_five_min %>% 
  rename(rainfall_mm=sum_rain)

#removing duplicated time values keeping the first and removing the rest
data_five_min <- data_five_min%>%  
  distinct(time_five_min,
           .keep_all = T)

#Save the data as a csv file
write_csv(data_five_min, 
          "data/data_five_min.csv")


#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 15 minutes interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_five_min <- data_five_min %>% 
  #Removing the accumulated rain columns
  select(-rain_one_day_accumulated, 
         -rain_two_day_accumulated, 
         -rain_three_day_accumulated, 
         -rain_four_day_accumulated,
         -rain_five_day_accumulated, 
         -rain_six_day_accumulated, 
         -rain_seven_day_accumulated) %>% 
  #Converting to a tsibble
  as_tsibble(index=time_five_min)

#Finding the system time
t0 <- Sys.time()

#Create the five minute interval for the first month of the data frame, and 
#creating the accumulated rain columns
data_fifteen_min <- data_five_min %>% 
  #Selecting the first month
  filter_index("2018-01") %>% 
  #Average and sum over the first month to get a 15-min time interval
  average_over_time_interval("15 mins", 
                             time_five_min, 
                             rainfall_mm) %>% 
  #Creating the accumulated rain columns for the first month (96 15-min pr. day)
  sum_over_rain_data(sum_rain, 
                     96)

#Creating the 15 min time interval
for(months in months_meausred){
  
  df1 <- data_five_min %>% 
    filter_index(months)%>% 
    #Average and sum over the month to get a 15-min time interval
    average_over_time_interval("15 mins", 
                               time_five_min, 
                               rainfall_mm) %>% 
    #Creating the accumulated rain columns (96 15-min pr. day)
    sum_over_rain_data(sum_rain, 
                       96)
  
  #Merging all the months back to one data frame
  data_fifteen_min <- bind_rows(df1, 
                                data_fifteen_min)
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
data_fifteen_min <- data_fifteen_min %>% 
  rename(time_fifteen_min=temp_name1)

#Renaming the rain column
data_fifteen_min <- data_fifteen_min %>% 
  rename(rainfall_mm=sum_rain)

#Removing duplicated time values, by keeping the first and removing the rest
data_fifteen_min <- data_fifteen_min%>%  
  distinct(time_fifteen_min,
           .keep_all = T)

#Save the data as a csv file
write_csv(data_fifteen_min, 
          "data/data_fifteen_min.csv")


#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 30 minutes interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_fifteen_min <- data_fifteen_min %>% 
  #Removing the accumulated rain columns
  select(-rain_one_day_accumulated, 
         -rain_two_day_accumulated, 
         -rain_three_day_accumulated, 
         -rain_four_day_accumulated,
         -rain_five_day_accumulated, 
         -rain_six_day_accumulated, 
         -rain_seven_day_accumulated) %>%
  #Converting to a tsibble
  as_tsibble(index=time_fifteen_min)

#Finding the system time
t0 <- Sys.time()

#Create the five minute interval for the first month of the data frame, and 
#creating the accumulated rain columns
data_thirty_min <- data_fifteen_min %>%
  #Selecting the first month
  filter_index("2018-01") %>% 
  #Averaging (and summing) over the 30 minutes interval
  average_over_time_interval("30 mins", 
                             time_fifteen_min, 
                             rainfall_mm) %>% 
  #Creating the accumulated rain columns (48 30-min pr. day)
  sum_over_rain_data(sum_rain,
                     48)

#Creating the 30 min time interval
for(months in months_meausred){
  
  df1 <- data_fifteen_min %>% 
    #Selecting the month
    filter_index(months)%>% 
    #Averaging (and summing) over the 30 minutes interval
    average_over_time_interval("30 mins", 
                               time_fifteen_min, 
                               rainfall_mm) %>% 
    #Creating the accumulated rain columns (48 30-min pr. day)
    sum_over_rain_data(sum_rain,
                       48)
  
  #Bind the months back to one data frame
  data_thirty_min <- bind_rows(df1, 
                               data_thirty_min)
  
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
data_thirty_min <- data_thirty_min %>% 
  rename(time_thirty_min=temp_name1)

#Renaming the rain column
data_thirty_min <- data_thirty_min %>% 
  rename(rainfall_mm=sum_rain)

#Removing the duplicated time values and keeping the first value and removing the rest
data_thirty_min <- data_thirty_min%>%  
  distinct(time_thirty_min,
           .keep_all = T)

#Save the data as a csv file
write_csv(data_thirty_min, 
          "data/data_thirty_min.csv")


#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 1 hour interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_thirty_min <- data_thirty_min %>%
  #Removing the accumulated rain columns
  select(-rain_one_day_accumulated, 
         -rain_two_day_accumulated, 
         -rain_three_day_accumulated, 
         -rain_four_day_accumulated,
         -rain_five_day_accumulated, 
         -rain_six_day_accumulated, 
         -rain_seven_day_accumulated) %>% 
  #Converting to a tsibble
  as_tsibble(index=time_thirty_min)

#Finding the system time
t0 <- Sys.time()

#Create the five minute interval for the first month of the data frame, and 
#creating the accumulated rain columns
data_hour <- data_thirty_min %>% 
  #selecting the first month
  filter_index("2018-01") %>% 
  #Averaging (and summing) over the first time interval
  average_over_time_interval("hour", 
                             time_thirty_min, 
                             rainfall_mm) %>% 
  #Creating the accumulated rain columns (24 hours pr. day)
  sum_over_rain_data(sum_rain,
                     24)


#Creating the hourly time interval
for(months in months_meausred){
  
  df1 <- data_thirty_min %>%
    #Select the month
    filter_index(months)%>% 
    #Averaging (and summing) over the month
    average_over_time_interval("hour", 
                               time_thirty_min, 
                               rainfall_mm)%>% 
    #Creating the accumulated rain columns (24 hours pr. day)
    sum_over_rain_data(sum_rain,
                       24)
  
  #Combining all the months in one data frame
  data_hour <- bind_rows(df1, 
                         data_hour)
  
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
data_hour <- data_hour %>% 
  rename(time_hour=temp_name1)

#Renaming the rain column
data_hour <- data_hour %>% 
  rename(rainfall_mm=sum_rain)

#Remove duplicated time values keep the first value and remove the rest
data_hour <- data_hour%>%  
  distinct(time_hour,
           .keep_all = T)

#Save the data as a csv file
write_csv(data_hour, 
          "data/data_hour.csv")
