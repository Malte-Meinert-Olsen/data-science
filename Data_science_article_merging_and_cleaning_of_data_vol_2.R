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
#-------------------------------------------
#Loading the data and data pre-processing function
#-------------------------------------------
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
#-------------------------------------------
#Averaging over time intervals function
#-------------------------------------------
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
#-------------------------------------------
#Renaming the the columns
#-------------------------------------------
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


#-------------------------------------------
#-------------------------------------------
#Finding how many minutes are NA in a hour
#-------------------------------------------
#-------------------------------------------

#Defining function
counting_minutes_missing_in_an_hour <- function(df,
                                                column_to_check_for_NA)
{
  #Print how many values are missing in the entire column
  df %>% 
    #Select all the values which is NA
    filter(is.na({{column_to_check_for_NA}})) %>% 
    #Count the length of the column
    nrow() %>% 
    #Print the length
    print
  
  #Create a date frame with time, all the NA values, and a counter
  df <- df %>% 
    #Select the time and column of interest
    select(time_one_min,{{column_to_check_for_NA}}) %>% 
    #Removing all the values which is not NA
    filter(is.na({{column_to_check_for_NA}})) %>% 
    #Making a new column with all values to be 1
    mutate(conuter=1)
  
  #Create a data frame which count how many NA values there are in an hour
  df <- df %>% 
    #Round all time values down to the nearest hour
    mutate(hour_time=floor_date(time_one_min,"hour")) %>% 
    #Removing time column with minutes and the column with NA values
    select(-time_one_min, {{column_to_check_for_NA}}) %>%
    #Group by the hour 
    group_by(hour_time) %>% 
    #Sum all counter values within the hour and add to a new column
    summarise(hour_count=sum(conuter))
}


#-------------------------------------------
#-------------------------------------------
#Finding how many hours have a NA in a day
#-------------------------------------------
#-------------------------------------------


#Defining the function
#This function requires that the data frame is the format which is the output by the 
#counting_minutes_missing_in_an_hour
counting_hours_with_a_NA_in_a_day <- function(df_hour_data)
{
  
  #Defining the data frame
  df_hour_data <- df_hour_data%>% 
    #Create a new column which is assigned one to each hour
    mutate(counter=1)%>% 
    #Rounding down the time to each day
    mutate(day_time=floor_date(hour_time,"day")) %>% 
    #Removing the hourly count from the previous function and the hour time
    select(-hour_time, hour_count) %>%
    #Grouping by the day
    group_by(day_time) %>% 
    #Creating a new column which sum all the hours which has one or more missing values
    summarise(day_count=sum(counter))
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Merging all the data to one data frame 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Defining the first month in the list of month manually 
data_one_min <- load_and_preprocess_dataframe("Malte_apr2018.csv")
#Defining the counter
i=0
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_filenames){
  #Loading and pre-processing the data
  df1 <- load_and_preprocess_dataframe(file)
  
  #Skip the first manually assigned file 
  if (i>0) {
    data_one_min <- bind_rows(df1, data_one_min)
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
          "sensors_with_no_measurements.csv")


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
          "duplicated_time_values.csv")

#Removing the duplicated values from the data frame, with the dplyr distinct function
data_one_min <- data_one_min %>% 
  distinct(DATETIME,
           .keep_all = T)

#Renaming the time column
data_one_min <- data_one_min%>% 
  rename(time_one_min=DATETIME)


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
          "rain_negative_values.csv")

#Replacing the the negative values with zero
data_one_min$rainfall_mm[data_one_min$rainfall_mm<0] <- 0

#Peaks give large values in negative and positive direction
#As it is unlikely to have 5.5 mm rain in one minute this is the value threshold
#due to the danish record of rain is 5.4 mm/min
rain_large_values <- data_one_min %>% 
  filter(rainfall_mm>5.5)

#Saving the data in a csv file
write_csv(rain_large_values, 
          "rain_large_values.csv")

#Replacing the the values over 10 to zero
data_one_min$rainfall_mm[data_one_min$rainfall_mm>5.5] <- 0


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
data_eflluent <- bind_rows(temp1,temp2,temp3) %>%
  #Removing data after the 2022-04-19 as this data was extracted later than the other data
  as_tsibble() %>% 
  filter_index(.~"2022-04-19") %>% 
  as_tibble()

#Renaming all the columns
data_eflluent <- data_eflluent%>% 
  rename(time_one_min=DATETIME,
         ammonium_effluent_mg_L=`EGA.AM-FI734-M_NH4N`,
         nitrate_effluent_mg_L=`EGA.NM-FI735-M_NO3N`)


#joining the data by the time column
data_one_min <- data_one_min %>% 
  left_join(data_eflluent)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Finding periods with missing data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------
#-------------------------------------------
#Rain fall data
#-------------------------------------------
#-------------------------------------------

#Finding periods with NA, and overwriting them based on DMI weather archive 


#Finding the missing data in the rainfall column hour based
rain_missing_hour <- counting_minutes_missing_in_an_hour(data_one_min, 
                                                         rainfall_mm)
#Converting the missing data from the rain column to day based 
rain_missing_day <- counting_hours_with_a_NA_in_a_day(rain_missing_hour)

#Making a data frame with dates with no rain verified by DMI weather archives
temp_merge <- tibble(day = ymd(c("2018-05-12",
                                 "2018-05-13",
                                 "2018-05-14",
                                 "2018-05-15",
                                 "2018-05-16",
                                 "2018-05-17",
                                 "2018-05-18",
                                 "2018-05-19",
                                 "2018-05-20",
                                 "2018-05-21",
                                 "2018-05-22",
                                 "2018-05-23",
                                 "2018-05-24",
                                 "2018-05-25",
                                 "2018-05-26",
                                 "2018-05-27",
                                 "2018-02-05",
                                 "2018-02-04",	
                                 "2018-01-25",
                                 "2018-01-26",
                                 "2018-01-30",
                                 "2018-01-21",
                                 "2018-01-08",
                                 "2018-01-19",
                                 "2018-02-18",
                                 "2018-02-24",
                                 "2018-05-11",
                                 "2019-01-10",
                                 "2019-01-14",
                                 "2018-07-02",
                                 "2018-07-09",
                                 "2018-07-21",
                                 "2018-06-26")), 
                     rainfall_new = c(0))

#Replacing all the NA values were there was no rain with 0
data_one_min <- data_one_min %>% 
  #Creating a column with round down dates
  mutate(day=floor_date(time_one_min,"day")) %>% 
  #Merging the data frame with no rain dates with the original data frame, 
  #joining them by the day date
  left_join(temp_merge) %>%  
  #Replacing the NA values in the original data frame with values from the data frame
  #containg the 0 on dates with no rain
  mutate(rainfall_mm=if_else(is.na(rainfall_mm), 
                             rainfall_new, 
                             rainfall_mm)) %>%
  #Removing the day dates and the column containing 0 on those dates.
  select(-day,-rainfall_new)

#Creating the hours with missing values after replacing some NA with 0
rain_new_missing_values_hour <- counting_minutes_missing_in_an_hour(data_one_min, 
                                                                    rainfall_mm) 
#Creating the days with missing values after replacing some NA with 0
rain_new_missing_values_day <- counting_hours_with_a_NA_in_a_day(rain_new_missing_values_hour)


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
  mutate(rainfall_new= as.numeric(gsub(",", ".",  as.character(rainfall_new)))) %>% 
  #Converting the rain data from rain pr. hour to rain pr. minute
  mutate(rainfall_new=rainfall_new/60)



data_one_min <- data_one_min %>% 
  mutate(hour=floor_date(time_one_min,"hour")) %>% 
  left_join(data_DMI_rain) %>%  
  mutate(rainfall_mm=if_else(is.na(rainfall_mm), 
                             rainfall_new, 
                             rainfall_mm)) %>%
  select(-hour,-rainfall_new)


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Make a joining of the drought data


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
  mutate(drought= as.numeric(gsub(",", ".",  as.character(drought)))) %>% 
  distinct(day,
           .keep_all = T) %>% 
  as_tsibble(index=day) %>% 
  filter_index(~"2022-04-19") %>% 
  as_tibble()


data_one_min <- data_one_min %>% 
  mutate(day=floor_date(time_one_min,"day")) %>% 
  left_join(data_DMI_drought) %>% 
  select(-day)



data_one_min <- data_one_min %>% 
  test %>%
  mutate(day = as.Date(time_one_min))%>%
  mutate(rain_one_day_accumulated = map_dbl(day,
                                              ~sum(rainfall_mm[(day > . - 1) & 
                                                                 (day < .)])))%>%
  mutate(rain_two_day_accumulated = map_dbl(day,
                                              ~sum(rainfall_mm[(day > . - 2) & 
                                                                 (day < .)])))%>%
  mutate(rain_three_day_accumulated = map_dbl(day,
                                              ~sum(rainfall_mm[(day > . - 3) & 
                                                                 (day < .)])))%>%
  mutate(rain_four_day_accumulated = map_dbl(day,
                                              ~sum(rainfall_mm[(day > . - 4) & 
                                                                 (day < .)])))%>%
  mutate(rain_five_day_accumulated = map_dbl(day,
                                              ~sum(rainfall_mm[(day > . - 5) & 
                                                                 (day < .)])))%>%
  mutate(rain_six_day_accumulated = map_dbl(day,
                                              ~sum(rainfall_mm[(day > . - 6) & 
                                                                 (day < .)]))) %>%
  mutate(rain_seven_day_accumulated = map_dbl(day,
                                              ~sum(rainfall_mm[(day > . - 7) & 
                                                                 (day < .)])))





test <- data_one_min %>% 
  select(time_one_min,rainfall_mm) %>% 
  as_tsibble() %>% 
  filter_index("2019-02") %>% 
  filter(!is.na())






test <- test %>%
  as_tibble() %>% 
  mutate(day = as.Date(time_one_min)) %>%
  mutate(rain_seven_day_accumulated = map_dbl(day,
                                 ~sum(rainfall_mm[(day > . - 7) & 
                                                    (day < .)])))
test %>% autoplot(Order_History)
test %>% autoplot(rainfall_mm)

# 
# 
# test2 <- test2 %>%
#   mutate(SOLVED_DATE = as.Date(temp_name1)) %>%
#   mutate(Order_History = map_dbl(SOLVED_DATE, 
#                                  ~sum(sum_rain[(SOLVED_DATE > . - 7) & (SOLVED_DATE < .)])))
# 
# ggplot(test2, aes(x=temp_name1, y=Order_History))+
#   geom_bar(stat='identity')
# 
# ggplot(test2, aes(x=temp_name1, y=sum_rain))+
#   geom_bar(stat='identity')
# 
# test2 %>% autoplot(rainfall_mm)
# 

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Selecting the columns of interest
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


test <- data_one_min





#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#make the accumulated rain column

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Saving the final data frame with one minut data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

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
#-------------------------------------------
#Creating the a data frame with 5 minutes interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_one_min <- data_one_min %>% 
  as_tsibble()


#Finding the system time
t0 <- Sys.time()

data_five_min <- data_one_min %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("5 mins", 
                             time_one_min, 
                             rainfall_mm)

#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_one_min %>% 
    filter_index(months)%>% 
    average_over_time_interval("5 mins", 
                               time_one_min, 
                               rainfall_mm)
  
  data_five_min <- bind_rows(df1, data_five_min)
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


#Save the data as a csv file
write_csv(data_five_min, 
          "data_five_min.csv")


#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 15 minutes interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_five_min <- data_five_min %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_fifteen_min <- data_five_min %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("15 mins", 
                             time_five_min, 
                             rainfall_mm)

#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_five_min %>% 
    filter_index(months)%>% 
    average_over_time_interval("15 mins", 
                               time_five_min, 
                               rainfall_mm)
  
  data_fifteen_min <- bind_rows(df1, data_fifteen_min)
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

#Save the data as a csv file
write_csv(data_fifteen_min, 
          "data_fifteen_min.csv")


#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 30 minutes interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_fifteen_min <- data_fifteen_min %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_thirty_min <- data_fifteen_min %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("30 mins", 
                             time_fifteen_min, 
                             rainfall_mm)

#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_fifteen_min %>% 
    filter_index(months)%>% 
    average_over_time_interval("30 mins", 
                               time_fifteen_min, 
                               rainfall_mm)
  
  #Skip the first manually assigned file 
  
  data_thirty_min <- bind_rows(df1, data_thirty_min)
  
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


#Save the data as a csv file
write_csv(data_thirty_min, 
          "data_thirty_min.csv")


#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 1 hour interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_thirty_min <- data_thirty_min %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_hour <- data_thirty_min %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("hour", 
                             time_thirty_min, 
                             rainfall_mm)
#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_thirty_min %>% 
    filter_index(months)%>% 
    average_over_time_interval("hour", 
                               time_thirty_min, 
                               rainfall_mm)
  
  data_hour <- bind_rows(df1, data_hour)
  
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

#Save the data as a csv file
write_csv(data_hour, 
          "data_hour.csv")


#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 1 day interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_hour <- data_hour %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_day <- data_hour %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("day", 
                             time_hour, 
                             rainfall_mm)
#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_hour %>% 
    filter_index(months)%>% 
    average_over_time_interval("day", 
                               time_hour, 
                               rainfall_mm)
  
  data_day <- bind_rows(df1, data_day)
  
}

#Rename the time column
data_day <- data_day %>% 
  rename(time_day=temp_name1)

#Renaming the rain column
data_day <- data_day %>% 
  rename(rainfall_mm=sum_rain)

#Save the data as a csv file
write_csv(data_day, 
          "data_day.csv")


#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 1 week interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_day <- data_day %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_week <- data_day %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("week", 
                             time_day, 
                             rainfall_mm)
#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_day %>% 
    filter_index(months)%>% 
    average_over_time_interval("week", 
                               time_day, 
                               rainfall_mm)
  
  data_week <- bind_rows(df1, data_week)
  
}

#Rename the time column
data_week <- data_week %>% 
  rename(time_week=temp_name1)

#Renaming the rain column
data_week <- data_week %>% 
  rename(rainfall_mm=sum_rain)

#Save the data as a csv file
write_csv(data_week, 
          "data_week.csv")


#-------------------------------------------
#-------------------------------------------
#Creating the a data frame with 1 month interval
#-------------------------------------------
#-------------------------------------------

#Turn the data_one_min to a tsibble
data_week <- data_week %>% 
  as_tsibble()

#Finding the system time
t0 <- Sys.time()

data_month <- data_week %>% 
  filter_index("2018-01") %>% 
  average_over_time_interval("month", 
                             time_week, 
                             rainfall_mm)
#Merging all the csv file in one data frame and preprocess the csv files
for(months in months_meausred){
  
  df1 <- data_week %>% 
    filter_index(months)%>% 
    average_over_time_interval("month", 
                               time_week, 
                               rainfall_mm)
  
  data_month <- bind_rows(df1, data_month)
  
}

#Rename the time column
data_month <- data_month %>% 
  rename(time_day=temp_name1)

#Renaming the rain column
data_month <- data_month %>% 
  rename(rainfall_mm=sum_rain)

#Save the data as a csv file
write_csv(data_month, 
          "data_month.csv")