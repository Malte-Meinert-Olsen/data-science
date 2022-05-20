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
list_of_filenames <- list.files( here::here(getwd(), "raw_data"))

#Removing the folder with the effluent data
list_of_filenames <- list_of_filenames[-1]

#Defining a list of all the months of data, expect for 2018-01
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
#-------------------------------------------------------------------------------
#Defining the functions  
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Loading the data and data pre-processing function
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Defining the function
load_and_preprocess_dataframe <- function(path){
  #Loading the csv file in to the function defined data frame
  #using the read_delim to specify that the file is separated by ;
  #converting all the data columns to charters, to prevent R form 
  #automatically converting the columns to numbers, which gives problems 
  #with commas and dots
  df <- read_delim(here::here("raw_data",path), 
                   delim=";",
                   col_types = cols(.default = "c"))
  
  # Change "," to "." and converting all columns to numeric, expect in the time column
  df[,-1] <- lapply(df[,-1], 
                    function(x) 
                      as.numeric(gsub(",", ".",  as.character(x))))
  
  # Change the time column to the year-month-day hour-minute-second format
  df <- df %>% 
    mutate(DATETIME=ymd_hms(DATETIME))
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Averaging over time intervals function
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Define the function and specify the variables that shall be used
#df= data frame with smaller timer intervals, 
#time_interval=the time interval that the new data frame should have, needs a string
#old_time_column_name= the name of the time column in the df
#rain_fall_column= the name of the rain column of the df
average_over_time_interval <-function(df,
                                      time_interval,
                                      old_time_column_name,
                                      rain_fall_column){
  #As the rain is measured as accumulated rain over a year the new rain column
  #should be based the sum of rain, not the average as with the other columns
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
    #Remove the rain fall column as this column has been subjected to summation in df1
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


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Renaming the the columns
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Name structure: [chemical]_[place at WWTP_type of data]_[unit]

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
  name_of_columns <- read_delim(here::here("data/Name_converter.csv"), delim=";")
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
    #is printed if the sum is larger than 0
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
#Finding how many minutes are NA in a hour
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

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


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Finding how many hours have a NA in a day
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Defining the function
#This function requires that the data frame is the format which is the output by 
#the counting_minutes_missing_in_an_hour
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
#Printing the NA values for each column
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Defining the function
print_NA <- function(column_name){
  
  #Preprocess the data to count how many values which are missing in each hour
  temp1 <- counting_minutes_missing_in_an_hour(data_one_min,
                                               {{column_name}})
  
  #Preprocess the data to count how many hours a day have at least one missing value
  temp2 <- counting_hours_with_a_NA_in_a_day(temp1) 
  
  #Printing how many hours have a missing value
  temp1 %>% 
    nrow() %>% 
    print()
  
  #Removing all hours which has less than 50 minutes of missing data
  temp1 <- temp1 %>% 
    filter(hour_count > 49)
  
  #Printing how many hours have 50 or more missing data points
  temp1 %>% 
    nrow() %>% 
    print()
  
  #Printing how many days have missing values
  temp2 %>%
    nrow() %>% 
    print()
  
  #Printing how many days have more than 15 hours where a least one value is missing
  temp2 %>% 
    filter(day_count > 15) %>% 
    nrow %>% 
    print()
  
  #Finding the all the hours where there are more than 50 missing data points within the hour
  temp1 <- temp1 %>% 
    select(hour_time)
  
  #Binding all the hours which have atleast 50 missing data points
  times_to_get <- bind_rows(times_to_get,
                            temp1)
}



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#summing over 1 to 7 dayes of rain column 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Defining the function
sum_over_rain_data <- function(df, rain_column_name, time_intervals_pr_day){
  
  df <- df %>%
    #Creating a column with accumulated rain for one day with the rollappplyr function
    mutate(rain_one_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                 width=1*time_intervals_pr_day, 
                                                 FUN=sum, 
                                                 partial=T))%>%
    #Creating a column with accumulated rain for two day with the rollappplyr function
    mutate(rain_two_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                 width=2*time_intervals_pr_day, 
                                                 FUN=sum, 
                                                 partial=T))%>%
    #Creating a column with accumulated rain for three day with the rollappplyr function
    mutate(rain_three_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                   width=3*time_intervals_pr_day, 
                                                   FUN=sum, 
                                                   partial=T))%>%
    #Creating a column with accumulated rain for four day with the rollappplyr function
    mutate(rain_four_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                  width=4*time_intervals_pr_day, 
                                                  FUN=sum, 
                                                  partial=T))%>%
    #Creating a column with accumulated rain for five day with the rollappplyr function
    mutate(rain_five_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                  width=5*time_intervals_pr_day, 
                                                  FUN=sum, 
                                                  partial=T))%>%
    #Creating a column with accumulated rain for six day with the rollappplyr function
    mutate(rain_six_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                  width=6*time_intervals_pr_day, 
                                                  FUN=sum, 
                                                  partial=T)) %>%
    #Creating a column with accumulated rain for seven day with the rollappplyr function
    mutate(rain_seven_day_accumulated = rollapplyr({{rain_column_name}}, 
                                                  width=7*time_intervals_pr_day, 
                                                  FUN=sum, 
                                                  partial=T))
}


