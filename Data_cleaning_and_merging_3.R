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

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#-------------------------------------------
#Merging the effluent csv files to the data frame
#-------------------------------------------


temp1 <- load_and_preprocess_dataframe("Malte_udløb.csv")
temp2 <- load_and_preprocess_dataframe("Malte_udløb2.csv")
temp3 <- load_and_preprocess_dataframe("Malte_udløb3.csv")

temp4 <- bind_rows(temp1,temp2,temp3)

data_one_min <- bind_cols(data_one_min,temp4)


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
data_one_min %>% filter(Rainfall_mm<0) %>% view() 


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

#Finding the system time
t0 <- Sys.time()

#Create the 5 minutes time interval data frame
data_five_min <- data_one_min %>% 
  average_over_time_interval("5 mins", Time_one_min, Rainfall_mm)

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
#!!!!!!!!!!!!!!!!!!!
pass


#Save the data as a csv file
write_csv(data_five_min, "data_five_min.csv")


#-------------------------------------------
#Creating the a data frame with 15 minutes interval
#-------------------------------------------

#Finding the system time
t0 <- Sys.time()

#Create the 15 minutes time interval data frame
data_fifteen_min <- data_five_min %>% 
  average_over_time_interval()

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
#!!!!!!!!!!!!!!!!!!!
pass

#Save the data as a csv file
write_csv(data_fifteen_min, "data_fifteen_min.csv")


#-------------------------------------------
#Creating the a data frame with 30 minutes interval
#-------------------------------------------

#Finding the system time
t0 <- Sys.time()

#Create the 30 minutes time interval data frame
data_thirty_min <- data_fifteen_min %>% 
  average_over_time_interval()

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
#!!!!!!!!!!!!!!!!!!!
pass

#Save the data as a csv file
write_csv(data_thirty_min, "data_thirty_min.csv")


#-------------------------------------------
#Creating the a data frame with 1 hour interval
#-------------------------------------------

#Finding the system time
t0 <- Sys.time()

#Create the 1 hour time interval data frame
data_hour <- data_thirty_min %>% 
  average_over_time_interval()

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Rename the time column
#!!!!!!!!!!!!!!!!!!!
pass

#Save the data as a csv file
write_csv(data_hour, "data_hour.csv")


#-------------------------------------------
#Creating the a data frame with 1 day interval
#-------------------------------------------

#Finding the system time
t0 <- Sys.time()

#Create the 1 day time interval data frame
data_day <- data_hour %>% 
  average_over_time_interval()

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Save the data as a csv file
write_csv(data_day, "data_day.csv")

test <- read_csv("Data_hourly.csv") %>% distinct(hourly, .keep_all = T) %>% 
  as_tsibble()



test <- data_one_min %>% filter_index("2019-01") %>% 
  test_fcn("5 mins", Time_one_min, Rainfall_mm)
i=0
#Merging all the csv file in one data frame and preprocess the csv files
for(year_tmp in tmp){

  df1 <- data_one_min %>% filter_index(year_tmp)%>% 
    test_fcn("5 mins", Time_one_min, Rainfall_mm)
  
  #Skip the first manually assigned file 
  if (i>0) {
    test <- bind_rows(df1, test)
  }
  i=i+1
}
  
data_day <- test %>% 
  filter_index("2018")
 data_day <-   test_fcn(test, "hour", test_time, sum_rain)
data_day <- test %>% 
  average_over_time_interval("1 day", hourly, testtest)


for (year in tmp){
  tmp2 <- test %>% 
    filter_index(year) %>% 
    average_over_time_interval("1 day", hourly, est %>% 
                                 filter_index(year) %>% 
                                 average_over_time_interval("1 day", hourly, testtest ))
  data_day <- bind_rows(data_day,tmp2)
}
  


test2 <- test %>% splitting_data_in_year_and_average_over_time_interval("1 day", hourly, testtest )

#-------------------------------------------
#Creating the a data frame with 1 week interval
#-------------------------------------------

#Finding the system time
t0 <- Sys.time()

#Create the 1 week time interval data frame
data_week <- data_day %>% 
  average_over_time_interval()

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Save the data as a csv file
write_csv(data_week, "data_week.csv")


#-------------------------------------------
#Creating the a data frame with 1 month interval
#-------------------------------------------

#Finding the system time
t0 <- Sys.time()

#Create the 1 month time interval data frame
data_month <- data_week %>% 
  average_over_time_interval()

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#Save the data as a csv file
write_csv(data_month, "data_month.csv")