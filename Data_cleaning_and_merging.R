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

i=0


#Making a list of all the csv file names that should be used
list_of_filenames <- list.files( here::here(getwd(), "Data"))



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Defining the functions to be used later 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------
#Loading the data and data preprocessing funciton
#-------------------------------------------

#Defining the function
load_and_preprocess_dataframe <- function(path){
  #Loading the csv filt in to the function defined data frame
  #using the read_delim to specify that the file is seperated by ;
  df <- read_delim(here::here("Data",path), 
                   delim=";",
                   col_types = cols(.default = "c"))
  
  # Change "," to "." and converting all columns to numeric, expect in the time column
  df[,-1] <- lapply(df[,-1], 
                    function(x) 
                      as.numeric(gsub(",", ".",  as.character(x))))
 
   # Change time format to the year-month-day hour-minute-second formate
  df <- df %>% 
    mutate(DATETIME=ymd_hms(DATETIME))
}


#-------------------------------------------
#Averageting over time intervals function
#-------------------------------------------

#Defining the funtion, requires data frame, column names of the time column from the data frame
#and which name the column shall have in the new data frame, and the time interval which to average over
average_over_time_interval <- function(df, time_interval){
  df %>% 
    #Creates a new time column with the times in their appropiate time intervals
    mutate(Date_time= 
             floor_date(DATETIME, 
                        time_interval))
  df %>% 
    #Removing the old time column
    select(-DATETIME) 
  df %>% 
    #Grouping by the time intervals
    group_by(Date_time) %>% 
    #Averagting over the time intervals in all numeric columns
    #the na.rm=T makes the averagting robust agianst sigle missing values in the averageting
    summarise(across(where(is.numeric), mean, na.rm=T))  
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Merging all the data to one data frame 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

t0 <- Sys.time()
#Defining the first month int he list of month manually 
Data_1min <- load_and_preprocess_dataframe("Malte_apr2018.csv")
i=0
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_filenames){
  df1 <- load_and_preprocess_dataframe(file)
  
  if (i>0) {
    Data_1min <- bind_rows(df1, Data_1min)
  }
  i=i+1
}
end <- Sys.time() - t0
print(end)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Renaming the data columns 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Name structure: chemical_place at WWTP_type of data_unit

#Place at WWTP can be blank if the sensor is not measured at any specific location
#Type of data could be online sensor data, set point e.g. if blank assumed to be a online sensor

#Abbreviations: PT = process tank, SP = set point, AN = Anoxic tank, SS = suspended solids
#Flow is assumed to be flow of waste water, if the flow is of air it is specified
#HT = hydrolysis tank, COD = chemical oxygen demand, TN = total Nitrogen
#DO = dissolved oxygen, T = wastewater temperature, C = degrees Celsius
#SC = secondary clarifies, EU = electricity use


#Load the new names from a separate csv file
name_of_columns <- read_delim(here::here("Name_converter.csv"), delim=";")
#Defining the old names as the column "Sensors"
oldnames <- name_of_columns$Sensors
#Defining the new names as the column NEW !!!!!! NAMES 
newnames <- name_of_columns$`Old Names`

#Matching the column names in the data frame with the sensors names in the csv file,
#this creates a new vector of the position of all the matches, and returns NA
#if a column is not matched with the sensors names in the csv file
existing <- match(oldnames,names(Data_1min))

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
  
#Overwrite the old column names with the new names specificed in the csv file
names(Data_1min)[na.omit(existing)] <- newnames[which(!is.na(existing))]

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Finding sensors with no measurements
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  
#Finding all the column where all the values are NA
Sensors_with_no_measurements <- Data_1min %>% 
    select(
    where(
        ~all(is.na(.x))
      )
    )  
  
#Saving this columns as a csv file
write_csv(Sensors_with_no_measurements, "Sensors_with_no_measurements.csv")

#Removing the columns where there are only NA values
Data_1min <- Data_1min %>% 
  select(
    where(
      ~!all(is.na(.x))
    )
  )
#Saving the data in a csv file
write_csv(Data_1min, "Data_1min.csv")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Splitting the data in other time intervals
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 
# #Creating the a data frame with 5 minutes interval


t0 <- Sys.time()
i=0
Data_five_min <- load_and_preprocess_dataframe("Malte_apr2018.csv")

Data_five_min <- aggregate(Data_five_min[], 
                          list(fiveMin=cut(Data_five_min$DATETIME, "5 mins")),
                          mean, na.rm=T) %>% 
  select(-DATETIME) %>% 
  mutate(fiveMin=ymd_hms(as.character(fiveMin)))
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_filenames){
  df1 <- load_and_preprocess_dataframe(file)
  
  df1 <- aggregate(df1[], 
                   list(fiveMin=cut(df1$DATETIME, "5 mins")),
                   mean, na.rm=T) %>% 
    select(-DATETIME) %>% 
    mutate(fiveMin=ymd_hms(as.character(fiveMin)))
  
  if (i>0) {
    Data_five_min <- bind_rows(df1, Data_five_min)
  }
  i=i+1
  
}

end <- Sys.time() - t0
print(end)

write_csv(Data_five_min, "Data_five_min.csv")



# #Creating the a data frame with 15 minutes interval
t0 <- Sys.time()
i=0
Data_fifteen_min <- load_and_preprocess_dataframe("Malte_apr2018.csv")

Data_fifteen_min <- aggregate(Data_fifteen_min[], 
                           list(fifteenMin=cut(Data_fifteen_min$DATETIME, 
                                               "15 mins")),
                           mean, na.rm=T) %>% 
  select(-DATETIME) %>% 
  mutate(fifteenMin=ymd_hms(as.character(fifteenMin)))
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_filenames){
  df1 <- load_and_preprocess_dataframe(file)
  
  df1 <- aggregate(df1[], 
                   list(fifteenMin=cut(df1$DATETIME, "15 mins")),
                   mean, na.rm=T) %>% 
    select(-DATETIME) %>% 
    mutate(fifteenMin=ymd_hms(as.character(fifteenMin)))
  
  if (i>0) {
    Data_fifteen_min <- bind_rows(df1, Data_fifteen_min)
  }
  i=i+1
  
}

end <- Sys.time() - t0
print(end)

write_csv(Data_fifteen_min, "Data_fifteen_min.csv")



# #Creating the a data frame with 30 minutes interval
t0 <- Sys.time()
i=0
Data_thirty_min <- load_and_preprocess_dataframe("Malte_apr2018.csv")

Data_thirty_min <- aggregate(Data_thirty_min[], 
                              list(thirtyMin=cut(Data_thirty_min$DATETIME, 
                                                  "30 mins")),
                              mean, na.rm=T) %>% 
  select(-DATETIME) %>% 
  mutate(thirtyMin=ymd_hms(as.character(thirtyMin)))
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_filenames){
  df1 <- load_and_preprocess_dataframe(file)
  
  df1 <- aggregate(df1[], 
                   list(thirtyMin=cut(df1$DATETIME, "30 mins")),
                   mean, na.rm=T) %>% 
    select(-DATETIME) %>% 
    mutate(thirtyMin=ymd_hms(as.character(thirtyMin)))
  
  if (i>0) {
    Data_thirty_min <- bind_rows(df1, Data_thirty_min)
  }
  i=i+1
  
}

end <- Sys.time() - t0
print(end)

write_csv(Data_thirty_min, "Data_thirty_min.csv")



# #Creating the a data frame with 1 hour interval
t0 <- Sys.time()
i=0
Data_hourly <- load_and_preprocess_dataframe("Malte_apr2018.csv")

Data_hourly <- aggregate(Data_hourly[], 
                             list(hourly=cut(Data_hourly$DATETIME, 
                                                "1 hour")),
                             mean, na.rm=T) %>% 
  select(-DATETIME) %>% 
  mutate(hourly=ymd_hms(as.character(hourly)))
#Merging all the csv file in one data frame and preprocess the csv files
for(file in list_of_filenames){
  df1 <- load_and_preprocess_dataframe(file)
  
  df1 <- aggregate(df1[], 
                   list(hourly=cut(df1$DATETIME, "1 hour")),
                   mean, na.rm=T) %>% 
    select(-DATETIME) %>% 
    mutate(hourly=ymd_hms(as.character(hourly)))
  
  if (i>0) {
    Data_hourly <- bind_rows(df1, Data_hourly)
  }
  i=i+1
  
}

end <- Sys.time() - t0
print(end)

write_csv(Data_hourly, "Data_hourly.csv")


