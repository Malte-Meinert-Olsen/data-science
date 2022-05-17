#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Setup of different helping tools 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Loading packages
library(tidyverse)
library(lubridate)
library(fpp3)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)

#Making a list of all the csv file names 
list_of_filenames <- list.files(here::here(getwd(), "Data"))





load_and_preprocess_dataframe <- function(path){
  df <- read_delim(here::here("Data",path), delim=";")
  
  # Change "," to "."
  df[,-1] <- lapply(df[,-1], 
                          function(x) 
                            as.numeric(gsub(",", ".",  as.character(x))))
  # Change time format
  df <- df %>% 
    mutate(DATETIME=ymd_hms(DATETIME))
}

#map og bind rows smider alle mine dataframes
rename_columns <- function(df){
  name_of_columns <- read_delim(here::here("Name_converter.csv"), delim=";")
  oldnames <- name_of_columns$Sensors
  newnames <- name_of_columns$`Old Names`
  
  existing <- match(oldnames,names(df))
  
  if(sum(is.na(existing)) > 0){
    print("The following indices did not match any columns:")
    print(which(is.na(existing)))
    print("Corresponding to these columns in oldnames")
    print(oldnames[which(is.na(existing))])
  }
  
  names(df)[na.omit(existing)] <- newnames[which(!is.na(existing))]
}


df %>% 
  rename_columns()
rename_columns(Raw_data)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Cleaning the raw data
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Loading the csv files
Raw_data <- read.csv("Data/Malte_apr2018.csv", 
                     head=TRUE,sep=";",
                     na.strings=c(""," ","NA"))
#-------------------------------------------
#Convert the columns to the correct data types
#-------------------------------------------

#Replace all the commas with dots, to create number with the correct separator, expect for the first coulumn
Raw_data[,-1] <- lapply(Raw_data[,-1], 
                        function(x) 
                          as.numeric(gsub(",", ".",  as.character(x))))

#Convert the time column to the correct year-month-day hour-minute-second type
Raw_data <- Raw_data %>% 
  mutate(DATETIME=ymd_hms(DATETIME))

#-------------------------------------------
#Renaming the column names to a more descriptive name
#-------------------------------------------

#Name structure: chemical_place at WWTP_type of data_unit

#Place at WWTP can be blank if the sensor is not measured at any specific location
#Type of data could be online sensor data, set point e.g. if blank assumed to be a online sensor

#Abbreviations: PT = process tank, SP = set point, AN = Anoxic tank, SS = suspended solids
#Flow is assumed to be flow of waste water, if the flow is of air it is specified
#HT = hydrolysis tank, COD = chemical oxygen demand, TN = total Nitrogen
#DO = dissolved oxygen, T = wastewater temperature, C = degrees Celsius
#SC = secondary clarifies, EU = electricity use



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Removal of data columns which are without importance for this project
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Converting to alternative time frames
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Data frame naming structure: Data_month_year_time interval
#The data is aggregated to the following time intervals:
#1 minutes(online measurement), 5 minutes, 15 minutes, 30 minutes, hourly, daily, weekly, monthly, quarterly
#The aggregated time interval is reporting the mean of the time interval


for(file in list_of_files){
  df <- load_and_preprocess(file)
  # skift navne
  
  # summarize
}

#-------------------------------------------
#1 minute data
#-------------------------------------------
data_apr_2018_1min <- Raw_data


t0 <- Sys.time()
tmp <- Raw_data %>% 
  mutate(time_period = floor_date(DATETIME, "5 mins")) %>% 
  select(-DATETIME) %>% 
  group_by(time_period) %>% 
  summarise(across(where(is.numeric), mean, na.rm=T))
end <- Sys.time() - t0
print(end)
tmp %>% 
  mutate(time_period = floor_date(time_period, "15 mins")) %>% 
  select(-DATETIME) %>% 
  group_by(time_period) %>% 
  summarise(across(where(is.numeric), mean, na.rm=T))

tmp <-Raw_data %>% 
  mutate(time_period = floor_date(DATETIME, "15 mins")) %>% 
  select(time_period)

write_csv(df_name, "lol.csv")
#-------------------------------------------
#5 minutes data
#-------------------------------------------
t0 = Sys.time()
data_apr_2018_5min <- aggregate(data_apr_2018_1min[], 
                   list(fiveMin=cut(data_apr_2018_1min$DATETIME, "5 mins")),
                   mean, na.rm=T) %>% 
  select(-DATETIME) %>% 
  mutate(fiveMin=ymd_hms(as.character(fiveMin)))
end <- Sys.time() - t0
print(end)


test_5min_Malte <- aggregate(data_apr_2018_1min[], 
                                list(fiveMin=cut(data_apr_2018_1min$DATETIME, "1 day")),
                                mean, na.rm=T) %>% 
  mutate(fiveMin=ymd_hms(as.character(fiveMin)))

#-------------------------------------------
#15 minutes data
#-------------------------------------------
data_apr_2018_15min <- aggregate(data_apr_2018_5min[], 
                                  list(min_15=cut(data_apr_2018_5min$fiveMin, "15 mins")),
                                  mean, na.rm=TRUE) %>% 
  select(-fiveMin) %>% 
  mutate(min_15=ymd_hms(as.character(min_15)))
  
#-------------------------------------------
#30 minutes data
#-------------------------------------------
data_apr_2018_30min <- aggregate(data_apr_2018_15min[], 
                                 list(min_30=cut(data_apr_2018_15min$min_15, "30 mins")),
                                 mean, na.rm=TRUE) %>% 
  select(-min_15)%>% 
  mutate(min_30=ymd_hms(as.character(min_30)))

#-------------------------------------------
#1 hour data
#-------------------------------------------
data_apr_2018_hour <- aggregate(data_apr_2018_30min[], 
                                 list(hourly=cut(data_apr_2018_30min$min_30, "1 hour")),
                                 mean, na.rm=TRUE) %>% 
  select(-min_30)%>% 
  mutate(hourly=ymd_hms(as.character(hourly)))

#-------------------------------------------
#Merging of all the hourly data frames 
#-------------------------------------------







#-------------------------------------------
#1 day data
#-------------------------------------------
data_apr_2018_day <- aggregate(data_apr_2018_hour[], 
                                list(daily=cut(data_apr_2018_hour$hourly, "1 day")),
                                mean, na.rm=TRUE) %>% 
  select(-hourly)%>% 
  mutate(daily=ymd(as.character(daily)))

#-------------------------------------------
#1 week data
#-------------------------------------------
data_apr_2018_week <- aggregate(data_apr_2018_day[], 
                               list(weekly=cut(data_apr_2018_day$daily, "1 week")),
                               mean, na.rm=TRUE) %>% 
  select(-daily)%>% 
  mutate(weekly=yearweek(as.character(weekly)))

#-------------------------------------------
#1 month data
#-------------------------------------------



#-------------------------------------------
#1 quarter
#-------------------------------------------











# #Naming is done after following scheme:
# #1. chemical 2. Place at the WWTP 3. type of measurement e.g. SP, 
# #if nothing is written it is a sensor 4. unit 
# Raw_data <- Raw_data %>% 
#   rename(
#     Date_time_min         = DATETIME,
#     Nedbør_mm             = EGA.RE.BL750.M_ANT_MM,
#     NH4N_til_AN_XX        = EGA.AM.BL610.Z_TILLOB_NH4N,
#     Ammonium_PT1_xx       = EGA.AM.BL714.M_NH4N,
#     Ammonium_PT1_SP_xx    = EGA.AM.BL714.Z_NH4N_SP,
#     Ammonium_PT2_xx       = EGA.AM.BL724.M_NH4N,
#     Ammonium_PT2_SP_xx    = EGA.AM.BL724.Z_NH4N_SP,
#     Ammonium_PT3_xx       = EGA.AM.BL734.M_NH4N,
#     Ammonium_PT3_SP_xx    = EGA.AM.BL734.Z_NH4N_SP,
#     Ammonium_PT4_xx       = EGA.AM.BL744.M_NH4N,
#   )
# 
# 
# 
# name_converter <- read_excel("Name_converter.xlsx") %>% 
#   select('Old Names') %>% 
#   as.list()
# 
# name_list <- c(name_converter[])
# 
# 
# test_data <- Raw_data %>% 
#   select(DATETIME, EGA.RE.BL750.M_ANT_MM)
# names(Raw_data)[] <- name_converter[]


# na_in_data2 <- sapply(data_apr_2018_5min, function(x) {sum(is.na(x))}) %>% as.numeric()
# 
# 
# column_names <- colnames(data_apr_2018_1min)
# 
# df <- data.frame(column_names, na_in_data)
# df %>% filter(na_in_data<1000 & na_in_data>1) %>% select(na_in_data) %>% count(na_in_data==62)
# tmp1 <-data_apr_2018_1min %>% select(DATETIME, EGA.AM.BL610.Z_TILLOB_NH4N) %>%
#   filter(is.na(EGA.AM.BL610.Z_TILLOB_NH4N))
# 
# 
# tmp2 <- data_apr_2018_1min %>% select(DATETIME, EGA.AM.BL714.M_NH4N) %>%
#   filter(is.na(EGA.AM.BL714.M_NH4N))

