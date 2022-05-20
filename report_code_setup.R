#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Load the data into R
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Loading packages
library(tidyverse) 
library(lubridate)
library(fpp3)
library(zoo)
#Making the lubridate package run faster
options(lubridate.fasttime = TRUE)


data_one_min <- read_csv("data/data_one_min.csv") %>% 
  mutate(time_one_min=ymd_hms(time_one_min)) %>% 
  as_tsibble()

data_five_min <- read_csv("data/data_five_min.csv") %>% 
  mutate(time_five_min=ymd_hms(time_five_min)) %>% 
  as_tsibble()

data_fifteen_min <- read_csv("data/data_fifteen_min.csv") %>% 
  mutate(time_fifteen_min=ymd_hms(time_fifteen_min)) %>% 
  as_tsibble()

data_thirty_min <- read_csv("data/data_thirty_min.csv") %>% 
  mutate(time_thirty_min=ymd_hms(time_thirty_min)) %>% 
  as_tsibble()

data_hour <- read_csv("data/data_hour.csv") %>% 
  mutate(time_hour=ymd_hms(time_hour)) %>% 
  as_tsibble()

