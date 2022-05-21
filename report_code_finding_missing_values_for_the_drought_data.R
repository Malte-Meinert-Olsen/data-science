#Replacing the missing drought index values
# 
# missing_drougth_data <- data_hour %>% filter(is.na(drought)) %>% 
#   as_tibble() %>% 
#   select(drought,time_hour) %>% 
#   mutate(time_day=as.Date(time_hour)) %>% 
#   select(-time_hour) %>% 
#   distinct(time_day)
# 
# 


#Loading the three csv files containing the effluent data
temp1 <- read_delim(here::here("more_drought_data/aarhus-kommune-april-2018.csv"), 
                    delim=";",
                    col_types = cols(.default = "c"))
temp2 <- read_delim(here::here("more_drought_data/aarhus-kommune-juli-2021.csv"), 
                    delim=";",
                    col_types = cols(.default = "c"))

temp3 <- read_delim(here::here("more_drought_data/aarhus-kommune-oktober-2020.csv"), 
                    delim=";",
                    col_types = cols(.default = "c"))

#Creating the data frame containing the three csv file
extra_data_drought <- bind_rows(temp1,temp2,temp3) %>%
  as_tibble()


extra_data_drought <- extra_data_drought %>% 
  #Renaming the columns
  rename(day=DateTime, 
         drought_new="Tørkeindex") %>% 
  #Converting the time column to the correct format
  mutate(day=as.Date(day)) %>% 
  #Replacing comma with dot and converting to numeric
  mutate(drought_new= as.numeric(gsub(",", ".",  as.character(drought_new)))) %>% 
  as_tsibble()

#Making a replace function for the missing drought data
finding_missing_drought_data <- function(df, time_column_name){
  df <- df %>% 
  mutate(day=as.Date({{time_column_name}})) %>% 
  left_join(extra_data_drought) %>%  
  mutate(drought=if_else(is.na(drought), 
                         drought_new, 
                         drought)) %>% 
  select(-drought_new,-day)

}

#data_one_min <- finding_missing_drought_data(data_one_min, time_one_min)

data_five_min <- finding_missing_drought_data(data_five_min, time_five_min)

data_fifteen_min <- finding_missing_drought_data(data_fifteen_min, time_fifteen_min)

data_thirty_min <- finding_missing_drought_data(data_thirty_min, time_thirty_min)

data_hour <- finding_missing_drought_data(data_hour, time_hour)




