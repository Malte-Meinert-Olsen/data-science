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

#----------------------
#----------------------
#Year 2018
#----------------------
#----------------------

#Finding the system time
t0 <- Sys.time()

#Resetting the counter
i=0

#Splitting the data frame into month, enable the PC to work with the data, without 
#crashing do to lost working memory
temp2018 <- temp %>% 
  #Selecting the first month with data measured 
  filter_index("2018-01") %>%
  #Applying the summing function (1440 is minutes pr. day)
  sum_over_rain_data(rainfall_mm, 
                     1440)

#Splitting the data frame into months and preform the rain summation
for(months in year_2018){
  
  df1 <- temp %>% 
    #Spilt the data into all the months in the list of months
    filter_index(months)%>%
    #Apply the summing function (1440 is minutes pr. day)
    sum_over_rain_data(rainfall_mm, 
                       1440)
  
  #Bind all the data frame together in one data frame again
  temp2018 <- bind_rows(df1, 
                        temp2018)
  
  #Add one to the counter
  i=i+1
  
  #Print the counter to follow the process of the code
  print(i)
  
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#----------------------
#----------------------
#Year 2019
#----------------------
#----------------------

#Finding the system time
t0 <- Sys.time()

#Resetting the counter
i=0

#Splitting the data frame into month, enable the PC to work with the data, without 
#crashing do to lost working memory
temp2019 <- temp %>% 
  #Selecting the first month with data measured 
  filter_index("2019-01") %>%
  #Applying the summing function (1440 is minutes pr. day)
  sum_over_rain_data(rainfall_mm, 
                     1440)

#Splitting the data frame into months and preform the rain summation
for(months in year_2019){
  
  df1 <- temp %>% 
    #Spilt the data into all the months in the list of months
    filter_index(months)%>%
    #Apply the summing function (1440 is minutes pr. day)
    sum_over_rain_data(rainfall_mm, 
                       1440)
  
  #Bind all the data frame together in one data frame again
  temp2019 <- bind_rows(df1, 
                        temp2019)
  
  #Add one to the counter
  i=i+1
  
  #Print the counter to follow the process of the code
  print(i)
  
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#----------------------
#----------------------
#Year 2020
#----------------------
#----------------------

#Finding the system time
t0 <- Sys.time()

#Resetting the counter
i=0

#Splitting the data frame into month, enable the PC to work with the data, without 
#crashing do to lost working memory
temp2020 <- temp %>% 
  #Selecting the first month with data measured 
  filter_index("2020-01") %>%
  #Applying the summing function (1440 is minutes pr. day)
  sum_over_rain_data(rainfall_mm, 
                     1440)

#Splitting the data frame into months and preform the rain summation
for(months in year_2020){
  
  df1 <- temp %>% 
    #Spilt the data into all the months in the list of months
    filter_index(months)%>%
    #Apply the summing function (1440 is minutes pr. day)
    sum_over_rain_data(rainfall_mm, 
                       1440)
  
  #Bind all the data frame together in one data frame again
  temp2020 <- bind_rows(df1, 
                        temp2020)
  
  #Add one to the counter
  i=i+1
  
  #Print the counter to follow the process of the code
  print(i)
  
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#----------------------
#----------------------
#Year 2021
#----------------------
#----------------------

#Finding the system time
t0 <- Sys.time()

#Resetting the counter
i=0

#Splitting the data frame into month, enable the PC to work with the data, without 
#crashing do to lost working memory
temp2021 <- temp %>% 
  #Selecting the first month with data measured 
  filter_index("2021-01") %>%
  #Applying the summing function (1440 is minutes pr. day)
  sum_over_rain_data(rainfall_mm, 
                     1440)

#Splitting the data frame into months and preform the rain summation
for(months in year_2021){
  
  df1 <- temp %>% 
    #Spilt the data into all the months in the list of months
    filter_index(months)%>%
    #Apply the summing function (1440 is minutes pr. day)
    sum_over_rain_data(rainfall_mm, 
                       1440)
  
  #Bind all the data frame together in one data frame again
  temp2021 <- bind_rows(df1, 
                        temp2021)
  
  #Add one to the counter
  i=i+1
  
  #Print the counter to follow the process of the code
  print(i)
  
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)

#----------------------
#----------------------
#Year 2022
#----------------------
#----------------------

#Finding the system time
t0 <- Sys.time()

#Resetting the counter
i=0

#Splitting the data frame into month, enable the PC to work with the data, without 
#crashing do to lost working memory
temp2022 <- temp %>% 
  #Selecting the first month with data measured 
  filter_index("2022-01") %>%
  #Applying the summing function (1440 is minutes pr. day)
  sum_over_rain_data(rainfall_mm, 
                     1440)

#Splitting the data frame into months and preform the rain summation
for(months in year_2022){
  
  df1 <- temp %>% 
    #Spilt the data into all the months in the list of months
    filter_index(months)%>%
    #Apply the summing function (1440 is minutes pr. day)
    sum_over_rain_data(rainfall_mm, 
                       1440)
  
  #Bind all the data frame together in one data frame again
  temp2022 <- bind_rows(df1, 
                        temp2022)
  
  #Add one to the counter
  i=i+1
  
  #Print the counter to follow the process of the code
  print(i)
  
}

#Reporting the code run time
end <- Sys.time() - t0
print(end)


data_one_min <- bind_rows(temp2018, 
                          temp2019)

data_one_min <- bind_rows(data_one_min, 
                          temp2020)

data_one_min <- bind_rows(data_one_min, 
                          temp2021)

rm()data_one_min <- bind_rows(data_one_min, 
                              temp2022)



year_2018 <- c("2018-02","2018-03","2018-04",
               "2018-05","2018-06","2018-07",
               "2018-08","2018-09","2018-10",
               "2018-11","2018-12"
)
year_2019 <- c("2019-02","2019-03",
               "2019-04","2019-05","2019-06",
               "2019-07","2019-08","2019-09",
               "2019-10","2019-11","2019-12"
)

year_2020 <- c("2020-02","2020-03",
               "2020-04","2020-05","2020-06",
               "2020-07","2020-08","2020-09",
               "2020-10","2020-11","2020-12"
)
year_2021 <- c("2021-02","2021-03",
               "2021-04","2021-05","2021-06",
               "2021-07","2021-08","2021-09",
               "2021-10","2021-11","2021-12"
)
year_2022 <- c("2022-02","2022-03",
               "2022-04"
)

