#Setup 

sources("report_code_setup.R")


#Plotting flow and ammonium for each year of operation ammonium being red and see trough

#Plot coefficient

coeff=400

#Plot for all years
data_hour %>% 
  ggplot(aes(x=time_hour)) +
  geom_line( aes(y=flow_effluent_m3_h)) + 
  geom_line( aes(y=ammonium_effluent_mg_L * coeff), 
             col="Red", 
             alpha=0.5) + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Flow in the effluent [m3/h]",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Ammonium [mg/L]")
  )


#Plot for 2018
data_hour %>% 
  filter_index("2018") %>% 
  ggplot(aes(x=time_hour)) +
  geom_line( aes(y=flow_effluent_m3_h)) + 
  geom_line( aes(y=ammonium_effluent_mg_L * coeff), 
             col="Red", 
             alpha=0.5) + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Flow in the effluent [m3/h]",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Ammonium [mg/L]")
  )

#Plot for 2019
data_hour %>% 
  filter_index("2019") %>% 
  ggplot(aes(x=time_hour)) +
  geom_line( aes(y=flow_effluent_m3_h)) + 
  geom_line( aes(y=ammonium_effluent_mg_L * coeff), 
             col="Red", 
             alpha=0.5) + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Flow in the effluent [m3/h]",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Ammonium [mg/L]")
  )

#Plot for 2020
data_hour %>% 
  filter_index("2020") %>% 
  ggplot(aes(x=time_hour)) +
  geom_line( aes(y=flow_effluent_m3_h)) + 
  geom_line( aes(y=ammonium_effluent_mg_L * coeff), 
             col="Red", 
             alpha=0.5) + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Flow in the effluent [m3/h]",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Ammonium [mg/L]")
  )

#Plot for 2021
data_hour %>% 
  filter_index("2021") %>% 
  ggplot(aes(x=time_hour)) +
  geom_line( aes(y=flow_effluent_m3_h)) + 
  geom_line( aes(y=ammonium_effluent_mg_L * coeff), 
             col="Red", 
             alpha=0.5) + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Flow in the effluent [m3/h]",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Ammonium [mg/L]")
  )









