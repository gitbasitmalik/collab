#loading and arranging the dataset
library(tidyverse)
global_Warming_dataset <- read_csv('long_format_annual_surface_temp.csv',) #Importing Dataset
global_Warming_dataset$Year <- sapply(global_Warming_dataset$Year, function(x) gsub("F", "",  x)) #Removing F character from years
global_Warming_dataset$Year <- as.numeric(global_Warming_dataset$Year) #Converting Year to numeric type

#Grouping the dataset based upon the years and mean of the temperature in whole world in each year
yearly_global_mean <- global_Warming_dataset %>% #Creating a new subset of Main dataset
  +     group_by(Year) %>% #and then grouping the data by Year
  +     summarise(global_Temperature = mean(Temperature, na.rm = TRUE),) %>% #and then calculating the mean of the global temperature for each year
  +     mutate(global_Temperature = round(global_Temperature, 4)) # and then rounding off the result upto 4 decimal points


group <- subset(yearly_global_mean$global_Temperature, yearly_global_mean$Year < "1981")
group <- subset(yearly_global_mean$global_Temperature, yearly_global_mean$Year > "1981" & yearly_global_mean$Year < "2001")
group <- subset(yearly_global_mean$global_Temperature, yearly_global_mean$Year > "2001" )




