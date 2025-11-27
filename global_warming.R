#R Code will be written here
library(tidyverse)
global_Warming_dataset <- read_csv('long_format_annual_surface_temp.csv',) #Importing Dataset
global_Warming_dataset$Year <- sapply(global_Warming_dataset$Year, function(x) gsub("F", "",  x)) #Removing F character from years
