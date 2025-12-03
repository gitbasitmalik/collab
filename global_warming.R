# loading and arranging the dataset
library(tidyverse)

# Importing Dataset
global_Warming_dataset <- read_csv('long_format_annual_surface_temp.csv')

# Removing F character from years
global_Warming_dataset$Year <- gsub("F", "", global_Warming_dataset$Year)

# Converting Year to numeric type
global_Warming_dataset$Year <- as.numeric(global_Warming_dataset$Year)

# Grouping the dataset based upon the years and mean temperature per year
yearly_global_mean <- global_Warming_dataset %>%
  group_by(Year) %>%
  summarise(global_Temperature = round(mean(Temperature, na.rm = TRUE), 4))

# Creating three groups based on Year
year1961to81 <- subset(yearly_global_mean, Year < 1981)
year1981to01 <- subset(yearly_global_mean, Year >= 1981 & Year < 2001)
year2001to22 <- subset(yearly_global_mean, Year >= 2001)
#calculte mean temperature for each period
mean_1961to81 <- mean(year1961to81$global_Temperature, na.rm = TRUE)
mean_1981to01 <- mean(year1981to01$global_Temperature, na.rm = TRUE)
mean_2001to22 <- mean(year2001to22$global_Temperature, na.rm = TRUE)
