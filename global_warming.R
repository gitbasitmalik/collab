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

# Creating two groups based on Year
year_1961to91 <- subset(yearly_global_mean, Year <= 1991)
year_1992to22 <- subset(yearly_global_mean, Year > 1991)

#Creating new Dataset to Merge temperature of Grouped Years from two diffrent dataset
new_dataset <- data.frame(
      '1961-1991'       = year_1961to91$global_Temperature,  
       '1992-2022' = year_1992to22$global_Temperature
      )

#Boxplot
boxplot(
  new_dataset$X1961.1991,
  new_dataset$X1992.2022,
  names = c("1961–1991", "1992–2022"),
  xlab = "Year Range",
  ylab = "Temperature (°C)",
  col = c("lightblue", "orange"),
  main = "Temperature Distribution Across Two Periods"
)

#T test
t_test_result = t.test(new_dataset$X1961-1991, new_dataset$X1992-2022, paired = TRUE)

