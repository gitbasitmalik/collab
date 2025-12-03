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

#calculte mean temperature for each period
mean_1961to91 <- mean(year_1961to91$global_Temperature, na.rm = TRUE)
mean_1992to22 <- mean(year_1992to22$global_Temperature, na.rm = TRUE)

new_dataset <- data.frame(
      "Year Range"       = c("1961–1991", "1992–2022"),  
       "Mean Temperature" = c(0.1033258, 0.8921226)
   
      )

barplot(new_dataset$Mean.Temperature,
names.arg = new_dataset$Year.Range,
col = "lightblue",
border = "black",
main = "Annual Mean Temperature (1961–2022)",
xlab = "Year",
ylab = "Mean Temperature (°C)",
ylim = c(0,1),
las = 2)

