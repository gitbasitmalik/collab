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

# Histogram for Average Global Temperature Change from period 1961 - 2022
h <- hist(yearly_global_mean$global_Temperature, breaks = 20, plot = FALSE)

plot(h,
     freq = TRUE,
     col = "lightblue",
     border = "black",
     main = "Global Average Temperature Change for period 1961–2022",
     xlab = "Average Temperature Change (°C)",
     ylab = "Number of Years",
     xlim = c(h$breaks[1], tail(h$breaks, 1)),
     xaxs = "i", xaxt = "n", las = 1,
     cex.main = 1.4, cex.lab = 1.3)

axis(1, at = pretty(c(h$breaks[1], tail(h$breaks, 1)), n = 8), las = 1)

lines(smooth.spline(h$mids, h$counts, spar = 0.6),
      col = "darkred", lwd = 5)

# Small legend for the histogram
legend("topright",
       legend = c("Histogram", "Smoothed curve"),
       col    = c("lightblue", "darkred"),
       lwd    = c(10, 5),         
       lty    = c(1, 1),
       seg.len = 1,
       cex    = 0.85,               
       pt.cex = 1,
       box.lwd = 0.8,
       box.col = "gray70",
       bg     = "white",
       inset  = 0.01)

#Boxplot for two periods 
boxplot(
  new_dataset$X1961.1991,
  new_dataset$X1992.2022,
  names = c("1961–1991", "1992–2022"),
  xlab = "Year Range",
  ylab = "Temperature (°C)",
  col = c("lightblue", "orange"),
  main = "Temperature Distribution Across Two Periods"
)

#Wilcoxon test
wilcox_test_result <- wilcox.test(
  new_dataset$X1961.1991,
  new_dataset$X1992.2022,
  paired = FALSE
)
 print(wilcox_test_result)
