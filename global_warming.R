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

#min and max our data
exact_min <- min(c(new_dataset$X1961.1991, new_dataset$X1992.2022), na.rm = TRUE)
exact_max <- max(c(new_dataset$X1961.1991, new_dataset$X1992.2022), na.rm = TRUE)
#Create cleaner tick marks 
ticks <- seq(from = floor(exact_min*10)/10,   # round down to nearest 0.1
             to   = ceiling(exact_max*10)/10, # round up to nearest 0.1
             by = 0.1)

# plotting
boxplot(new_dataset$X1961.1991,
        new_dataset$X1992.2022,
        names = c("1961–1991", "1992–2022"),
        xlab = "Period",
        ylab = "Average Change in Temperature (°C)",
        main = "Distribution of Changes in Global Average Temperature\n(1961–1991 vs 1992–2022)",
        col = c("lightblue", "orange"),
        border = "black",
        ylim = c(exact_min, exact_max),
        outline = TRUE,
        pch = 19,
        cex = 0.8,
        yaxt = "n",
        las = 1)

# 4. Add the custom axis with ALL values
axis(side = 2,                     # left side
     at = ticks,                   # every 0.1 (or 0.05, 0.01)
     labels = sprintf("%.2f", ticks),  # show 2 decimal places
     las = 1,                      # horizontal labels
     cex.axis = 0.9,               # size of tick labels
     tck = -0.02)

#Wilcoxon test
wilcox_test_result <- wilcox.test(
  new_dataset$X1961.1991,
  new_dataset$X1992.2022,
  paired = FALSE
)
 print(wilcox_test_result)

 