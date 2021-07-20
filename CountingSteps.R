##### Load libraries and initialise data ------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(zoo)

# Load in step data
data <- read.csv("StepData.csv")

names(data)[1] <- "Year"
data <- mutate(data, Date = as.Date(paste(Month, Day, Year, sep = "/"), "%m/%d/%Y"))





##### Plot distribution of daily step counts --------------------------------------------------------------

hist(data$Steps, breaks = seq(0, 40000, by = 1000), xlab = "Daily Step Count", ylab = "Days",
     col = c(colorRampPalette(c("red4", "red"), 0.5)(8), colorRampPalette(c("green", "green4"))(32)),
     main = "")
box()





##### Plot moving average and daily counts ----------------------------------------------------------------

plot(data$Date, data$Steps, type = "l", ylim = c(0, 40000), ylab = "Daily Step Count",
     xlab = "", xaxt = "n")
axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "1 mon"), format = "%m/%Y")
lines(data$Date, rollmean(data$Steps, 7, fill = list(NA, NULL, NA)), col = "red", lwd = 3)
abline(h = 8000, col = "blue", lty = 3)

