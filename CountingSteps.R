##### Load libraries and initialise data ------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(zoo)
library(grid)
library(gridBase)

# Load in step data
data <- read.csv("StepData.csv")

names(data)[1] <- "Year"
data <- mutate(data, Date = as.Date(paste(Month, Day, Year, sep = "/"), "%m/%d/%Y"))





##### Plot distribution of daily step counts --------------------------------------------------------------

# Function to generate histogram
hPlot <- function(){
  histColours <-  c(colorRampPalette(c("red4", "red"), 0.5)(8), 
                    colorRampPalette(c("green", "green4"))(8), rep("green4", 16))
  hist(data$Steps, breaks = seq(0, 40000, by = 1000), xlab = "", ylab = "Days",
       col = histColours, main = "", ylim = c(0, 150))
  abline(v = 8000, col = "blue", lty = 3)
  box()}





##### Plot moving average and daily counts ----------------------------------------------------------------

# Function to generate time series plot
tPlot <- function(){
  plot(data$Date, data$Steps, type = "l", ylim = c(0, 40000), ylab = "Daily Step Count",
       xlab = "", xaxt = "n")
  axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "1 mon"), format = "%m/%Y")
  lines(data$Date, rollmean(data$Steps, 7, fill = list(NA, NULL, NA)), col = "red", lwd = 3)
  abline(h = 8000, col = "blue", lty = 3)}





##### Combine plots into a single canvas ------------------------------------------------------------------

# Prepare graphics device
tiff(filename = "StepData.tif", width = 3304, height = 2272, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(800, 1200)
pushViewport(viewport(layout = gly))

# CN non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 400:800, layout.pos.col = 1:1200))
par(fig = gridFIG(), mar = c(1, 1, 0.1, 0.1), cex.axis = 0.45, cex.lab = 0.5, tcl = -0.2, mgp = c(0.3, 0, 0))
par(new = TRUE)
tPlot()
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 1:400, layout.pos.col = 1:800))
par(fig = gridFIG(), mar = c(1, 1, 0.1, 0.1), cex.axis = 0.45, cex.lab = 0.5, tcl = -0.2, mgp = c(0.3, 0, 0))
par(new = TRUE)
hPlot()
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

