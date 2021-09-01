##### Load libraries and initialise data ------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(zoo)
library(grid)
library(gridBase)

# Load in step data
data <- read.csv("Data/StepData.csv")

names(data)[1] <- "Year"
data <- mutate(data, Date = as.Date(paste(Month, Day, Year, sep = "/"), "%m/%d/%Y"))





##### Plot distribution of daily step counts --------------------------------------------------------------

# Function to generate histogram
hPlot <- function(){
  par(lwd = 0.5)
  histColours <-  c(colorRampPalette(c("red4", "red"), 0.5)(8), 
                    colorRampPalette(c("green", "green4"))(8), rep("green4", 16))
  hist(data$Steps, breaks = seq(0, 40000, by = 1000), xlab = "", xaxt = "n",
       ylab = "", yaxt = "n", col = histColours, main = "", ylim = c(0, 150))
  axis(side = 1, at = seq(0, 40000, by = 10000), labels = c("0", "10k", "20k", "30k", "40k"))
  axis(side = 2, at = seq(0, 150, by = 50), labels = TRUE, mgp = c(0, 0.17, 0))
  abline(v = 8000, col = "blue", lty = 3, lwd = 0.5)
  box(lwd = 1)}





##### Plot moving average and daily counts ----------------------------------------------------------------

# Function to generate time series plot
tPlot <- function(){
  plot(data$Date, data$Steps, type = "l", ylim = c(0, 40000), xlab = "", xaxt = "n",
       ylab = "", yaxt = "n", lwd = 0.5)
  axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "1 mon"), format = "%m/%Y")
  axis(side = 2, at = seq(0, 40000, by = 10000), labels = c("0", "10k", "20k", "30k", "40k"),
       mgp = c(0, 0.17, 0))
  lines(data$Date, rollmean(data$Steps, 7, fill = list(NA, NULL, NA)), col = "red", lwd = 1)
  abline(h = 8000, col = "blue", lty = 3, lwd = 0.5)}





##### Get overall step count stats ------------------------------------------------------------------------

# Get longest goal streak
goalStreak <- rle(data$Steps >= 8000)
goalStreak <- max(goalStreak$lengths[goalStreak$values == TRUE])

# Calculate total steps, total distance, and average daily steps
# Also calculate rate that goal is met; include longest goal streak
stepStats <- c(sum(data$Steps),
               round(sum(data$Steps)*0.427/1000, 0),
               round(mean(data$Steps), 2),
               round(length(data$Steps[data$Steps >= 8000])/nrow(data)*100, 2),
               goalStreak)





##### Combine plots into a single canvas ------------------------------------------------------------------

# Prepare graphics device
tiff(filename = "StepData.tif", width = 3304, height = 2272, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(800, 1200)
pushViewport(viewport(layout = gly))

# Plot step count time series
pushViewport(vp = viewport(layout.pos.row = 400:800, layout.pos.col = 1:1200))
par(fig = gridFIG(), mar = c(1, 1, 0.4, 0.4), cex.axis = 0.42, tcl = -0.2, mgp = c(0, -0.2, 0))
par(new = TRUE)
tPlot()
popViewport()

# Plot step count histogram
pushViewport(vp = viewport(layout.pos.row = 1:400, layout.pos.col = 1:700))
par(fig = gridFIG(), mar = c(1, 1, 0.4, 0.4), cex.axis = 0.42, tcl = -0.2, mgp = c(0, -0.2, 0))
par(new = TRUE)
hPlot()
popViewport()

# Create figure labels
grid.text(label = c("Daily Step Count", "(Distribution)", "Daily Step Count", "(Time Series)"),
          x = rep(0.061, 4), y = c(0.947, 0.930, 0.447, 0.430), hjust = 0, gp = gpar(cex = 0.3))
popViewport()

# Create legend (top)
grid.text(label = c("Above Goal", "Below Goal"),
          x = rep(0.486, 2), y = c(0.947, 0.930),
          hjust = 0, gp = gpar(cex = 0.3))
grid.rect(x = rep(0.476, 2), y = c(0.947, 0.930),
          width = rep(0.008, 2), height = rep(0.010, 2),
          gp = gpar(col = c("green", "red"), fill = c("green", "red")))

# Create legend (bottom)
grid.text(label = c("Daily Count", "7-Day Average", "Daily Goal"),
          x = rep(0.890, 3), y = c(0.447, 0.430, 0.413),
          hjust = 0, gp = gpar(cex = 0.3))
grid.segments(x0 = rep(0.860, 3), y0 = c(0.447, 0.430, 0.413),
              x1 = rep(0.880, 3), y1 = c(0.447, 0.430, 0.413),
              gp = gpar(col = c("black", "red", "blue"), lty = c(1, 1, 3), lwd = 0.5))

# summary stats
grid.text(label = c(paste0(stepStats[1]), "Total Steps",
                    paste0(stepStats[2], " km"), "Total Distance",
                    paste0(stepStats[3]), "Average Daily Steps",
                    paste0(stepStats[4], "%"), "Goal Success Rate",
                    paste0(stepStats[5], " days"), "Longest Goal Streak"),
          x = c(0.610, 0.700, 0.610, 0.700, 0.610, 0.700, 0.610, 0.700, 0.610, 0.700),
          y = c(rep(0.95, 2), rep(0.86, 2), rep(0.77, 2), rep(0.68, 2), rep(0.59, 2)),
          hjust = 0, gp = gpar(cex = 0.5, col = rep(c("blue", "black"), 5)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

