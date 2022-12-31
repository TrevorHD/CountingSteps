##### Load libraries and initialise data ------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(zoo)
library(grid)
library(gridBase)

# Load in step data
data <- read.csv("Data/StepData.csv")

# Format date objects
names(data)[1] <- "Year"
data <- mutate(data, Date = as.Date(paste(Month, Day, Year, sep = "/"), "%m/%d/%Y"))





##### Plot distribution of daily step counts --------------------------------------------------------------

# Function to generate histogram
hPlot <- function(){
  par(lwd = 0.5)
  histColours <-  c(colorRampPalette(c("red4", "red"), 0.5)(8), 
                    colorRampPalette(c("green", "green4"))(8), rep("green4", 24))
  h <- hist(data$Steps, breaks = seq(0, 40000, by = 1000), plot = FALSE)
  h$density <- h$counts/sum(h$counts)*100
  plot(h, freq = FALSE, xlab = "", xaxt = "n",
       ylab = "", yaxt = "n", col = histColours, main = "", ylim = c(0, 30))
  axis(side = 1, at = seq(0, 40000, by = 5000), labels = c("0", "", "10k", "", "20k", "", "30k", "", "40k"))
  axis(side = 2, at = seq(0, 30, by = 5), labels = c("0%", "", "10%", "", "20%", "", "30%"),
       mgp = c(0, 0.17, 0))
  abline(v = 8000, col = "blue", lty = 3, lwd = 0.5)
  box(lwd = 1)}





##### Plot moving average and daily counts ----------------------------------------------------------------

# Function to generate time series plot
tPlot <- function(){
  plot(data$Date, data$Steps, type = "l", ylim = c(0, 40000), xlab = "", xaxt = "n",
       ylab = "", yaxt = "n", lwd = 0.5)
  axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "3 mon"), format = "%m/%y")
  axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "1 mon"), labels = FALSE, tcl = -0.11)
  axis(side = 2, at = seq(0, 40000, by = 5000), labels = c("0", "", "10k", "", "20k", "", "30k", "", "40k"),
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
               round(sum(data$Steps)*0.427/1000, 1),
               round(mean(data$Steps), 0),
               round(length(data$Steps[data$Steps >= 8000])/nrow(data)*100, 2),
               goalStreak)

# Get total and mean step count, goal rate over last 30 days
data30 <- data[(nrow(data)-30):nrow(data), ]
step30Stats <- c(sum(data30$Steps),
                 round(mean(data30$Steps), 1),
                 round(length(data30$Steps[data30$Steps >= 8000])/nrow(data30)*100, 2))





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
pushViewport(vp = viewport(layout.pos.row = 370:790, layout.pos.col = 1:1175))
par(fig = gridFIG(), mar = c(0.8, 1, 0.4, 0.4), cex.axis = 0.42, tcl = -0.2, mgp = c(0, -0.2, 0))
par(new = TRUE)
tPlot()
popViewport()

# Plot step count histogram
pushViewport(vp = viewport(layout.pos.row = 15:350, layout.pos.col = 1:700))
par(fig = gridFIG(), mar = c(0.3, 1, 0.4, 0.4), cex.axis = 0.42, tcl = -0.2, mgp = c(0, -0.2, 0))
par(new = TRUE)
hPlot()
popViewport()

# Create figure labels
grid.text(label = c("Daily Step Count", "(Distribution)", "Daily Step Count", "(Time Series)"),
          x = c(0.549, 0.549, 0.061, 0.061), y = c(0.932, 0.915, 0.488, 0.471),
          hjust = c(1, 1, 0, 0), gp = gpar(cex = 0.3))
popViewport()

# Create legend (top)
grid.text(label = c("Above Goal", "Below Goal"),
          x = rep(0.486, 2), y = c(0.882, 0.865),
          hjust = 0, gp = gpar(cex = 0.3))
grid.rect(x = rep(0.476, 2), y = c(0.882, 0.865),
          width = rep(0.008, 2), height = rep(0.010, 2),
          gp = gpar(col = c("green", "red"), fill = c("green", "red")))

# Create legend (bottom)
grid.text(label = c("Daily Count", "7-Day Average", "Daily Goal"),
          x = rep(0.085, 3), y = c(0.440, 0.423, 0.408),
          hjust = 0, gp = gpar(cex = 0.3))
grid.segments(x0 = rep(0.061, 3), y0 = c(0.443, 0.426, 0.411),
              x1 = rep(0.081, 3), y1 = c(0.443, 0.426, 0.411),
              gp = gpar(col = c("black", "red", "blue"), lty = c(1, 1, 3), lwd = 0.5))

# Summary stats
grid.text(label = c(paste0(stepStats[2], " km"), "Total Distance",
                    paste0(stepStats[1]), "Total Steps",
                    paste0(stepStats[3]), "Average Daily Steps",
                    paste0(sprintf(stepStats[4], fmt = "%#.2f"), "%"), "Goal Success Rate",
                    paste0(stepStats[5], " days"), "Longest Goal Streak",
                    paste0(step30Stats[1]), "30-day Total Steps",
                    paste0(step30Stats[2]), "30-day Average Daily Steps",
                    paste0(sprintf(step30Stats[3], fmt = "%#.2f"), " %"), "30-day Goal Success Rate"),
          x = rep(c(0.605, 0.709), 8), rep(c(seq(0.946, 0.755, length.out = 5),
                                             seq(0.695, 0.597, length.out = 3)), each = 2), hjust = 0,
          gp = gpar(cex = 0.5, col = c(rep(c("blue", "black"), 5), rep(c("purple", "black"), 3))))
grid.segments(x0 = 0.605, y0 = 0.725, x1 = 0.958, y1 = 0.725, gp = gpar(lwd = 0.65))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()
