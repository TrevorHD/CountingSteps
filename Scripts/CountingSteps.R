##### Load libraries and initialise data ------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(zoo)
library(grid)
library(gridBase)

# Set working directory
setwd("~/GitHub/CountingSteps")

# Load in step data
data <- read.csv("Data/StepData.csv")

# Format date objects
names(data)[1] <- "Year"
data <- mutate(data, Date = as.Date(paste(Month, Day, Year, sep = "/"), "%m/%d/%Y"))





##### Set up plotting functions ---------------------------------------------------------------------------

# Function to generate time series plot
tPlot <- function(data, bottom = FALSE){
  plot(data$Date, data$Steps, type = "l", ylim = c(0, 30000),
       xlab = "", xaxt = "n", ylab = "", yaxt = "n", lwd = 0.50)
  axis(side = 2, at = seq(0, 30000, by = 5000),
       labels = c("0", "", "10k", "", "20k", "", "30k"),
       mgp = c(0.00, 0.17, 0.00))
  lines(data$Date, rollmean(data$Steps, 7, fill = list(NA, NULL, NA)),
        col = "red", lwd = 1)
  if(!is.na(data$Steps[1])){
    abline(h = 8000, col = "blue", lty = 3, lwd = 0.50)}
  if(bottom == TRUE){
    axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "1 mon"),
              labels = month.abb, tcl = -0.20)}
  if(bottom == FALSE){
    axis.Date(1, at = seq(min(data$Date), max(data$Date), by = "1 mon"),
              labels = FALSE, tcl = -0.12)}}

# Function to generate histogram
hPlot <- function(data, bottom = FALSE){
  par(lwd = 0.50)
  data$Steps[data$Steps > 30000] <- 30000
  histColours <-  c(colorRampPalette(c("red4", "red"), 0.50)(8), 
                    colorRampPalette(c("green", "green4"))(8), rep("green4", 24))
  h <- hist(data$Steps, breaks = seq(0, 30000, by = 1000), plot = FALSE)
  h$density <- h$counts/sum(h$counts)*100
  plot(h, freq = FALSE, ylim = c(0, 42), main = "",
       xlab = "", xaxt = "n",  ylab = "", yaxt = "n", col = histColours)
  axis(side = 4, at = seq(0, 42, by = 7),
       labels = c("0%", "", "14%", "", "28%", "", "42%"),
       mgp = c(0.00, -0.15, 0.00))
  box(lwd = 1.00)
  if(!is.na(data$Steps[1])){
    abline(v = 8000, col = "blue", lty = 3, lwd = 0.50)}
  if(bottom == TRUE){
    axis(side = 1, at = seq(0, 30000, by = 5000),
         labels = c("0", "5k", "10k", "15k", "20k", "25k", "30k+"), tcl = -0.20)}
  if(bottom == FALSE){
    axis(side = 1, at = seq(0, 30000, by = 5000),
         labels = FALSE, tcl = -0.12)}}

# Function to assist in plotting YoY indicators
dInd <- function(num, type){
  if(type == "col"){
    inds <- ifelse(num < 0, "red", ifelse(num > 0, "green", "black"))}
  if(type == "pch"){
    inds <- ifelse(num < 0, 25, ifelse(num > 0, 24, 22))}
  if(type == "adj"){
    inds <- ifelse(num < 0, 0.002, ifelse(num > 0, -0.002, 0))}
  return(inds)}





##### Prepare data for plotting ---------------------------------------------------------------------------

# Calculate key stats by year
data %>% group_by(Year) %>% 
  summarise(av = round(mean(Steps, na.rm = TRUE),0),
            ma = max(Steps, na.rm = TRUE),
            st = max(rle(Steps >= 8000)$lengths),
            pt = sprintf("%0.2f%%", sum(Steps >= 8000, na.rm = TRUE)/sum(!is.na(Steps))*100)) -> data_y
for(i in 1:nrow(data_y)){
  if(is.nan(data_y$av[i])){
    data_y[i, 2:ncol(data_y)] <- rep(NA, 4)}}

# Calculate YoY deltas for key stats
data_d <- data_y
data_d$pt <- as.numeric(gsub("%", "", data_d$pt))
data_d <- tail(data_d, -1) - head(data_d, -1)
data_d <- rbind(c(1, 0, 0, 0, 0), data_d)
data_d$Year <- data_y$Year

# Add helper columns for plotting YoY indicators
data_d %>% mutate(avC = dInd(av, "col"), maC = dInd(ma, "col"),
                  stC = dInd(st, "col"), ptC = dInd(pt, "col"),
                  avP = dInd(av, "pch"), maP = dInd(ma, "pch"),
                  stP = dInd(st, "pch"), ptP = dInd(pt, "pch"),
                  avA = dInd(av, "adj"), maA = dInd(ma, "adj"),
                  stA = dInd(st, "adj"), ptA = dInd(pt, "adj")) -> data_d





##### Plot data -------------------------------------------------------------------------------------------

# Set years to plot; must be 5-year window
pYears <- 2025:2029

# Filter data to selected years
data_py <- filter(data_y, Year %in% pYears)
data_pd <- filter(data_d, Year %in% pYears)

# Prepare graphics device
tiff(filename = "Figures/StepData_2025_2029.tif", width = 4800, height = 4000,
     units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1200)
pushViewport(viewport(layout = gly))

# Plot up to 5 years of step count time series, plus legend
pCols <- seq(1, 783, length.out = length(pYears))
for(i in 1:length(pYears)){
  if(i < length(pYears)){
    pushViewport(vp = viewport(layout.pos.row = (pCols[i]):(pCols[i + 1]), layout.pos.col = 1:590))
    par(fig = gridFIG(), mar = c(0.30, 1.00, 0.30, 0.00), cex.axis = 0.40,
        tcl = -0.20, mgp = c(0.00, -0.20, 0.00))
    par(new = TRUE)
    tPlot(filter(data, Year == pYears[i]))}
  if(i == length(pYears)){
    pushViewport(vp = viewport(layout.pos.row = 784:990, layout.pos.col = 1:590))
    par(fig = gridFIG(), mar = c(0.60, 1.00, 0.30, 0.00), cex.axis = 0.40,
        tcl = -0.20, mgp = c(0.00, -0.20, 0.00))
    par(new = TRUE)
    tPlot(filter(data, Year == pYears[i]), bottom = TRUE)}
  popViewport()}
grid.text(label = c("Daily Goal", "7-Day Avg."),
          x = rep(0.057, 2), y = c(0.940, 0.928),
          hjust = 0, gp = gpar(cex = 0.35))
grid.segments(x0 = rep(0.040, 2), y0 = c(0.940, 0.928),
              x1 = rep(0.051, 2), y1 = c(0.940, 0.928),
              gp = gpar(col = c("blue", "red"), lty = c(3, 1), lwd = 0.50))
  
# Plot up to 5 years of step count histograms, plus legend
for(i in 1:length(pYears)){
  if(i < length(pYears)){
    pushViewport(vp = viewport(layout.pos.row = (pCols[i]):(pCols[i + 1]), layout.pos.col = 591:865))
    par(fig = gridFIG(), mar = c(0.30, 0.00, 0.30, 0.30), cex.axis = 0.40,
        tcl = -0.20, mgp = c(0.00, -0.20, 0.00))
    par(new = TRUE)
    hPlot(filter(data, Year == pYears[i]))}
  if(i == length(pYears)){
    pushViewport(vp = viewport(layout.pos.row = 784:990, layout.pos.col = 591:865))
    par(fig = gridFIG(), mar = c(0.60, 0.00, 0.30, 0.30), cex.axis = 0.40,
        tcl = -0.20, mgp = c(0.00, -0.20, 0.00))
    par(new = TRUE)
    hPlot(filter(data, Year == pYears[i]), bottom = TRUE)}
  popViewport()}
grid.text(label = c("Above Goal", "Below Goal"),
          x = rep(0.688, 2), y = c(0.940, 0.928),
          hjust = 1, gp = gpar(cex = 0.35))
grid.rect(x = rep(0.700, 2), y = c(0.940, 0.928),
          width = rep(0.008, 2), height = rep(0.007, 2),
          gp = gpar(col = c("green", "red"), fill = c("green", "red")))

# Add figure labels
grid.text(label = c("Daily Step Count", "(Distribution)", "Daily Step Count", "(Time Series)"),
          x = c(0.039, 0.039, 0.705, 0.705), y = c(0.976, 0.961, 0.976, 0.961),
          hjust = c(0, 0, 1, 1), gp = gpar(cex = 0.40))

# Add labels, key stats by year, and YoY indicators
yBaseline <- c(0.040, 0.070, 0.100, 0.130)
for(i in 1:length(pYears)){
  yAdj <- c(data_pd$stA[i], data_pd$ptA[i], data_pd$maA[i], data_pd$avA[i])
  grid.segments(x0 = 0.750, y0 = c(0.988, 0.793, 0.597, 0.402, 0.205)[i],
                x1 = 0.980, y1 = c(0.988, 0.793, 0.597, 0.402, 0.205)[i])
  grid.segments(x0 = 0.750, y0 = c(0.816, 0.621, 0.425, 0.230, 0.035)[i],
                x1 = 0.980, y1 = c(0.816, 0.621, 0.425, 0.230, 0.035)[i])
  grid.points(x = rep(0.970, 4), y = yBaseline + c(0.795, 0.600, 0.403, 0.209, 0.013)[i] + yAdj,
              pch = c(data_pd$stP[i], data_pd$ptP[i], data_pd$maP[i], data_pd$avP[i]),
              gp = gpar(col = c(data_pd$stC[i], data_pd$ptC[i], data_pd$maC[i], data_pd$avC[i]),
                        fill = c(data_pd$stC[i], data_pd$ptC[i], data_pd$maC[i], data_pd$avC[i]),
                        cex = 0.40))
  grid.text(label = pYears[i], x = 0.750, y = c(0.961, 0.765, 0.570, 0.375, 0.178)[i],
            hjust = 0, gp = gpar(cex = 1.00, fontface = "bold"))
  grid.text(label = c("Longest Goal Streak", "Goal Success Rate",
                      "Maximum Daily Steps", "Average Daily Steps"),
            x = rep(0.750, 4), y = yBaseline + c(0.795, 0.600, 0.403, 0.209, 0.013)[i],
            hjust = 0, gp = gpar(cex = 0.50))
  if(!anyNA(data_py[, i])){
    grid.text(label = c(paste(data_py$st[i], "d"), data_py$pt[i], data_py$ma[i], data_py$av[i]),
              x = rep(0.900, 4), y = yBaseline + c(0.795, 0.600, 0.403, 0.209, 0.013)[i],
              hjust = 0, gp = gpar(cex = 0.50))}}

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

