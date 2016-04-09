
# Construct the plot and save it to a PNG file with a width of 480 pixels 
# and a height of 480 pixels.
# Name each of the plot files as plot1.png, plot2.png, etc
# Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the 
# corresponding plot, i.e. code in plot1.r constructs the plot1.png plot. Your 
# code file should include code for reading the data 
# so that the plot can be fully reproduced. You must also include the code that 
# creates the PNG file.
# Add the PNG file and R code file to the top-level folder of your git repository
# (no need for separate sub-folders)

# We will only be using data from the dates 2007-02-01 and 2007-02-02.

# ordinarily this function would be in a separate file, sourced in and not
# duplicated at the top of every file but I include it so that every file is
# self contained for the purpose of this exercise

library(lubridate)
library(dplyr)
library(data.table)

readFile <- function() {
  file <- "household_power_consumption.txt"
  
  range <- c(ymd("2007-02-01"), ymd("2007-02-03"))
  
  data <- fread(file, sep=";", na.strings="?", stringsAsFactors = T, data.table = F, showProgress = F) %>%
    mutate(DateTime = dmy_hms(paste(Date, Time))) %>%
    filter(DateTime < range[2] & DateTime >= range[1]) %>%
    select(c(10, 3:9))
  
  # convert the numerical fields from factors to numbers
  lapply(2:8, function(x) { data[, x] <<- as.numeric(as.character(data[, x])); NULL })
  
  # return table
  tbl_df (data)
}

createChart <- function(data) {
  png(filename="plot3.png", width=480, height=480, bg="transparent")

  plot(data$DateTime, data$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(data$DateTime, data$Sub_metering_2, col="red")
  lines(data$DateTime, data$Sub_metering_3, col="blue")
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, col=c("black", "red", "blue"))
  
  dev.off()
}

data <- readFile()
createChart(data)