## First, download the zipped data file to 'downloads' directory using the given link and then extract the file
## Change the R working directory to 'downloads/exdata-data-household_power_consumption' to read in the portion of the file of interest

setwd("C:/Users/Ronald/Downloads/exdata-data-household_power_consumption")

## Read the first 20 lines of file to see header and file organization
## The file is sequential in time with one entry per minute. In other words, each minute is one row in file. 
## The Date and time sampling starts on 12/16/2006 at approximately 17:24
## We are only interested in values sampled on 2/1 and 2/2/2007
## At one sample/minute, there will be 24 x 60 = 1440 samples/day
## We would like to skip X lines in the file to get us near 2/1 
## We then read at least 2 x 1440 lines to make sure we include all of the samples for 2/1 and 2/2
## There are approximately 46 days and 7 hours from the time the file starts to the beginning of 2/1
## This translates to 66,600 minutes. So we will skip 66,600 lines and then read the next 3000
## This should give us a dataframe with the records we need which can be sub-setted to get only 2/1 and 2/2.

df1 <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?", skip=66600, nrow = 3000)

## We also need to get the right variable (column) names. 

df2 <- read.table("household_power_consumption.txt", header = TRUE, sep = ";",  nrow = 3)
n <- names(df2)
names(df1) <- n

## Change the working directory back to homework

setwd("~/R/Homework")

## Get lubridate for handling date-time variables

library(dplyr)
library(lubridate)

## create df3 to keep df1 intact

df3 <- df1

df3$Date <- dmy(df3$Date)

## subset for the two days of interest

df3 <- subset(df3, df3$Date > dmy("31/1/2007") & df3$Date < dmy("3/2/2007"))

par(mfcol = c(2,2))

par(mar=c(3, 3, 2, 2), ps = 12, pty = "s")
plot(df3$Global_active_power, type="l", pch=0, ylab="Global Active Power", xaxt='n')
axis(1, c(1, 1440,2880), c("Thu","Fri", "Sat"))

par(mar=c(3, 3, 2, 2), ps = 12, pty = "s")
plot(df3$Sub_metering_1,type="l", pch=0, ylab="Energy Sub Metering", xaxt='n', col="black")
points(df3$Sub_metering_2,type="l", pch=0,  col="red")
points(df3$Sub_metering_3,type="l",  col="blue")
axis(1, c(1, 1440,2880), c("Thu","Fri", "Sat"))
legend("topright", inset=0, cex = .5, c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), lty=c(1,1,1), col=c("black","red","blue"), horiz=FALSE, bty="n")

par(mar=c(4, 3, 2, 2), ps = 12, pty = "s")
plot(df3$Voltage,type="l", pch=0, ylab="Voltage",  xlab = "datetime", xaxt='n')
axis(1, c(1, 1440,2880), c("Thu","Fri", "Sat"))

par(mar=c(4, 3, 2, 2), ps = 12, pty = "s")
plot(df3$Global_reactive_power, type="l", pch=0, ylab = "Global_reactive_power", xlab = "datetime", xaxt='n')
axis(1, c(1, 1440,2880), c("Thu","Fri", "Sat"))

dev.print(png, "plot4.png", width = 480, height = 480, units = "px")
