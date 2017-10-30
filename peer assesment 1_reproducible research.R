# set working directory

setwd("C:/Users/mihaldma/Documents/coursera/REPRODUCIBLE_RESEARCH/REPRODUCIBLE_RESEARCH_PEER ASSESSMENT1")

dir()

library(knitr)
library(dplyr)
library(ggplot2)
library(lattice)
library(reshape2)
library(lubridate)

# Loading and preprocessing the data

# Show any code that is needed to Load the data

opts_chunk$set(echo = TRUE)


data <- read.csv("activity.csv")

head(data)

tail(data)

summary(data)

str(data)



# Process/transform the data (if necessary) into a format suitable for your analysis

dt = Sys.time()

date <- format(dt,"%d-%b-%Y")

time <- format(dt,"%H:%M:%S")

Rversion <- version$version.string

str(data)

head(data)

summary(data)

datatransform <- na.omit(data)

head(datatransform)

summary(datatransform)

## What is mean total number of steps taken per day?

# Calculate the total number of steps taken per day

sumsteps<- aggregate(steps ~ date, datatransform, FUN=sum)

head(sumsteps)

hist(sumsteps$steps, main= "Total Steps per Day", xlab="Steps")

abline(v=median(sumsteps$steps), lty=4, col="red")


summary(sumsteps$steps)

## What is the average daily activity pattern?

stepsinterval<- aggregate(steps ~ interval, datatransform, mean, na.rm = TRUE)

plot (stepsinterval$interval, stepsinterval$steps, type = "l", lwd = 2, 
     xlab = "Interval", 
     ylab = "Total Steps", 
     main = "Average daily activity pattern")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


summary(stepsinterval)

stepsinterval[which.max(stepsinterval$steps),]
stepsinterval

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

missingvalues <- is.na(data)

totalmissingvalues <- sum(as.numeric(missingvalues))

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# median for 5-minute interval as the strategy to fill the missing values

nd <- data
for (i in stepsinterval) {
  nd[nd$interval == i & is.na(nd$steps), ]$steps <- 
    stepsinterval$steps[stepsinterval$interval == i]
}

head(nd)

sum(is.na(nd))

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and 
# median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

daily_no_steps <- aggregate(steps ~ date, data = nd, sum, na.rm = TRUE)

hist(daily_no_steps$steps, main = "Daily total number of steps", xlab = "Step", breaks = 60)
abline(v=median(daily_no_steps$steps), lty = 5, col = "red", main = "Median")

median(daily_no_steps$steps)
mean(daily_no_steps$steps)

median(sumsteps$steps)
mean(sumsteps$steps)



## Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable in the dataset with two levels – 
# “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


nd <-nd %>% 
  
  mutate(day=as.factor(ifelse(wday(date) %in% c(1,7),"weekend","weekday")))

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


nd <- nd %>% 
  
  group_by(day,interval) %>%
  
  summarise(meansteps=mean(steps))

ggplot(nd , aes(x = interval , y = meansteps)) + geom_line() + labs(title = "weekday vs weekend", x = "Interval", y = "Number of Steps") + facet_wrap(~`day` , ncol = 2, nrow=1)
