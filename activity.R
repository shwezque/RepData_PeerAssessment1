# Assignment: Course Project 1

# Libraries used
library(lubridate); library(dplyr)

# Loading and preprocessing the data
## 1. Load the data (i.e. read.csv())
## 2. Process/transform the data (if necessary) into a format 
## suitable for your analysis
data <- read.csv("activity.csv", header=TRUE, na.strings="NA",
                 colClasses=c("numeric", "character", "integer"))
data$date <- ymd(data$date)


# What is mean total number of steps taken per day?
## 1. calculate the total number of steps taken per day
data.rm <- data[!is.na(data$step),]
dailySteps <- tapply(data.rm$steps, data.rm$date, sum)

## 2. Make a histogram of the total number of steps taken each day
par(mfrow=c(1,1), bg="#ffffff")
hist(dailySteps, breaks=20, col="#FF6659",
     xlab="Day", ylab="Frequency",
     main="Total number of steps taken each day")
dev.copy(png,file="hist_TotalSteps.png")
dev.off()

## 3. Calculate and report the mean and median of the total number 
## of steps taken per day
mean(dailySteps)
median(dailySteps)


# What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type="l") of the 5-minute
## interval (x-axis) and the average number of steps taken, 
## averaged across all days (y-axis)
dailyAct <- tapply(data.rm$steps, data.rm$interval, mean)
par(mfrow=c(1,1), bg="#ffffff")
plot(y=dailyAct, x=names(dailyAct), type="l", col="#00C2C2", lwd=2,
     xlab="5-minute-intervals", ylab="Average number of steps",
     main="Daily Activity Pattern")
dev.copy(png,file="plot_IntervalSteps.png")
dev.off()

## 2. Which 5-minute interval, on average across all the days 
## in the dataset, contains the maximum number of steps?
dailyAct[dailyAct==max(dailyAct)]


# Imputing Missing Values
## 1. Calculate and report the total number of missing values in 
## the dataset (i.e. the total number of rows with NAs).
missing <- nrow(data)-
    sum(!is.na(data)[,1]&!is.na(data)[,2]&!is.na(data)[,3])
## or
missing <- sum(is.na(data))

## 2. Devise a strategy for filling in all of the missing values
## in the dataset.
## Use average number of steps in the same 5-minute interval

## 3. Create a new dataset that is equal to the original dataset 
## but with the missing data filled in.
meanIntervalSteps <- tapply(data$steps, data$interval, mean,
                            na.rm=TRUE, simplify=TRUE)
data.nw <- data
nas <- is.na(data.nw$steps)
data.nw$steps[nas] <- meanIntervalSteps[as.character(data.nw$interval[nas])]
sum(is.na(data.nw))

## 4. Make a histogram of the total number of steps taken each day 
## and Calculate and report the mean and median total number of 
## steps taken per day. Do these values differ from the estimates 
## from the first part of the assignment? What is the impact of 
## imputing missing data on the estimates of the total daily number 
## of steps?
dailySteps.nw <- tapply(data.nw$steps, data.nw$date, sum)
par(mfrow=c(1,1), bg="#ffffff")
hist(dailySteps.nw, breaks=20, col="#FF6659",
     xlab="Day", ylab="Frequency",
     main="Total number of steps taken each day")
dev.copy(png,file="hist_DailyStepsNew.png")
dev.off()
mean(dailySteps.nw)
median(dailySteps.nw)
## Impact is mean and median are both the same at 10766.19

# Are there differences in activity patterns between weekdays and
# weekends? Use weekdays() function and the dataset with filled-in
# missing values.
## 1. Create a new factor variable in the dataset with two levels 
## - "weekday" and "weekend" indicating whether a given date is a 
## weekday or weekend day.
data.wk <- data.nw
data.wk$dayType <- NA
for (i in 1:nrow(data.wk)) {
    if (weekdays(data.wk$date[i])=="Saturday" | 
        weekdays(data.wk$date[i])=="Sunday") {
        data.wk$dayType[i]<-"Weekend"
    }
    else {
        data.wk$dayType[i]<-"Weekday"
    }
}

## 2. Make a panel plot containing a time series plot (type="l") of
## the 5-minute interval (x-axis) and the average number of steps
## taken, averaged across all weekday days or weekend days(y-axis).
meanStepsWeekday <- tapply(data.wk[data.wk$dayType=="Weekday",]$steps, 
                           data.wk[data.wk$dayType=="Weekday",]$interval, mean,
                           na.rm=TRUE, simplify=TRUE)
meanStepsWeekend <- tapply(data.wk[data.wk$dayType=="Weekend",]$steps, 
                           data.wk[data.wk$dayType=="Weekend",]$interval, mean,
                           na.rm=TRUE, simplify=TRUE)
par(mfrow=c(1,2), bg="#ffffff")
plot(meanStepsWeekday, col="#FF6659", type="l", lwd="2",
     xlab="interval", ylab="Average number of steps",
     main="Weekday", ylim=c(0,250))
plot(meanStepsWeekend, col="#00C2C2", type="l", lwd="2",
     xlab="interval", ylab="Average number of steps",
     main="Weekend", ylim=c(0,250))
dev.copy(png,file="panelplot_WeekendWeekdayActivityPattern.png")
dev.off()




