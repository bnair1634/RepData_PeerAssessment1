# Reproducible Research: Peer Assessment 1
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')

## Loading and preprocessing the data
library(data.table)
library(ggplot2)
rdata <- read.csv('activity.csv', header = TRUE, sep = ",",colClasses=c("numeric", "character", "numeric"))
## tidy the data

rdata$date <- as.Date(rdata$date, format = "%Y-%m-%d")
rdata$interval <- as.factor(rdata$interval)
str(rdata)

## What is mean total number of steps taken per day?

steps_per_day <- aggregate(steps ~ date, rdata, sum)
colnames(steps_per_day) <- c("date","steps")
head(steps_per_day)

# make a histogram of the total number of steps taken per day, plotted with appropriate bin interval

ggplot(steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 

steps_mean <- mean(steps_per_day$steps, na.rm=TRUE)
steps_median <- median(steps_per_day$steps, na.rm=TRUE)

## What is the average daily activity pattern?

steps_per_interval <- aggregate(rdata$steps, 
                                by = list(interval = rdata$interval),
                                FUN=mean, na.rm=TRUE)
#convert to integers

steps_per_interval$interval <- 
        as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")

#make the plot with the time series of the average number of steps taken (averaged across all days) versus the 5-minute intervals

ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
        geom_line(color="green", size=1) +  
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw()
# find the 5-minute interval containing the maximum number of steps

max_interval <- steps_per_interval[which.max(  
        steps_per_interval$steps),]

## Imputing missing values





## Are there differences in activity patterns between weekdays and weekends?
