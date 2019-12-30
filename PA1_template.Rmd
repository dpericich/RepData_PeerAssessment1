---
title: "Reproducible Research: Peer Assessment 1"
author : "Daniel Pericich"
output: 
html_document:
keep_md: true
---


## Loading and preprocessing the data
Set Global Options
```{r}
knitr::opts_chunk$set(warning=FALSE, message = FALSE, echo = TRUE)
```

Load all libraries
```{r}
library(ggplot2)
library(tidyr)
library(dplyr)
```
Unzip zip file to access data files
```{r}
unzip("activity.zip")
```

Load and store data from csv file
```{r}
activity <- read.csv("activity.csv")
```

Clean data by adding date class to dates and add extra column with the day of the week 
```{r}
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity, weekday)
```

Show summary of activity data after preprocessing
```{r}
summary(activity)
```

Calculate Total number of steps taken per day
```{r}
steps_per_day <- with(activity, aggregate(steps, by = list(date), sum, na.rm = TRUE))
names(steps_per_day) <- c("date", "steps")
```

Create a histogram for total number of steps
```{r}
hist(steps_per_day$steps, main = "Total steps taken per day", xlab = "Total Steps taken per day",
     col = "red", breaks = seq(0, 22500, by = 2500))
```

## What is mean total number of steps taken per day?
Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean(steps_per_day$steps)
median(steps_per_day$steps)
```

What is the average daily activity pattern?
```{r}
average_steps_taken <- with(activity, aggregate(steps, by = list(interval), mean, na.rm = TRUE))
names(average_steps_taken) <- c("interval", "steps")
plot(average_steps_taken$interval, average_steps_taken$steps, type = "l", main = "Average number
     of steps taken per interval", xlab = "Intervals", ylab = "Average Steps Taken", col = "red",
     lwd = 3)
```

Which 5-minute interval, on average across all the days in the dataset, contains 
the maximum number of steps?
```{r}
average_steps_taken[which.max(average_steps_taken$steps),]$interval
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
value_na <- sum(is.na(activity$steps))
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r}
imputed_steps <- average_steps_taken$steps[match(activity$interval, 
                 average_steps_taken$interval)]
```
Create a new dataset that is equal to the original dataset but with the missing data filled in
```{r}
activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps,
                    no = activity$steps))
total_imputed <- aggregate(steps ~ date, activity_imputed, sum)
names(total_imputed) <- c("date", "_daily_steps")
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
hist(steps_per_day$steps, main = "Total Steps taken per day", xlab = "Steps", col = "red")
mean(steps_per_day$steps)
```
## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y})
```
Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot 
should look like using simulated data.
```{r}
activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot <- ggplot(data = activity_by_date, mapping = aes(interval, steps, color = datetype)) +
  geom_line() +
  labs(title = "Average Steps per day by interval", x = "Interval", y = "Average Steps" ) +
  facet_wrap(~datetype, nrow = 2, ncol = 1)
print(plot)
```

