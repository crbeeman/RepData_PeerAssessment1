# Reproducible Research: Week 2 Assignment

library(ggplot2)
library(dplyr)
library(knitr)

echo=TRUE

## Loading and preprocessing the data

### 1. Load the data 

```{r}
        activity <- read.csv('activity.csv')
```

### 2. Process the data
```{r}
data <- na.omit(activity)
data$date <- as.Date(data$date)
```
##What is mean total number of steps taken per day?


```{r}
dailysteps <- tapply(data$steps, data$date, sum, na.rm=TRUE)
```
### 1. Make a histogram of the total number of steps taken each day

```{r}
hist(dailysteps, breaks = 6, main = "Freq of steps per day", 
    xlab = "Number of steps", ylab = "Frequency", col = "blue")
```

### 2. Calculate the mean and median

```{r}
        stepsMean <- mean(steps, na.rm=TRUE)
        stepsMedian <- median(steps, na.rm=TRUE)
```
Mean: 'r stepsMean'
*Median: 'r stepsMedian'

##What is the average daily activity pattern?

```{r}
averageSteps <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
'''

### 1. time series plot

'''r
AvgInterval <- aggregate(steps ~ interval, data, mean)
AvgDailySteps <- aggregate(steps ~ date, data, mean)

plot(AvgInterval$interval, AvgInterval$steps, type='l', col=1, main="Average steps per Interval", xlab="Time", ylab="Average number of steps")
'''

### 2. Which 5 min interval, on average across all the days in the dataset, contains the maximum number of steps?

'''{r}
MaxInterval <- which.max(AvgInterval$steps)

'''
* Max steps: 'r MaxInterval'

## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

'''{r}
NAcount <- length(which(is.na(activity$steps)))
'''

* Number of NA: 'r NAcount'

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

'''{r}
activityNew <- activity
for (i in 1:nrow(activityNew)) {
    if(is.na(activityNew$steps[i])) {
        val <- AvgInterval$steps[which(AvgInterval$interval == activityNew$interval[i])]
        activityNew$steps[i] <- val 
    }
}

'''

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

* Data set is: activityNew

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

'''{r}
stepsNew <- aggregate(steps ~ date, activityNew, sum)
hist(stepsNew$steps, main = "Histogram of steps per day (NA assigned)", xlab = "Steps per day")
'''

'''{r}
stepsNewMean <- mean(stepsNew$steps)
stepsNewMedian <- median(stepsNew$steps)
'''
* Mean (without NA): 'r stepsNewMean'
* Median (without NA):  'r stepsNewMedian'


## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


'''{r}
dayDef <- function(date_val) {
    wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
    } else {
        x <- 'Weekend'
    }
    x
}
'''

### 2. Make a panel plot containing a time series plot

'''{r}
activityNew$day_type <- as.factor(sapply(activityNew$date, dayDef))
dailynewSteps <- aggregate(steps ~ interval+day_type, activityNew, mean)
plt <- ggplot(dailynewSteps, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = day_type)) +
    theme_gray() +
    facet_grid(day_type ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("Num of Steps")) +
    ggtitle("Num of steps per interval by day def")
print(plt)
'''


