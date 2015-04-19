# Reproducible Research: Peer Assessment 1
```
Sys.setlocale("LC_MESSAGES", 'en_US.UTF-8')
Sys.setlocale("LC_TIME", 'en_US.UTF-8')
```

## Loading and preprocessing the data


```r
data <- read.csv('activity.csv')
data$date <- as.Date(data$date)
```
## What is mean total number of steps taken per day?


```r
total_steps = sum(data$steps, na.rm=TRUE)
total_days = length(unique(data$date))
mean_steps_per_day = total_steps / total_days

steps_per_day = aggregate(x=data$steps, FUN=sum, by=list(date=data$date))
names(steps_per_day) <- c('date', 'steps')
hist(steps_per_day$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean number of steps per day is (1.0766189\times 10^{4}). The median is 10765.

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_interval <- aggregate(steps ~ interval, data, mean, na.rm=TRUE)
plot(steps_interval$interval, steps_interval$steps, type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_idx <- which.max(steps_interval$steps)
interv <-  steps_interval$interval[[max_idx]]
```

Answer: 835 



## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
count_rm <- sum(!complete.cases(data))
```

Answer: 2304.

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will simply use the mean number of steps for that day.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}'

mean_steps_interval <- aggregate(steps ~ interval, data, mean, na.rm=TRUE)
incomplete_indices <- which(!complete.cases(data))

for(i in incomplete_indices) {
    data[i, ]$steps <-  mean_steps_interval[mean_steps_interval$interval==data[i, ]$interval, ]$steps 
}

```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
steps_per_day = aggregate(x=data$steps, FUN=sum, by=list(date=data$date))
names(steps_per_day) <- c('date', 'steps')
hist(steps_per_day$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
mean_steps <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)
```

Answers:
mean: NA.
median: NA.

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


is (1.076618910^{4}). The median is 10765.

The mean is exactly the same, which makes sense because we replaced missing values with the mean. Only the median has changed slightly. It hasn't changed much because the NA's were evently distributed across the entire dataset.


## Are there differences in activity patterns between weekdays and weekends?


Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data$day_group <- ifelse(weekdays(data$date) %in% c('Saturday' ,'Sunday'), 'weekend', 'weekday')
data$day_group <- factor(data$day_group)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
library(lattice)

steps_day_group <- aggregate(steps ~ interval + day_group, data, mean)

xyplot(steps ~ interval | day_group, data = steps_day_group, type = "l", aspect='xy')
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 
