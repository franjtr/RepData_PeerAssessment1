# Reproducible Research: Peer Assessment 1






## Reading the data



```r
act <- read.csv("activity.csv", sep = ",")
act$date <- as.Date(act$date, format="%Y-%m-%d")
```




## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day



```r
steps <- aggregate(act$steps, by=list(act$date), FUN="sum")
hist(steps$x, xlab="Steps", main="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
med <- median(steps$x, na.rm=T)
mean <- mean(steps$x, na.rm=T)
```

The median of the total number of steps per day is 10765 and the mean is 10766.1886792.




## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
interval <- aggregate(act$steps, by=list(act$interval), FUN="mean", na.rm=T)
plot(interval$Group.1, interval$x, type="l", xlab="Time", ylab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxint <- interval[interval$x==max(interval$x),1]
```

The interval from minute 835 to 840 on average contains the maximum number of steps.



## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
any(is.na(act$interval))|any(is.na(act$date))
```

```
## [1] FALSE
```

```r
sum(is.na(act$steps))
```

```
## [1] 2304
```


We will replace the missing values with the averge of the time interval.


```r
replna <- act
for (i in 1:17568) {
      if (is.na(replna[i,1])){
            replna[i,1] <- interval[interval$Group.1==replna[i,3],2]
      }
}


stepn <- aggregate(replna$steps, by=list(replna$date), FUN="sum")
hist(stepn$x, xlab="Steps", main="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
median(stepn$x)
```

```
## [1] 10766.19
```

```r
mean(stepn$x)
```

```
## [1] 10766.19
```

We observe that the results are very similar to those obtained in the first part of the assignment.



## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(dplyr)
Sys.setlocale("LC_TIME", "English") ## My local language is not english, i use this to get the weekdays in english.
```

```r
weekdata <- mutate(replna, day=as.factor(ifelse(weekdays(date)=="Saturday" | weekdays(date)=="Sunday", "weekend", "weekday")))
week <- subset(weekdata, day=="weekday")
weekend <- subset(weekdata, day=="weekend")
wsteps <- aggregate(week$steps, by=list(week$interval), FUN="mean")
westeps <- aggregate(weekend$steps, by=list(weekend$interval), FUN="mean")
par(mfrow=c(1,2))
plot(wsteps[,1], wsteps$x, type="l", xlab="Time", ylab="Weekday Steps", lwd=1.5, col="blue")
plot(westeps[,1], westeps$x, type="l", xlab="Time", ylab="Weekend Steps", lwd=1.5, col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->





