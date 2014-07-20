# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
mydata <- read.csv("activity.csv")
cc_mydata <- complete.cases(mydata)
cc_mydata <- mydata[cc_mydata,]
mysteps <- mydata$steps
mysteps <- mysteps[!is.na(mysteps)]
#spliting the steps data by date and by interval:
steps_by_date <- split(mydata$steps,mydata$date)
steps_by_interval <- split(cc_mydata$steps,cc_mydata$interval)
```



## What is mean total number of steps taken per day?

```r
#getting the total number of steps per day into a numerical vector
sum_steps <- sapply(steps_by_date, sum)
sum_steps <- sum_steps[!is.na(sum_steps)]

#Histogram for total steps per day
barplot(sum_steps,xlab ='date',ylab='total steps')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
#mean of steps taken per day
mean(sum_steps)
```

```
## [1] 10766
```

```r
#median of steps taken per day
median(sum_steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
steps_by_interval <- split(cc_mydata$steps,cc_mydata$interval)
average_steps_interval <- sapply(steps_by_interval,mean)
intervals <- as.numeric(names(steps_by_interval))
plot(intervals,average_steps_interval, type='l',ylab='average steps')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
#Interval with highest average of steps
max_steps <- max(average_steps_interval)
max_index <- match(max_steps,average_steps_interval)
print(intervals[max_index])
```

```
## [1] 835
```

## Imputing missing values

```r
#Number of missing values
u <- complete.cases(mydata)
print(sum(!u))
```

```
## [1] 2304
```

```r
#My strategy for the missing values is to just find the average of steps across all days, divide that number by the number of intervals in a day, and then replace all NAs with the resulting number

filler_data <- mean(sum_steps)/length(intervals) # equals 37.3826
new_mydata <- mydata
step_data <- mydata$steps
u <- is.na(step_data)
step_data[u] <- filler_data
#New data set:
new_mydata$steps <- step_data
new_mydata[1:5,]
```

```
##   steps       date interval
## 1 37.38 2012-10-01        0
## 2 37.38 2012-10-01        5
## 3 37.38 2012-10-01       10
## 4 37.38 2012-10-01       15
## 5 37.38 2012-10-01       20
```

```r
#plotting total steps each day
new_steps_by_date <- split(new_mydata$steps,new_mydata$date)
new_sum_steps <- sapply(new_steps_by_date, sum)
barplot(new_sum_steps,xlab ='date',ylab='total steps')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
#finding the mean and median of steps
mean(new_sum_steps)
```

```
## [1] 10766
```

```r
median(new_sum_steps)
```

```
## [1] 10766
```


## Are there differences in activity patterns between weekdays and weekends?

```r
day_names  <- weekdays(as.Date(new_mydata$date))
days <- gl(n=2,length = length(day_names),labels = c("weekday","weekend"),k=0)
u <- day_names == "Sunday" | day_names == "Saturday"
days[u] <- as.factor("weekend")
days[!u] <- as.factor("weekday")
new_mydata["day_type"] <- days

new_steps_by_interval <- split(new_mydata$steps,new_mydata$interval)
new_average_steps_interval <- sapply(steps_by_interval,mean)

#cretaing 2 data frames. One for weekdays and one for weekends
u <- new_mydata$day_type == "weekday"
weekday_data <- new_mydata[u,]
u <- new_mydata$day_type == "weekend"
weekend_data <- new_mydata[u,]

weekday_steps_by_interval <- split(weekday_data$steps,weekday_data$interval)
weekend_steps_by_interval <- split(weekend_data$steps,weekend_data$interval)

weekday_average_steps_interval <- sapply(weekday_steps_by_interval,mean)
weekend_average_steps_interval <- sapply(weekend_steps_by_interval,mean)

par(mfrow = c(1,2))
plot(intervals,weekday_average_steps_interval, type='l',ylab='average steps',main = 'weekdays')
plot(intervals,weekend_average_steps_interval, type='l',ylab='average steps',main = 'weekends')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 



