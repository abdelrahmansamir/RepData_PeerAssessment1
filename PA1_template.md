---
title: 'Reproducible Research: Peer Assessment 1'
author: "Abdelrahman"
date: "May 22, 2017"
output: html_document
---



## 1. Code for reading in the dataset and processing the data


```r
activity<-read.csv("data/activity.csv",sep = ",",na.strings = "NA")
activity$date<-as.Date(activity$date,"%Y-%m-%d")
```

## 2. Histogram of the total number of steps taken each day  

```r
stepsPerDay<-tapply(activity$steps,activity$date,sum,na.rm=T)
```

1.  display the first few rows of the `stepsPerDay` data

```r
head(stepsPerDay)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

2. Histogram

```r
hist(stepsPerDay, 
     breaks=seq(from=0, to=25000, by=2500),
      col="green", 
      xlab="Number of steps", 
      main="Histogram of the total number of steps per day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

## 3. Mean and median number of steps taken each day

```r
mean(stepsPerDay)
```

```
## [1] 9354.23
```

```r
median(stepsPerDay)
```

```
## [1] 10395
```

## 4. Time series plot of the average number of steps taken


```r
meanIn<-tapply(activity$steps,activity$interval,mean,na.rm=T)
```

1.  display the first few rows of the `meanIn` data

```r
head(meanIn)
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

2. Time series plot

```r
plot(names(meanIn),meanIn,type="l",
     lwd=2,
     col="green", 
     xlab="Interval", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

## 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
# the index of the maximum mean
max_ind <- which(meanIn == max(meanIn))
max_ind
```

```
## 835 
## 104
```

```r
# the value of interval at max mean
max_interval <- meanIn[max_ind]
max_interval
```

```
##      835 
## 206.1698
```

## 6. Code to describe and show a strategy for imputing missing data

1. Show the number of missing values in the input data

```r
numMissingValues <- length(which(is.na(activity$steps)))
numMissingValues
```

```
## [1] 2304
```

2. impute data

```r
# calculate the number of NA values.
NA_count <- sum(is.na(activity$steps))
NA_count
```

```
## [1] 2304
```

```r
# NA indices
na_ind <- which(is.na(activity$steps))

# Create a vector of means
mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_ind))

# Replace the NAs values
activity[na_ind, "steps"] <- mean_vec

head(activity)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```


## 7. Histogram of the total number of steps taken each day after missing values are imputed

```r
stepsByDay <- tapply(activity$steps, activity$date, sum)
hist(stepsByDay,
     col = 'green',
     breaks=seq(from=0, to=25000, by=2500),
     xlab='Total steps per day',
     main="Histogram of the total number of steps taken each day (after imputation)")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
#The mean and median are computed like
mean(stepsByDay)
```

```
## [1] 10766.19
```

```r
median(stepsByDay)
```

```
## [1] 10766.19
```

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends 

1. Create a new variable holding "weekday" and "weekend".

```r
activity$type <-  ifelse(as.POSIXlt(activity$date)$wday %in% c(1:5), 'weekday', 'weekend')
```

2. build a panel plot

```r
mean_da <- aggregate(activity$steps, 
                     by=list(activity$type 
                    , activity$interval), mean)

names(mean_da) <- c("type", "interval", "mean")

library(lattice)
xyplot(mean ~ interval | type, mean_da, 
        type="l", 
        col="green",
        lwd=2, 
        xlab="Interval", 
        ylab="Number of steps", 
        layout=c(1,2))
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)


