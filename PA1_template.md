---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
rawData<-read.csv("activity.csv")
stripNA<-rawData[!is.na(rawData$steps),]
```
## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
totalStepsPerDay<-aggregate(stripNA$steps, by=list(date=stripNA$date), FUN=sum)
head(totalStepsPerDay)
```

```
##         date     x
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(totalStepsPerDay$x,xlab="Total Steps Per Day", main="Histogram of Total Steps Per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
summary(totalStepsPerDay$x)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgStepPerInterval<-aggregate(stripNA$steps, by=list(interval=stripNA$interval), FUN=mean)

with(avgStepPerInterval, plot(interval, x, main = "Average Steps per Interval", xlab="Intervals in a Day", ylab="Average Steps", type="l"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgStepPerInterval[avgStepPerInterval$x==max(avgStepPerInterval$x),]
```

```
##     interval        x
## 104      835 206.1698
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missValues<-rawData[is.na(rawData$steps),]
nrow(missValues)
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
fillNA<- function(rawData, avgStepPerInterval)
{
  newData<-rawData
	for(i in 1:nrow(newData))
	{
	   if(is.na(newData[i,"steps"]))
	   {
	      interval<-newData[i,"interval"]
		  newData[i,"steps"] = avgStepPerInterval[ which(avgStepPerInterval$interval==interval),"x"]
	   }
	}
    newData
}

newData<-fillNA(rawData,avgStepPerInterval)
head(newData)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day 


```r
newTotalPerDay<-aggregate(newData$steps, by=list(date=newData$date), FUN=sum)
head(newTotalPerDay)
```

```
##         date        x
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

and Calculate and report the mean and median total number of steps taken per day.


```r
summary(newTotalPerDay$x)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

Do these values differ from the estimates from the first part of the assignment?

Yes.

What is the impact of imputing missing data on the estimates of the total daily number of steps?

For original data with NA values, imputing missing data increases estimates of the total daily number of steps. But for other records without NA values, it does not have any effect.




## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels ¡V ¡§weekday¡¨ and ¡§weekend¡¨ indicating whether a given date is a weekday or weekend day.

First things first, we need to set current teeminal locale in English to conform to requirement.

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

Create a Vector of weekdays

```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
```

Add factor column to show weekday or weekend.


```r
newData$date<-as.Date(newData$date)
newData$wDay <- factor((weekdays(newData$date) %in% weekdays1),levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 

head(newData)
```

```
##       steps       date interval    wDay
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
weekDayData <- newData[ which(newData$wDay=='weekday'), ]
weekendData <- newData[ which(newData$wDay=='weekend'), ]

avgWeekDay<-aggregate(weekDayData$steps, by=list(interval=weekDayData$interval), FUN=mean)

avgWeekend<-aggregate(weekendData$steps, by=list(interval=weekendData$interval), FUN=mean)

par(mfrow = c(2, 1))

plot(avgWeekDay$interval, avgWeekDay$x, main = "weekday", xlab="interval", ylab="Number of steps", type="l")
plot(avgWeekend$interval, avgWeekend$x, main = "weekend", xlab="interval", ylab="Number of steps", type="l")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
