# PA1_template
Philippine Reimpell  
25 Mai 2017  

#Reproducible Research Course Project 1
##Personal activity data

##Loading and preprocessing the data
1. Code for reading in the dataset and/or processing the data

```r
library(data.table)
library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## data.table + dplyr code now lives in dtplyr.
## Please library(dtplyr)!
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(knitr)
library(ggplot2)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:data.table':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday,
##     week, yday, year
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     combine, src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
#Change systems settings to English
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
activity <- fread("C:\\Users\\Philippine\\Documents\\R\\repdata%2Fdata%2Factivity\\activity.csv")
## Convert date variable from character to Date with lubridate
activity$date <- ymd(activity$date)
```


##What is the mean total of steps taken per day?
1. What is the total number of steps taken per day?

```r
stepsPerDay <- aggregate(steps ~ date, activity, sum)
head(stepsPerDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
include=TRUE
```

2. Histogramm of the total number of steps taken per day

```r
q1 <- ggplot(stepsPerDay, aes(x=date, y=steps))+geom_bar(stat="identity", aes(fill="orange"))+guides(fill=FALSE)+labs(title="Total number of steps taken per day with NAs")+theme(plot.title = element_text(hjust = 0.5))
q1
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
include=TRUE
```

3. Calculate and report the mean and median of the total number of steps per day

```r
#mean
meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay
```

```
## [1] 10766.19
```

```r
#median
medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay
```

```
## [1] 10765
```

```r
include=TRUE
```

##What is the average daily activity pattern?
1. Make a time-series plot of the 5-minute interval and the average number of steps taken, averaged accross all days

```r
stepsPerInterval <- aggregate(steps ~ interval, activity, sum)
q2<-ggplot(stepsPerInterval, aes(x=interval, y=steps))+geom_line()
q2
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
include=TRUE
```

2. Which 5-minute interval, on average contains the maximum number of steps

```r
MaxInterval <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
MaxInterval
```

```
## [1] 835
```

```r
include=TRUE
```

##Imputing missing values
1. Calculate and report the number of missing values

```r
Missingvalues <- apply(is.na(activity), 2, sum)
Missingvalues
```

```
##    steps     date interval 
##     2304        0        0
```

```r
include=TRUE
```
2. Imputation strategy - Impute with the mean for the day
3. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
activityDataImputed <- activity
activityDataImputed$steps <- impute(activity$steps, fun=mean)
include=TRUE
```


4. Make a histogram of the total number of steps taken each day
4a. Calculate and report the mean and median total number of steps taken per day. 
4b. Do these values differ from the estimates from the first part of the assignment? 
4c. What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsPerDayImputed <- aggregate(steps ~ date, activityDataImputed, sum)
head(stepsPerDayImputed)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
#4a
meanStepsPerDayImputed <- mean(stepsPerDayImputed$steps)
meanStepsPerDayImputed
```

```
## [1] 10766.19
```

```r
medianStepsPerDayImputed <- median(stepsPerDayImputed$steps)
medianStepsPerDayImputed
```

```
## [1] 10766.19
```

```r
#4b - The mean didn't change after the replacements of NAs, the median changed about 0.1% of the original #value.
include=TRUE
```


```r
q3 <- ggplot(stepsPerDayImputed, aes(x=date, y=steps))+geom_bar(stat="identity", aes(fill="orange"))+guides(fill=FALSE)+labs(title="Total number of steps taken per day with imputed data")+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept = meanStepsPerDayImputed, linetype="dashed")
q3
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
include=TRUE
```

##Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
activityDataImputed$week <- ifelse(weekdays(activityDataImputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
StepsPerIntervalByWeek <- aggregate(activityDataImputed$steps ~ activityDataImputed$interval + activityDataImputed$week, activityDataImputed, mean)
names(StepsPerIntervalByWeek) <- c("interval", "week", "steps")
q4<-qplot(interval, steps, data=StepsPerIntervalByWeek, facets=.~week, geom="line")+labs(title="Activity pattern on weekdays and weekends")+theme(plot.title = element_text(hjust = 0.5))
q4
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
include=TRUE
```

## Conclusion
Activity on the weekend appears to be lower than on weekdays
