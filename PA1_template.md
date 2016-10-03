---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---
The variables included in this dataset are:

*     steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

*     date: The date on which the measurement was taken in YYYY-MM-DD format

*     interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

Load the data from the activity.csv file. Review the data structure to ensure the columns are of an approriate data type. 


```r
act.data <- read.csv("activity.csv")
str(act.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

Prepare a histogram of steps taken each day. 
Calculate and report the mean and median steps. For this analysis, the "NA" values were excluded from the calculations. 


```r
hist(act.data$steps)
rug(act.data$steps)
```

![plot of chunk calcmean](figure/calcmean-1.png)

```r
mean <- mean (act.data$steps, na.rm = TRUE)
median <- median (act.data$steps, na.rm = TRUE)
paste("Excluding NA values, the mean and median of the total number of steps is ",format(mean,digits=2,nsmall=2)," and ", median,", respectively.",sep="")
```

```
## [1] "Excluding NA values, the mean and median of the total number of steps is 37.38 and 0, respectively."
```

## What is the average daily activity pattern?

Prepare a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, average across all days (y-axis).

Determine which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps <- act.data$steps
interval <- act.data$interval
avg.steps <- aggregate(x=steps,by=list(interval),FUN=mean,na.rm=TRUE)
colnames(avg.steps) <- c("interval","avg_steps")
with(avg.steps,plot(interval,avg_steps,main="Average Steps Across All Dates"))
```

![plot of chunk timeseries](figure/timeseries-1.png)

```r
avg.steps[which.max(avg.steps[,2]),]
```

```
##     interval avg_steps
## 104      835  206.1698
```

```r
paste("The 5-minute interval with the maximum number of steps is ",
      avg.steps[which.max(avg.steps[,2]),][1,1], sep="")
```

```
## [1] "The 5-minute interval with the maximum number of steps is 835"
```


## Imputing missing values
1.    Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

*     The "is.na" function was used to identify the NA records.


```r
paste("The total number of missing values in the dataset is ",sum(is.na(steps)),".",sep="")
```

```
## [1] "The total number of missing values in the dataset is 2304."
```

2.    Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*     The strategy used was to populate the NA values with the average values across dates for that interval, using the average data computed earlier.


```r
# make copy of original data and add avg_steps column
act.data2 <- merge(act.data, avg.steps, by="interval")
# get index of rows with NA
my.na <- is.na(act.data2$steps)
# replace NA values with average steps
act.data2$steps[my.na] <- act.data2$avg_steps[my.na]
```

3.    Create a new dataset that is equal to the original dataset but with the missing data filled in.

*     See prior code chunk; unneeded column is deleted in the following chunk.


```r
# delete avg_steps column; no longer needed
act.data2$avg_steps <- NULL
head(act.data2)
```

```
##   interval    steps       date
## 1        0 1.716981 2012-10-01
## 2        0 0.000000 2012-11-23
## 3        0 0.000000 2012-10-28
## 4        0 0.000000 2012-11-06
## 5        0 0.000000 2012-11-24
## 6        0 0.000000 2012-11-15
```


4.    Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

*     There are no differences in the mean or median between  the original and the filled datasets, which is consistent with the method used to fill the NA values.  A summary is presented below. 


```r
# make histogram
hist(act.data2$steps)
```

![plot of chunk calcmean2](figure/calcmean2-1.png)

```r
# calculation mean and median
mean2 <- mean (act.data2$steps)
median2 <- median (act.data2$steps)
# report mean and median
paste("Excluding NA values, the mean and median of the total number of steps is ",format(mean2,digits=2,nsmall=2)," and ", median2,", respectively.",sep="")
```

```
## [1] "Excluding NA values, the mean and median of the total number of steps is 37.38 and 0, respectively."
```

```r
stat_sum <- data.frame(V1 = c("Original","Filled"),V2 = c(mean,mean2),
                  V3 = c(median, median2))
colnames(stat_sum)<-c("Dataset","Mean","Median")
stat_sum
```

```
##    Dataset    Mean Median
## 1 Original 37.3826      0
## 2   Filled 37.3826      0
```

## Are there differences in activity patterns between weekdays and weekends?

*     Add columns to distinguish weekdays from weekends, then subset the data and plot weekends and weekday averages separately.

*     The weekend data shows the number of steps is spread across more intervals, although the mean across days has a higher maximum step count on weekdays. The interval where the maximum steps occurs is different. 

*     The weekend Mean is 42.37 steps, while the weekday Mean is 35.61 steps. 


```r
require(timeDate)
```

```
## Loading required package: timeDate
```

```r
library(timeDate)
act.data2$wkEnd <- isWeekend(act.data2$date)
act.data2$wkDay <- isWeekday(act.data2$date)
my.wkEnd <- act.data2$wkEnd
my.wkDay <- act.data2$wkDay
act.data2$dayType[act.data2$wkEnd] <- "Weekend"
act.data2$dayType[act.data2$wkDay] <- "Weekday"
act.data2$wkEnd = NULL
act.data2$wkDay = NULL
head(act.data2)
```

```
##   interval    steps       date dayType
## 1        0 1.716981 2012-10-01 Weekday
## 2        0 0.000000 2012-11-23 Weekday
## 3        0 0.000000 2012-10-28 Weekend
## 4        0 0.000000 2012-11-06 Weekday
## 5        0 0.000000 2012-11-24 Weekend
## 6        0 0.000000 2012-11-15 Weekday
```

```r
#create subsets
data.wkEnd <- subset(act.data2,dayType == "Weekend")
data.wkDay <- subset(act.data2,dayType == "Weekday")
# create average subsets
avg.wkEnd <- aggregate(x=data.wkEnd$steps,by=list(data.wkEnd$interval),FUN=mean)
avg.wkDay <- aggregate(x=data.wkDay$steps,by=list(data.wkDay$interval),FUN=mean)
colnames(avg.wkEnd) <- c("interval","avg_steps")
colnames(avg.wkDay) <- c("interval","avg_steps")
# check means for differences
mean.wkEnd <- mean(data.wkEnd$steps)
mean.wkDay <- mean(data.wkDay$steps)
mean.wkEnd
```

```
## [1] 42.3664
```

```r
mean.wkDay
```

```
## [1] 35.61058
```

```r
paste("The 5-minute interval with the maximum number of steps is ",
      avg.wkEnd[which.max(avg.wkEnd[,2]),][1,1], " on weekends and ", 
      avg.wkDay[which.max(avg.wkDay[,2]),][1,1], " on weekdays.",
      sep="")
```

```
## [1] "The 5-minute interval with the maximum number of steps is 915 on weekends and 835 on weekdays."
```

```r
# plot on panels, side-by-side to compare visually
par(mfrow=c(2,1), mar=c(5.1,2.1,2.1,2.1),mai = c(1, 0.2, 0.5,0.4) )
with(avg.wkDay,plot(interval,avg_steps,
                    main="Weekdays",type="l",xlab="",
                    ylab = "Number of Steps", ylim=c(0,220)))
with(avg.wkEnd,plot(interval,avg_steps,
                    main="Weekends",type="l",xlab = "Interval", 
                    ylab = "Number of Steps", ylim=c(0,220)))
```

![plot of chunk daytype](figure/daytype-1.png)
