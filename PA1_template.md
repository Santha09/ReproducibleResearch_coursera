-----
title: Reproducible Research - John Hopkins - Week-2 - Project-1
author: Santha Krishnasamy
-----

## Loading and preprocessing the data

```r
library(dplyr)
library(lattice)
```

###1. Download and process the data 

```r
setwd("/Users/GPR/Learning/JohnHopkins - DS/Reproducible Research/Data")         # set the working directory

fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" # the url of the data file to be downloaded for the project

if (!file.exists("factivity.zip")){
  download.file(fileurl,"factivity.zip",method = "curl")                         # download the file
  unzip("factivity.zip")
}
activity <- read.csv("activity.csv",sep=",",na="NA")                             # read the csv file
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
activity$date <- as.character(as.Date(activity$date,"%Y-%m-%d"))                 # convert the date into date format
activity$interval <- as.factor(activity$interval)                                # convert the interval into factor
```

## What is mean total number of steps taken per day?

###2. Histogram of total number of steps taken each day

```r
daysum <- tapply(activity$steps,activity$date,sum,na.rm=TRUE)
hist(daysum,breaks = 20, col = "green", xlab = "Total Steps Daily", ylab = "Frequency (in Days)", main = "Histogram of the total number of steps taken each day (IGNORING MISSING VALUE)")
```

![plot of chunk - histogram - total steps taken each day](figure/- histogram - total steps taken each day-1.png)

###3. Mean and Median number of steps taken each day

The Mean number of steps taken each day is 9354.2295082

The Median for the number of steps taken each day is 10395

But let us see whether there is any difference between the above mean and median value with evaluating the mean and median of number of steps taken each day individually


```r
daymeanmedian <- activity %>% group_by(date) %>% 
                                summarise(daymean = mean(steps,na.rm=TRUE),daymedian = median(steps,na.rm=TRUE))
with(daymeanmedian,plot(daymean ~ as.factor(date), na.rm = TRUE, col = "red", 
                        xlab = "Date", ylab = "Average steps taken daily", 
                        main = "Average number of steps taken each day"))
```

![plot of chunk - mean and median take each day](figure/- mean and median take each day-1.png)

```r
with(daymeanmedian,plot(daymedian ~ as.factor(date), na.rm = TRUE ,
                        type="l", col = "pink",
                        xlab = "Date", ylab = "Median number of steps taken daily", 
                        main = "Median value of steps taken each day"))
```

![plot of chunk - mean and median take each day](figure/- mean and median take each day-2.png)

If you see the median value of steps taken each day individually, then its mostly zero

Let us the histogram with the summary values

```r
mean <- mean(daysum,na.rm=TRUE)
median <- median(daysum)
hist(daysum,breaks = 20, col = "green", xlab = "Total Steps Daily", ylab = "Frequency (in Days)", main = "Histogram of the total number of steps taken each day")
abline(v=mean,col="black",lty=2,lwd=2)
abline(v=median,col="red",lty=3,lwd=2)
legend(x="topright",legend = c("Mean","Median"),lty=c(2,3),lwd=2,col=c("black","red"))
```

![plot of chunk - histogram with summary values](figure/- histogram with summary values-1.png)

The quantile of the sum of steps taken each day is 0, 6778, 1.04 &times; 10<sup>4</sup>, 9354, 1.281 &times; 10<sup>4</sup>, 2.119 &times; 10<sup>4</sup>

## What is the average daily activity pattern?

###4. Time series plot of the average number of steps taken

```r
avgsteps_interval <- activity %>% group_by(interval) %>% 
  summarise(avgsteps = mean(steps, na.rm =TRUE))
plot(avgsteps_interval, type = "l",
     xlab = "Interval", ylab = "Average steps taken",
     main = "Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days",
     cex.main = .8)
```

![plot of chunk - time series of avg steps](figure/- time series of avg steps-1.png)

###5. The 5-minute interval that, on average, contains the maximum number of steps is 835

###6. Code to describe and show a strategy for imputing missing data

```r
missingna <- sum(is.na(activity$steps))
```

Total number of missing values are 2304.

We will do little bit of exploratory analysis to better understand which part of the day mostly there exists a missing value


```r
activity$weekday <- as.factor(weekdays(as.Date(activity$date)))
activity %>% group_by(weekday) %>% summarise(sum(is.na(steps)))
```

```
## # A tibble: 7 × 2
##     weekday `sum(is.na(steps))`
##      <fctr>               <int>
## 1    Friday                 576
## 2    Monday                 576
## 3  Saturday                 288
## 4    Sunday                 288
## 5  Thursday                 288
## 6   Tuesday                   0
## 7 Wednesday                 288
```

If see the above summary of missing value grouped by weekday, except tuesdays, every other weekdays have missing values. But there is first and last working day have more number of missing values which obviously make lots of sense. The rest of the days Wednesday, Thursday, Saturday and Sunday have equal nuber of missing of values. We could easily draw a pattern here based on our own fitness workout pattern.

### To Impute we can strategise based on following logic
* For Friday and Monday, we impute with the mean value of that particular day alone
* For the rest of the week days, we impute the mean value of Wednesday, Thursday, Saturday and Sunday


```r
FriMon <- subset(activity,weekday == "Friday" | weekday =="Monday")
frimonavg <- FriMon %>% group_by(weekday,interval) %>% summarise(avgsteps = mean(steps,na.rm=TRUE) )
WTSaSu <- subset(activity,weekday == "Wednesday" | weekday =="Thursday" | 
                   weekday == "Saturday" | weekday == "Sunday")
wtsasuavg <- WTSaSu %>% group_by(weekday,interval) %>% summarise(avgsteps = mean(steps,na.rm=TRUE) )
activityAvgMday <- rbind(frimonavg,wtsasuavg)
activityImpute <- merge(activity,activityAvgMday,by = c("weekday","interval"))
activityImpute$impsteps <- ifelse(is.na(activityImpute$steps),activityImpute$avgsteps,activityImpute$steps)
Imputedaysum <-tapply(activityImpute$impsteps,activityImpute$date,sum)
meanImpute <- mean(Imputedaysum)
medianImpute <- median(Imputedaysum)
```

The mean and median for the imputed values are 1.114515 &times; 10<sup>4</sup> and 1.1257 &times; 10<sup>4</sup>
The mean and median for the original data set are 9354.2295082 and 10395

The difference between imputed and original mean is `r mean 
###7. Let us see the histogram of total number of steps taken each day after imputing missing values

```r
hist(Imputedaysum,breaks = 20, col = "green", 
     xlab = "Total Steps Daily", ylab = "Frequency (in Days)", 
     main = "Histogram of the total number of steps taken each day (AFTER IMPUTE)")
abline(v=meanImpute,col="black",lty=2,lwd=2)
abline(v=medianImpute,col="red",lty=3,lwd=2)
legend(x="topright",legend = c("Mean","Median"),lty=c(2,3),lwd=2,col=c("black","red"))
```

![plot of chunk - histogram - after Imputing - total steps taken each day](figure/- histogram - after Imputing - total steps taken each day-1.png)

###8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
activityImpute <- activityImpute %>% mutate(weekdayend = 
                                              ifelse(weekday == "Saturday" | weekday == "Sunday","weekend","weekday"))
activityImputeavgWD <- activityImpute %>% group_by(weekdayend,interval) %>% summarise(avgsteps = mean(impsteps))
xyplot(avgsteps ~ interval | weekdayend,data = activityImputeavgWD, type = 'l', layout = c(2,1),
       xlab = "Interval", ylab = "Average Number of Steps", 
       main = "Average number of steps taken per 5-minute interval across weekdays and weekends")
```

![plot of chunk - xyplot](figure/- xyplot-1.png)
