---
title: "Reproducible Data: Peer Review Assignment 1"
author: "Clyde Tressler"
date: "November 5, 2015"
output: html_document
---

###Reading the Data
First we load the activity data. We use strings as character vectors to facilitate conversion to dates. 


```r
setwd('~/RepData_PeerAssessment1')
dat <- read.csv('activity.csv', stringsAsFactors = FALSE)
dat$date <- as.Date(dat$date)
str(dat)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
###Analysis of Raw Data
Below we group interval observations by day, then plot a histogram of steps by day and report the mean and median number of steps per day. NA observations are removed when summing the interval data. The dplyr library is used to group and summarize the intervals by date.

```r
library(dplyr)
```


```r
date_grouped <- group_by(dat, date)
steps <- summarise_each(date_grouped, funs(sum(., na.rm = TRUE)), steps)
head(steps)
```

```
## Source: local data frame [6 x 2]
## 
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
hist(steps$steps, main = 'Histogram of Steps per Day', xlab = 'Number of Steps', ylim=c(0,20), col='steelblue', breaks=10)
```

![plot of chunk plot_histogram_1](figure/plot_histogram_1-1.png) 

```r
est_mean <- mean(steps$steps)
est_median <- median(steps$steps)
```


```r
est_mean
```

```
## [1] 9354.23
```

```r
est_median
```

```
## [1] 10395
```

Next we group the data by 5-minute interval values in order to visualize the average steps per interval over all days in the observations. 


```r
interval_grouped <- group_by(dat, interval)
interval_steps <- summarise_each(interval_grouped, funs(mean(., na.rm = TRUE)), steps)
head(interval_steps)
```

```
## Source: local data frame [6 x 2]
## 
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
tail(interval_steps)
```

```
## Source: local data frame [6 x 2]
## 
##   interval     steps
## 1     2330 2.6037736
## 2     2335 4.6981132
## 3     2340 3.3018868
## 4     2345 0.6415094
## 5     2350 0.2264151
## 6     2355 1.0754717
```

```r
plot(interval_steps$interval, interval_steps$steps, main = 'Number of Steps per 5-minute Interval', xlab = 'Interval', col='steelblue', type = 'l', ylab = 'Number of Steps', xlim=c(0,2500))
```

![plot of chunk plot_time_series](figure/plot_time_series-1.png) 

We verify the number of intervals observed and then identify the interval with the maximum number of steps:

```r
length(unique((dat$interval)))/12
```

```
## [1] 24
```

```r
subset(interval_steps, steps==max(steps))
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
## 1      835 206.1698
```
Calculating the time of day for max steps, assuming interval 0 corresponds to 00:00hrs. This may not be true, but I'm curious

```r
835/60 #calculate hours
```

```
## [1] 13.91667
```

```r
.91667*60 #calculate minutes from remainder
```

```
## [1] 55.0002
```

Time of day caluclated as 13:55. This is believable, but not proven.
Next we calculate the number of NA observations in the dataset:

```r
sum(is.na(dat$steps))
```

```
## [1] 2304
```
###Imputing Values for Missing Observations
Now we use vector recycling in R to quickly assign the step count means for each interval as a column in the original data frame, and then add another column to impute the missing values of the step observations by substituting the mean value when an NA is present in the original data.


```r
unique(dat$interval == interval_steps$interval) #check to see the intervals all match
```

```
## [1] TRUE
```

```r
dat$int_means <- interval_steps$steps
dat$imputed_steps <- ifelse(is.na(dat$steps), dat$int_means, dat$steps)
```
Let's make sure it worked:

```r
head(dat)
```

```
##   steps       date interval int_means imputed_steps
## 1    NA 2012-10-01        0 1.7169811     1.7169811
## 2    NA 2012-10-01        5 0.3396226     0.3396226
## 3    NA 2012-10-01       10 0.1320755     0.1320755
## 4    NA 2012-10-01       15 0.1509434     0.1509434
## 5    NA 2012-10-01       20 0.0754717     0.0754717
## 6    NA 2012-10-01       25 2.0943396     2.0943396
```

```r
tail(dat)
```

```
##       steps       date interval int_means imputed_steps
## 17563    NA 2012-11-30     2330 2.6037736     2.6037736
## 17564    NA 2012-11-30     2335 4.6981132     4.6981132
## 17565    NA 2012-11-30     2340 3.3018868     3.3018868
## 17566    NA 2012-11-30     2345 0.6415094     0.6415094
## 17567    NA 2012-11-30     2350 0.2264151     0.2264151
## 17568    NA 2012-11-30     2355 1.0754717     1.0754717
```

```r
dat[14000:14015,] #looks right!
```

```
##       steps       date interval int_means imputed_steps
## 14000     0 2012-11-18     1435  27.50943             0
## 14001    39 2012-11-18     1440  17.11321            39
## 14002    72 2012-11-18     1445  26.07547            72
## 14003    46 2012-11-18     1450  43.62264            46
## 14004     4 2012-11-18     1455  43.77358             4
## 14005     0 2012-11-18     1500  30.01887             0
## 14006     0 2012-11-18     1505  36.07547             0
## 14007    36 2012-11-18     1510  35.49057            36
## 14008     0 2012-11-18     1515  38.84906             0
## 14009     0 2012-11-18     1520  45.96226             0
## 14010     0 2012-11-18     1525  47.75472             0
## 14011     0 2012-11-18     1530  48.13208             0
## 14012   143 2012-11-18     1535  65.32075           143
## 14013     0 2012-11-18     1540  82.90566             0
## 14014   335 2012-11-18     1545  98.66038           335
## 14015   681 2012-11-18     1550 102.11321           681
```

Now we repeat the analyses using the imputed data:

```r
date_grouped <- group_by(dat, date)
imputed_steps <- summarise_each(date_grouped, funs(sum(.)), imputed_steps)
head(imputed_steps)
```

```
## Source: local data frame [6 x 2]
## 
##         date imputed_steps
## 1 2012-10-01      10766.19
## 2 2012-10-02        126.00
## 3 2012-10-03      11352.00
## 4 2012-10-04      12116.00
## 5 2012-10-05      13294.00
## 6 2012-10-06      15420.00
```

```r
hist(imputed_steps$imputed_steps, main = 'Histogram of Steps per Day', xlab = 'Number of Steps', ylim=c(0,25), col='steelblue', breaks=10)
```

![plot of chunk plot_histogram_2](figure/plot_histogram_2-1.png) 

```r
mean(imputed_steps$imputed_steps)
```

```
## [1] 10766.19
```

```r
median(imputed_steps$imputed_steps)
```

```
## [1] 10766.19
```

Are the values for the mean and median with the missing values filled-in the same as the previous calculations?

```r
est_mean == mean(imputed_steps$imputed_steps)
```

```
## [1] FALSE
```

```r
est_median == median(imputed_steps$imputed_steps)
```

```
## [1] FALSE
```
No, the values are different. In this case, replacing the missing data has resulted in step counts that seem to be more normally distributed around the mean.

###Weekday Activity vs Weekend Activity
Now we examine whether activity patterns are different on weekends versus weekdays. We add a factor variable and a day of week column to visually verify the factor.

```r
weekends <- c('Saturday', 'Sunday')
dat$dow <- weekdays(dat$date)
dat$day <- factor((weekdays(dat$date) %in% weekends), levels=c(TRUE, FALSE), labels=c('weekend', 'weekday'))
interval_grouped <- group_by(dat, interval, day)
interval_steps <- summarise_each(interval_grouped, funs(mean(.)), imputed_steps)
library(lattice)
xyplot(interval_steps$imputed_steps ~ interval_steps$interval | day, data=interval_steps, layout=c(1,2), main="Mean Steps for 5-Minute Intervals for Weekdays vs Weekends", xlab="Interval", ylab="Number of Steps", type=c("l","l"))
```

![plot of chunk plot_day_of_week](figure/plot_day_of_week-1.png) 

The Plots show that the weekday step count is highest earlier in the day and the weekend step count is more evenly distributed throughout the day.
