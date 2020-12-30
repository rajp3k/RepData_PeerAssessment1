---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
activity <- read.csv(paste("repdata_data_activity","activity.csv",sep = "/"))
```

## What is mean total number of steps taken per day?


```r
steps_per_day <- as.data.frame(with(activity,tapply(steps,date,sum,na.rm = TRUE)))

colnames(steps_per_day) <- "number_of_steps"

print(steps_per_day)
```

```
##            number_of_steps
## 2012-10-01               0
## 2012-10-02             126
## 2012-10-03           11352
## 2012-10-04           12116
## 2012-10-05           13294
## 2012-10-06           15420
## 2012-10-07           11015
## 2012-10-08               0
## 2012-10-09           12811
## 2012-10-10            9900
## 2012-10-11           10304
## 2012-10-12           17382
## 2012-10-13           12426
## 2012-10-14           15098
## 2012-10-15           10139
## 2012-10-16           15084
## 2012-10-17           13452
## 2012-10-18           10056
## 2012-10-19           11829
## 2012-10-20           10395
## 2012-10-21            8821
## 2012-10-22           13460
## 2012-10-23            8918
## 2012-10-24            8355
## 2012-10-25            2492
## 2012-10-26            6778
## 2012-10-27           10119
## 2012-10-28           11458
## 2012-10-29            5018
## 2012-10-30            9819
## 2012-10-31           15414
## 2012-11-01               0
## 2012-11-02           10600
## 2012-11-03           10571
## 2012-11-04               0
## 2012-11-05           10439
## 2012-11-06            8334
## 2012-11-07           12883
## 2012-11-08            3219
## 2012-11-09               0
## 2012-11-10               0
## 2012-11-11           12608
## 2012-11-12           10765
## 2012-11-13            7336
## 2012-11-14               0
## 2012-11-15              41
## 2012-11-16            5441
## 2012-11-17           14339
## 2012-11-18           15110
## 2012-11-19            8841
## 2012-11-20            4472
## 2012-11-21           12787
## 2012-11-22           20427
## 2012-11-23           21194
## 2012-11-24           14478
## 2012-11-25           11834
## 2012-11-26           11162
## 2012-11-27           13646
## 2012-11-28           10183
## 2012-11-29            7047
## 2012-11-30               0
```

```r
hist(steps_per_day[ ,1],breaks = 61,xlab = "number_of_steps")
```

![](RepData_PeerAssessment1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean_per_day <- mean(steps_per_day[ ,1])
print(mean_per_day)
```

```
## [1] 9354.23
```

```r
median_per_day <- median(steps_per_day[ ,1])
print(median_per_day)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
steps_per_interval <- as.data.frame(with(activity, tapply(steps,interval,mean,na.rm = TRUE)))

interval <- rownames(steps_per_interval)

plot(interval,steps_per_interval[ ,1],type = "l",ylab = "number_of_steps")
```

![](RepData_PeerAssessment1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
index_max_steps <- which.max(steps_per_interval[ ,1])

print(interval[index_max_steps])
```

```
## [1] "835"
```

## Imputing missing values


```r
number_na <- sum(is.na(activity))
print(number_na)
```

```
## [1] 2304
```

```r
mean_for_interval <- round(mean(activity[ ,1],na.rm = TRUE))

activity_no_na <- activity

for (i in 1:nrow(activity_no_na)) {
        if (is.na(activity_no_na[i,1])) {
                activity_no_na[i,1] = mean_for_interval
        }
}

steps_per_day_no_na <- as.data.frame(with(activity_no_na,tapply(steps,date,sum)))

hist(steps_per_day_no_na[ ,1],breaks = 61,xlab = "number_of_steps")
```

![](RepData_PeerAssessment1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean_per_day_no_na <- mean(steps_per_day_no_na[ ,1])

print(mean_per_day_no_na)
```

```
## [1] 10751.74
```

```r
median_per_day_no_na <- median(steps_per_day_no_na[ ,1])

print(median_per_day_no_na)
```

```
## [1] 10656
```

## Are there differences in activity patterns between weekdays and weekends?


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.2
```

```
## 
## Attaching package: 'dplyr'
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
library(lattice)

activity_no_na$date <- as.Date(activity_no_na$date)

activity_no_na$day <- weekdays(activity_no_na$date)

weekend <- c("Saturday","Sunday")

activity_no_na$weekend <- factor((activity_no_na$day %in% weekend),levels=c(FALSE,TRUE),labels = c("weekday","weekend"))

activity_ds <- activity_no_na %>%
        group_by(weekend,interval) %>%
        summarize_at(vars(steps),mean)

xyplot(activity_ds$steps ~ activity_ds$interval | activity_ds$weekend,type = "l",xlab = "Interval",ylab = "Number of steps",layout = c(1,2))
```

![](RepData_PeerAssessment1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
