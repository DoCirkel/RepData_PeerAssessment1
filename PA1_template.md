---
title: "Course Project 1 - Reproducible Research"
author: "D.R. Cirkel"
date: "29 december 2017"
output: 
  html_document: 
    keep_md: yes
---



### Loading and preprocessing the data
Show any code that is needed to

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(readr)
activity <- read_csv("activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
```

### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
library(dplyr)
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
library(tidyr)

activityperday <- activity %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  filter(!is.na(steps)) %>%
  as.data.frame()

head(activityperday)
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
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
library(ggplot2)
g1<- ggplot(activityperday, aes(steps)) + 
  geom_histogram(bins = 30) + 
  ggtitle("Total steps per day")

plot(g1)
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(activityperday$steps)
```

```
## [1] 10766.19
```

```r
median(activityperday$steps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
activityinterval <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarise(steps = mean(steps)) 

ggplot(activityinterval) + 
  geom_line(aes(interval, steps))
```

![](PA1_template_files/figure-html/interval-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxsteps = max(activity$steps, na.rm = TRUE)

activity[which(activity$steps == maxsteps),]$interval
```

```
## [1] 615
```

### Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
table(is.na(activity))
```

```
## 
## FALSE  TRUE 
## 50400  2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
* See next question, mean per interval is used.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity.new<- activity %>% 
  group_by(interval) %>% 
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = T), steps)) %>%
  as.data.frame()

head(activity.new)
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
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
activity.newperday <- activity.new %>%
  group_by(date) %>%
  summarise(steps = sum(steps)) 

library(ggplot2)
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
g2 <- ggplot(activity.newperday, aes(steps)) + 
  geom_histogram(bins = 30) + 
  ggtitle("Total steps per day, where \n NA = mean of corresponding interval")

grid.arrange(g1, g2, ncol = 2)
```

![](PA1_template_files/figure-html/hist + mean, median-1.png)<!-- -->

```r
mean(activity.newperday$steps)
```

```
## [1] 10766.19
```

```r
median(activity.newperday$steps)
```

```
## [1] 10766.19
```
* These values differ from the first part of the exercise. There are more observed values and the median is now equal to the mean. 

### Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activityweek <- activity.new %>%
  mutate(weekdays = weekdays(date)) %>%
  mutate(weekend = ifelse(weekdays %in% c("zaterdag", "zondag"), "weekend", "weekday"))

head(activityweek)
```

```
##       steps       date interval weekdays weekend
## 1 1.7169811 2012-10-01        0  maandag weekday
## 2 0.3396226 2012-10-01        5  maandag weekday
## 3 0.1320755 2012-10-01       10  maandag weekday
## 4 0.1509434 2012-10-01       15  maandag weekday
## 5 0.0754717 2012-10-01       20  maandag weekday
## 6 2.0943396 2012-10-01       25  maandag weekday
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
activityweekmean <- activityweek %>%
  group_by(weekend, interval) %>%
  summarise(steps = mean(steps)) 

ggplot(activityweekmean) + 
  geom_line(aes(interval, steps)) + 
  facet_grid(weekend~.)
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->
