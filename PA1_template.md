# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First, loading some libraries here.

```r
library(dplyr)
library(tidyr)
```
Reading the data, creating a data frame table

```r
act <- read.csv("activity.csv")
act <- tbl_df(act)
act
```

```
## Source: local data frame [17,568 x 3]
## 
##    steps       date interval
##    (int)     (fctr)    (int)
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## ..   ...        ...      ...
```

Preprocessing: group data by Date and then summarize, to show the total of steps per day.

```r
act_date <- act %>%
  group_by(date) %>%
  summarize(steps_day=sum(steps))

act_date
```

```
## Source: local data frame [61 x 2]
## 
##          date steps_day
##        (fctr)     (int)
## 1  2012-10-01        NA
## 2  2012-10-02       126
## 3  2012-10-03     11352
## 4  2012-10-04     12116
## 5  2012-10-05     13294
## 6  2012-10-06     15420
## 7  2012-10-07     11015
## 8  2012-10-08        NA
## 9  2012-10-09     12811
## 10 2012-10-10      9900
## ..        ...       ...
```

Preprocessing: Removing NA's


```r
act2 <- act_date %>%
  filter(!is.na(steps_day)) ## Remove NA values
 
act2  # tbl without NA values
```

```
## Source: local data frame [53 x 2]
## 
##          date steps_day
##        (fctr)     (int)
## 1  2012-10-02       126
## 2  2012-10-03     11352
## 3  2012-10-04     12116
## 4  2012-10-05     13294
## 5  2012-10-06     15420
## 6  2012-10-07     11015
## 7  2012-10-09     12811
## 8  2012-10-10      9900
## 9  2012-10-11     10304
## 10 2012-10-12     17382
## ..        ...       ...
```

## What is mean total number of steps taken per day?

Do the maths and then the histogram as indicated.

```r
mean(act2$steps_day)
```

```
## [1] 10766.19
```

```r
median(act2$steps_day)
```

```
## [1] 10765
```

```r
hist(act2$steps_day, xlab="Steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 



## What is the average daily activity pattern?

Now grouping the data by intervals, and then summarizing.

```r
act_int <- act %>%
  group_by(interval) %>%
  summarize(mean_steps=mean(steps, na.rm=TRUE))
  
plot(act_int$interval,act_int$mean_steps,type="l", col="red" )
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
filter(act_int, mean_steps==max(mean_steps))
```

```
## Source: local data frame [1 x 2]
## 
##   interval mean_steps
##      (int)      (dbl)
## 1      835   206.1698
```


## Imputing missing values

I'm going to use the mean steps per interval, as calculated in the last question.


```r
act_na <- act %>%
  filter(is.na(steps))
act_nona <- act %>%
  filter(!is.na(steps))

act_na
```

```
## Source: local data frame [2,304 x 3]
## 
##    steps       date interval
##    (int)     (fctr)    (int)
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## ..   ...        ...      ...
```

```r
act_int2 <- act_int %>%
  mutate(steps=as.integer(floor(mean_steps))) %>%
  select(interval,steps)

a <- complete(act_na, fill=act_int2, by=steps)

act3 <- rbind(act_nona, a)
act3 <- arrange(act3,date,interval)

act3 <- act3 %>%
  group_by(date) %>%
  summarize(steps_day=sum(steps))

hist(act3$steps_day, xlab="Steps taken per day",main="Imputing missing values")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 


```r
mean(act3$steps_day)
```

```
## [1] 10749.77
```

```r
median(act3$steps_day)
```

```
## [1] 10641
```
There is a little difference between these calculations and the numbers that I've got before (without completing the dataset) but it seems to be minimal, almost the same.

## Are there differences in activity patterns between weekdays and weekends?

Loading some libraries.

```r
library(lubridate)
library(lattice)
```

Doing the graph.

```r
act_days <- act %>%
  filter(!is.na(steps)) %>%
  mutate(Day_of_the_week=as.integer(wday(ymd(date)))) %>%
  mutate(Weekend = ifelse(Day_of_the_week %in% c(6,7), "Weekend", "Weekday"))%>%
  group_by(interval,Weekend) %>%
  mutate(mean_steps=mean(steps, na.rm=TRUE)) %>%
  select(steps,date,Weekend,interval,mean_steps)

act_days
```

```
## Source: local data frame [15,264 x 5]
## Groups: interval, Weekend [576]
## 
##    steps       date Weekend interval mean_steps
##    (int)     (fctr)   (chr)    (int)      (dbl)
## 1      0 2012-10-02 Weekday        0  2.3333333
## 2      0 2012-10-02 Weekday        5  0.4615385
## 3      0 2012-10-02 Weekday       10  0.1794872
## 4      0 2012-10-02 Weekday       15  0.2051282
## 5      0 2012-10-02 Weekday       20  0.1025641
## 6      0 2012-10-02 Weekday       25  2.8461538
## 7      0 2012-10-02 Weekday       30  0.7179487
## 8      0 2012-10-02 Weekday       35  1.1794872
## 9      0 2012-10-02 Weekday       40  0.0000000
## 10     0 2012-10-02 Weekday       45  1.8461538
## ..   ...        ...     ...      ...        ...
```

```r
xyplot(mean_steps ~ interval | Weekend, act_days, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 
