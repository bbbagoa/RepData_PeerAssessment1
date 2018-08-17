---
title: 'Reproducible Research: Peer Assessment 1'
author: "Brice Baem BAGOA"
date: "July 11, 2018"
output:
  html_document:
    keep_md: yes
  word_document: default
subtitle: Peer-graded Assignment
---



---

# Loading and preprocessing the data

#### 1.1 Load the data


```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1.9000     v purrr   0.2.5     
## v tibble  1.4.2          v dplyr   0.7.5     
## v tidyr   0.8.1          v stringr 1.3.1     
## v readr   1.1.1          v forcats 0.3.0
```

```
## -- Conflicts ------------------------------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
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

```r
head(activity)
```

```
## # A tibble: 6 x 3
##   steps date       interval
##   <int> <date>        <int>
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Our data variable is in a `string` format. We have to convert it form `string` to `Date` format. We also need to remove all the `NA` onservations from the data because we will not need them for the computing 

---

#### 1.2 Processing and cleaning the data


```r
## -- transforming data from string to data type
activity <- mutate(activity, dateT = as.Date(date, "%Y-%m-%d"))

## -- Deleting the data with the NAs
noNAactivity <- filter(activity, !is.na(steps))
```
---

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
stepsBYday <- noNAactivity %>% 
  group_by(dateT)%>%
  summarize(sumSteps = sum(steps))

head(stepsBYday)
```

```
## # A tibble: 6 x 2
##   dateT      sumSteps
##   <date>        <int>
## 1 2012-10-02      126
## 2 2012-10-03    11352
## 3 2012-10-04    12116
## 4 2012-10-05    13294
## 5 2012-10-06    15420
## 6 2012-10-07    11015
```
---

2. Make a histogram of the total number of steps taken each day


```r
qplot(stepsBYday$sumSteps, geom = "histogram",
      bins = 2000,
      main = "Total of steps by day",
      xlab = "Steps",
      color = I("black"),
      fill = I("magenta"))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
---

3. Calculate and report the mean and median of the total number of steps taken per day


```r
steps_mean <- mean(stepsBYday$sumSteps)
steps_median <- median(stepsBYday$sumSteps)
steps_mean
```

```
## [1] 10766.19
```

```r
steps_median
```

```
## [1] 10765
```
---

## What is the average daily activity pattern?

1 . Make a time series plot (i.e. `type = "l"`) of the 5-minute interval `(x-axis)` and the average number of steps taken, averaged across all days `(y-axis)`.


```r
## have a data with 5 min intervall means of steps
int5.data <- noNAactivity %>%
  group_by(interval) %>%
  summarize(sumSteps = sum(steps))

head(int5.data)
```

```
## # A tibble: 6 x 2
##   interval sumSteps
##      <int>    <int>
## 1        0       91
## 2        5       18
## 3       10        7
## 4       15        8
## 5       20        4
## 6       25      111
```

```r
qplot(x = int5.data$interval,
      y = int5.data$sumSteps,
      geom = "line",
      xlab = "5-minute Intervals",
      ylab = "Average Steps Taken by days",
      main = "Average Daily Activity Pattern",
      color = I("blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
---

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
filter(int5.data, sumSteps==max(sumSteps))
```

```
## # A tibble: 1 x 2
##   interval sumSteps
##      <int>    <int>
## 1      835    10927
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

`The strategy is to replace missing values by the mean of that day and replace the remain missing values by the mean of the 5 min interval`


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# function to fill the NA values with mean
fillMean <- function(data) replace(data, is.na(data), mean(data, na.rm = TRUE))

# fill tne NAs with the mean of each day
activity.fillSteps <- activity %>%
    group_by(date) %>%
    mutate(
        steps = fillMean(steps)
    )
sum(is.na(activity.fillSteps$steps))
```

```
## [1] 2304
```

```r
# fill the remain NAs with thea mean of each 5 min intervall
activity.fill <- activity.fillSteps %>%
  group_by(interval) %>%
  mutate(steps = fillMean(steps))


sum(is.na(activity.fill$steps))
```

```
## [1] 0
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Recalculating the number of steps for each day
stepsBYday2 <- activity.fill %>% 
  group_by(dateT)%>%
  summarize(sumSteps = sum(steps))

# Histogram Plotting
qplot(stepsBYday2$sumSteps, geom = "histogram",
      bins = 2000,
      main = "Total of steps by day (NAs Filled Data)",
      xlab = "Steps",
      color = I("black"),
      fill = I("magenta"))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
# Calclate the mean and median of steps
mean(stepsBYday2$sumSteps)
```

```
## [1] 10766.19
```

```r
median(stepsBYday2$sumSteps)
```

```
## [1] 10766.19
```


These values are different from the first computed values as NAs values was filled.

---


## Are there differences in #activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - `weekday` and `weekend` indicating whether a given date is a weekday or weekend day.


```r
# Creating a column for Type of days
activity.fill <- activity.fill %>%
  mutate(weekDay = as.POSIXlt(dateT)$wday) %>%
  mutate(weekDayType = as.factor(ifelse(weekDay == 0 | weekDay == 6, "weekend", "weekday")))

head(activity.fill)
```

```
## # A tibble: 6 x 6
## # Groups:   interval [6]
##    steps date       interval dateT      weekDay weekDayType
##    <dbl> <date>        <int> <date>       <int> <fct>      
## 1 1.72   2012-10-01        0 2012-10-01       1 weekday    
## 2 0.340  2012-10-01        5 2012-10-01       1 weekday    
## 3 0.132  2012-10-01       10 2012-10-01       1 weekday    
## 4 0.151  2012-10-01       15 2012-10-01       1 weekday    
## 5 0.0755 2012-10-01       20 2012-10-01       1 weekday    
## 6 2.09   2012-10-01       25 2012-10-01       1 weekday
```


2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
int5.data2 <- activity.fill %>%
  group_by_(.dots = c("interval", "weekDayType")) %>%
  mutate(meanSteps = mean(steps))

head(int5.data2)
```

```
## # A tibble: 6 x 7
## # Groups:   interval, weekDayType [6]
##    steps date       interval dateT      weekDay weekDayType meanSteps
##    <dbl> <date>        <int> <date>       <int> <fct>           <dbl>
## 1 1.72   2012-10-01        0 2012-10-01       1 weekday        2.25  
## 2 0.340  2012-10-01        5 2012-10-01       1 weekday        0.445 
## 3 0.132  2012-10-01       10 2012-10-01       1 weekday        0.173 
## 4 0.151  2012-10-01       15 2012-10-01       1 weekday        0.198 
## 5 0.0755 2012-10-01       20 2012-10-01       1 weekday        0.0990
## 6 2.09   2012-10-01       25 2012-10-01       1 weekday        1.59
```

```r
ggplot(data = int5.data2, aes(interval, meanSteps)) + 
  geom_line(color="blue") + 
  facet_grid(int5.data2$weekDayType ~ .) 
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
