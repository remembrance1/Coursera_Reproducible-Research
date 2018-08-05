Reproducible Research: Peer Assessment 1
================

Loading and preprocessing the data
----------------------------------

``` r
setwd("C:/Users/Javier Ng/Desktop/Coursera/Coursera_Reproducible-Research/Week 2/Old")
df <- read.csv("activity.csv")
df$date <- as.Date(df$date, "%Y-%m-%d")
df <- as.data.frame(df)
```

Histogram of the total number of steps taken each day
-----------------------------------------------------

``` r
library(ggplot2)
steps <- aggregate(df$steps, by = list(Date = df$date), FUN = sum) #total number of steps by day
colnames(steps) <- c("Date", "Total")
ggplot(na.omit(steps), aes(Date, Total)) + geom_bar(stat = 'identity') 
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-2-1.png)

What is mean total number of steps taken per day?
-------------------------------------------------

``` r
mean(na.omit(steps$Total))
```

    ## [1] 10766.19

``` r
median(na.omit(steps$Total))
```

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

``` r
five_min_steps <- aggregate(steps ~ interval, data = df, FUN =mean)
TimeSeries1 <- ggplot(data = five_min_steps, aes(x = interval, y = steps)) + 
  geom_line() +
  geom_rug(sides='b') +
  xlab("Minutes in a Day") + 
  ylab("Total Number of Steps") +
  ggtitle("Average Number of Steps Taken of the 5-Minute Interval") +
  theme_classic()
print(TimeSeries1)
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
five_min_steps[which(five_min_steps$steps == max(five_min_steps$steps)),] #finding max value
```

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
-----------------------

``` r
library(naniar)
```

    ## Warning: package 'naniar' was built under R version 3.4.4

``` r
vis_miss(df) #show missing values
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
replace_with_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meanday <- (df %>% group_by(interval) %>% mutate(steps = replace_with_mean(steps))) #replacing missing values with mean from 5 min interval
```

``` r
new_dataset <- as.data.frame(meanday)
new_steps <- aggregate(new_dataset$steps, by = list(new_dataset$date), FUN = sum)

names(new_steps)[names(new_steps) == "x"] <- "Total"
names(new_steps)[names(new_steps) == "Group.1"] <- "Date"

Timeseries2 <- ggplot(data = new_steps, aes(Total)) + 
  geom_histogram(binwidth = 1500, colour = "white") +
  xlab("Total Number of Steps Taken Each Day") +
  ylab("Count") +
  ggtitle("Histogram of the Total Number of Steps Taken Each Day with New Version Dataset")
print(Timeseries2)
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-6-1.png)

Comparison between the two plots, new data and old data before imputation

``` r
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 3.4.4

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
grid.arrange(TimeSeries1, Timeseries2, ncol = 2)
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-7-1.png)

From the comparison, we can see that the highest count of the new version data is larger than the one we have with NAs. The means of each dataset are same. The medians of each dataset are slightly different.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Identifying weekday/weekend in the dates

``` r
new_dataset$WeekendOrWeekday <- ifelse(weekdays(as.Date(new_dataset$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
head(new_dataset)
```

    ##       steps       date interval WeekendOrWeekday
    ## 1 1.7169811 2012-10-01        0          Weekday
    ## 2 0.3396226 2012-10-01        5          Weekday
    ## 3 0.1320755 2012-10-01       10          Weekday
    ## 4 0.1509434 2012-10-01       15          Weekday
    ## 5 0.0754717 2012-10-01       20          Weekday
    ## 6 2.0943396 2012-10-01       25          Weekday

Plot to show the number of steps' differences between weekday or weekend, based on mean

``` r
new_dataset <- (new_dataset %>% group_by(interval, WeekendOrWeekday) %>% summarise(Mean = mean(steps)))
ggplot(new_dataset, mapping = aes(x = interval, y = Mean)) + geom_line() +
    facet_grid(WeekendOrWeekday ~.) + xlab("Interval") + ylab("Mean of Steps") +
    ggtitle("Comparison of Average Number of Steps in Each Interval")
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-9-1.png)

Observations:

-   People tend to wake up later during the weekends and stay up later
-   Activity during the weekends are higher as most people are not confined by their working hours
