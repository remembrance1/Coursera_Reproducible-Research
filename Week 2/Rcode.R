#loading of data
df <- read.csv("activity.csv")
df$date <- as.Date(df$date, "%Y-%m-%d")
df <- as.data.frame(df)

#Histogram of the total number of steps taken each day
library(ggplot2)
steps <- aggregate(df$steps, by = list(Date = df$date), FUN = sum) #total number of steps by day
colnames(steps) <- c("Date", "Total")
ggplot(na.omit(steps), aes(Date, Total)) + geom_bar(stat = 'identity') 

#What is mean total number of steps taken per day?  
mean(na.omit(steps$Total))
median(na.omit(steps$Total))

#What is the average daily activity pattern?
five_min_steps <- aggregate(steps ~ interval, data = df, FUN =mean)
TimeSeries1 <- ggplot(data = five_min_steps, aes(x = interval, y = steps)) + 
  geom_line() +
  geom_rug(sides='b') +
  xlab("Minutes in a Day") + 
  ylab("Total Number of Steps") +
  ggtitle("Average Number of Steps Taken of the 5-Minute Interval") +
  theme_classic()
print(TimeSeries1)

five_min_steps[which(five_min_steps$steps == max(five_min_steps$steps)),] #finding max value

#imputing missing value
library(naniar)
vis_miss(df) #show missing values
library(dplyr)
replace_with_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meanday <- (df %>% group_by(interval) %>% mutate(steps = replace_with_mean(steps))) #replacing missing values with mean from 5 min interval

new_dataset <- as.data.frame(meanday)
new_steps <- aggregate(new_dataset$steps, by = list(new_dataset$date), FUN = sum)

names(new_steps)[names(new_steps) == "x"] <- "Total"
names(new_steps)[names(new_steps) == "Group.1"] <- "Date"

hist2 <- ggplot(data = new_steps, aes(Total)) + 
  geom_histogram(binwidth = 1500, colour = "white") +
  xlab("Total Number of Steps Taken Each Day") +
  ylab("Count") +
  ggtitle("Histogram of the Total Number of Steps Taken Each Day with New Version Dataset")
print(hist2)

http://www.rpubs.com/Disha_An/Reproducible_Research_Week2_Project 