---
title: "Project 1"
author: "lucy"
date: "April 5, 2016"
output: html_document
---
Loading & Preprocessing Data,    What is mean total number of steps taken per day?
##mean_steps=10766.19,  median_steps=10765
-------
```{r}
library(ggplot2)
library(plyr)
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile="./Desktop/data.zip", method="curl")
unzip("data.zip")
activity <- read.csv("activity.csv",quote="\"")
activity$date<-as.POSIXct(activity$date, "%Y-%M-%D")
activity$steps<-as.numeric(activity$steps)

totalstep<-aggregate(activity$steps, na.rm=T, by=list(activity$date), FUN=sum)
totalstep<-subset(totalstep, totalstep$x!="0")

##make the plot
g<-ggplot(totalstep, aes(x=x))
g+geom_histogram(binwidth=1000)+xlab("steps per day")+ylab("frequency")
mean_steps <- mean(totalstep$x, na.rm = TRUE)
median_steps<-median(totalstep$x, na.rm=TRUE)
```

-----
##What is the average daily activity pattern? Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? Answer:835th interval
-----

```{r}
activity_n<-subset(activity, activity$steps!="NA")
steps_ave<-aggregate(activity_n$steps, by=list(activity_n$interval), FUN=mean)

##make the plot
g<-ggplot(steps_ave, aes(x=Group.1, y=x))
g+geom_line()+xlab("interval")+ylab("mean steps")
steps_ave[which.max(steps_ave$steps), ]$interval
```
------
##Imputing missing values
------

```{r}
activity.replaceNA<- activity %>% group_by(interval)  %>% mutate(steps= ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))

head(activity)

  steps       date interval
1    NA 2012-10-01        0
2    NA 2012-10-01        5
3    NA 2012-10-01       10
4    NA 2012-10-01       15
5    NA 2012-10-01       20
6    NA 2012-10-01       25

head(activity.replaceNA)

 steps       date interval
      (dbl)     (fctr)    (int)
1 1.7169811 2012-10-01        0
2 0.3396226 2012-10-01        5
3 0.1320755 2012-10-01       10
4 0.1509434 2012-10-01       15
5 0.0754717 2012-10-01       20
6 2.0943396 2012-10-01       25
## 2304 NA values were filled.

##make the plot
totalstep<-aggregate(activity.replaceNA$steps, na.rm=T, by=list(activity.replaceNA$date), FUN=sum)
head(totalstep)
g<-ggplot(totalstep, aes(x))
g+geom_histogram()+xlab("steps per day")+ylab("Frequency")


```

---------
##Are there differences in activity patterns between weekdays and weekends?
---------
```{r}
activitynew<-activity.replaceNA
notweekend<-weekdays %in% c("Monday", "Tuesday", "Wednesday","Thursday","Friday")
weekdays[notweekend]<-"Weekday"
Sat<-weekdays=="Saturday"
weekdays[Sat]<-"Weekend"
Sun<-weekdays=="Sunday"
weekdays[Sun]<-"Weekend"
data<-cbind(activitynew, day=weekdays)
head(data)

     steps       date interval     day
1 1.7169811 2012-10-01        0 Weekday
2 0.3396226 2012-10-01        5 Weekday
3 0.1320755 2012-10-01       10 Weekday
4 0.1509434 2012-10-01       15 Weekday
5 0.0754717 2012-10-01       20 Weekday
6 2.0943396 2012-10-01       25 Weekday

##make the plot
dataweekend<-subset(data, data$day=="Weekend")
dataweekday<-subset(data, data$day=="Weekday")
datanew<-aggregate(dataweekday$steps, by=list(dataweekday$date), FUN=sum)
datanew2<-aggregate(dataweekend$steps, by=list(dataweekend$date), FUN=sum)
g<-ggplot(datanew, aes(x=Group.1, y=x))
g+geom_line()+xlab("interval")+ylab("mean steps")
g<-ggplot(datanew2, aes(x=Group.1, y=x))
g+geom_line()+xlab("interval")+ylab("weekday mean steps")
```



