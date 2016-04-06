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
g<-ggplot(totalstep, aes(x=x))
g+geom_histogram(binwidth=1000)+xlab("steps per day")+ylab("frequency")
mean_steps <- mean(totalstep$x, na.rm = TRUE)
median_steps<-median(totalstep$x, na.rm=TRUE)
```

-----
##What is the average daily activity pattern?
-----

```{r}
activity_n<-subset(activity, activity$steps!="NA")
steps_ave<-aggregate(activity_n$steps, by=list(activity_n$interval), FUN=mean)
g<-ggplot(steps_ave, aes(x=Group.1, y=x))
g+geom_line()+xlab("interval")+ylab("mean steps")
```
------
##Imputing missing values
------

```{r}
activity.replaceNA<- activity %>% group_by(interval)  %>% mutate(steps= ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
head(activity.replaceNA)
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
dataweekend<-subset(data, data$day=="Weekend")
dataweekday<-subset(data, data$day=="Weekday")
datanew<-aggregate(dataweekday$steps, by=list(dataweekday$date), FUN=sum)
datanew2<-aggregate(dataweekend$steps, by=list(dataweekend$date), FUN=sum)
g<-ggplot(datanew, aes(x=Group.1, y=x))
g+geom_line()+xlab("interval")+ylab("mean steps")
g<-ggplot(datanew2, aes(x=Group.1, y=x))
g+geom_line()+xlab("interval")+ylab("weekday mean steps")
```



