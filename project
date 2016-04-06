#Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#1.Loading and preprocessing the data

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile="./Desktop/data.zip", method="curl")
unzip("data.zip")
activity <- read.csv("activity.csv",quote="\"")

#2. Process the data
activity$date<-as.POSIXct(activity$date, "%Y-%M-%D")
activity$steps<-as.numeric(activity$steps)
head(activity)

  steps       date interval
1    NA 2012-10-01        0
2    NA 2012-10-01        5
3    NA 2012-10-01       10
4    NA 2012-10-01       15
5    NA 2012-10-01       20
6    NA 2012-10-01       25
#3 Data
#3.1Sum steps by day, create Histogram, and calculate mean and median.
totalstep<-aggregate(activity$steps, na.rm=T, by=list(activity$date), FUN=sum)
totalstep<-subset(totalstep, totalstep$x!="0")
g<-ggplot(totalstep, aes(x=x))
g+geom_histogram(binwidth=1000)+xlab("steps per day")+ylab("frequency")
mean_steps <- mean(totalstep$x, na.rm = TRUE)
mean_steps
10766.19
median_steps<-median(totalstep$x, na.rm=TRUE)
median_steps
10765

#4 What is the average daily activity pattern?
activity_n<-subset(activity, activity$steps!="NA")
steps_ave<-aggregate(activity_n$steps, by=list(activity_n$interval), FUN=mean)
head(steps_ave)
  Group.1         x
1       0 1.7169811
2       5 0.3396226
3      10 0.1320755
4      15 0.1509434
5      20 0.0754717
6      25 2.0943396

g<-ggplot(steps_ave, aes(x=Group.1, y=x))
g+geom_line()+xlab("interval")+ylab("mean steps")

#5 Imputing missing values
activity.replaceNA<- activity %>% group_by(interval)  %>% mutate(steps= ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
head(activity.replaceNA)
   steps       date interval
      (dbl)     (fctr)    (int)
1 1.7169811 2012-10-01        0
2 0.3396226 2012-10-01        5
3 0.1320755 2012-10-01       10
4 0.1509434 2012-10-01       15
5 0.0754717 2012-10-01       20
6 2.0943396 2012-10-01       25

totalstep<-aggregate(activity.replaceNA$steps, na.rm=T, by=list(activity.replaceNA$date), FUN=sum)
head(totalstep)
     Group.1        x
1 2012-10-01 10766.19
2 2012-10-02   126.00
3 2012-10-03 11352.00
4 2012-10-04 12116.00
5 2012-10-05 13294.00
6 2012-10-06 15420.00
g<-ggplot(totalstep, aes(x))
g+geom_histogram()+xlab("steps per day")+ylab("Frequency")

#Are there differences in activity patterns between weekdays and weekends?
>activitynew<-activity.replaceNA
> notweekend<-weekdays %in% c("Monday", "Tuesday", "Wednesday","Thursday","Friday")
> weekdays[notweekend]<-"Weekday"
> Sat<-weekdays=="Saturday"
> weekdays[Sat]<-"Weekend"
> Sun<-weekdays=="Sunday"
> weekdays[Sun]<-"Weekend"
> data<-cbind(activitynew, day=weekdays)
> head(data)
     steps       date interval     day
1 1.7169811 2012-10-01        0 Weekday
2 0.3396226 2012-10-01        5 Weekday
3 0.1320755 2012-10-01       10 Weekday
4 0.1509434 2012-10-01       15 Weekday
5 0.0754717 2012-10-01       20 Weekday
6 2.0943396 2012-10-01       25 Weekday
dataweekend<-subset(data, data$day=="Weekend")
dataweekday<-subset(data, data$day=="Weekday")
datanew<-aggregate(dataweekday$steps, by=list(dataweekday$date), FUN=sum)
datanew2<-aggregate(dataweekend$steps, by=list(dataweekend$date), FUN=sum)
g<-ggplot(datanew, aes(x=Group.1, y=x))
g+geom_line()+xlab("interval")+ylab("mean steps")
g<-ggplot(datanew2, aes(x=Group.1, y=x))
g+geom_line()+xlab("interval")+ylab("weekday mean steps")
