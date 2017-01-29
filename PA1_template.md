# RepData_PA1
CRK8  
January 7, 2017  

## R Markdown

Loading and preprocessing the data into R Studio:


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", dest="amd.zip", mode="wb")
unzip ("amd.zip")
amd<-read.csv("./activity.csv",colClasses=c("integer","Date","integer"))
```

**What is mean total number of steps taken per day?**

For this part of the assignment, you can ignore the missing values in the dataset.

1) Calculate the total number of steps taken per day
2) If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3) Calculate and report the mean and median of the total number of steps taken per day


```r
spd<-tapply(amd$steps,amd$date,sum)
spd
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

```r
hist(spd,main="Histogram of Steps per Day")
```

![](PA1_template_files/figure-html/total steps-1.png)<!-- -->

```r
dev.off()
```

```
## null device 
##           1
```

```r
mean(spd,na.rm=T)
```

```
## [1] 10766.19
```

```r
median(spd,na.rm=T)
```

```
## [1] 10765
```

**What is the average daily activity pattern?**

1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
ast<-tapply(amd$steps,amd$interval,mean, na.rm=T)
times<- strptime(sprintf("%04d",amd$interval[1:288]),"%H%M")
xlab<-"Time of Day"
ylab<-"Avg steps per 5 min"
plot(times,ast,type = "l",xlab=xlab,ylab=ylab,main="Avg Number of Steps Taken Throughout the Day")
```

![](PA1_template_files/figure-html/daily pattern-1.png)<!-- -->

```r
dev.off()
```

```
## null device 
##           1
```

```r
format(times[which(ast==max(ast))],"%H:%M")
```

```
## [1] "08:35"
```

**Imputing missing values**

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3) Create a new dataset that is equal to the original dataset but with the missing data filled in.
4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
nrow(amd)-sum(complete.cases(amd))
```

```
## [1] 2304
```

```r
amd1<-amd
amd1$steps[is.na(amd1$steps)]<-rep(ast,61)[is.na(amd1$steps)]

spd1<-tapply(amd1$steps,amd1$date,sum)
hist(spd1,main="Histogram of Steps per Day, omitting missing values")
```

![](PA1_template_files/figure-html/missing values-1.png)<!-- -->

```r
dev.off()
```

```
## null device 
##           1
```

```r
mean(spd1)
```

```
## [1] 10766.19
```

```r
median(spd1)
```

```
## [1] 10766.19
```

**Are there differences in activity patterns between weekdays and weekends?**

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
require(backports)
```

```
## Loading required package: backports
```

```
## Warning: package 'backports' was built under R version 3.2.5
```

```
## 
## Attaching package: 'backports'
```

```
## The following object is masked from 'package:base':
## 
##     lengths
```

```r
amd1$day<-as.factor(ifelse(startsWith(weekdays(amd1$date),"S"),"weekend","weekday"))
amd.weekday<-subset(amd1,day=="weekday")
amd.weekend<-subset(amd1,day=="weekend")
      
ast.weekday<-tapply(amd.weekday$steps,amd.weekday$interval,mean, na.rm=T)
ast.weekend<-tapply(amd.weekend$steps,amd.weekend$interval,mean, na.rm=T)
times1<- strptime(sprintf("%04d",amd1$interval[1:288]),"%H%M")

par(mfrow=c(2,1))
xlab<-"Time of Day"
ylab<-"Avg steps per 5 min"
plot(times1,ast.weekday,type = "l",xlab=xlab,ylab=ylab,main="Average Number of Steps on Weekdays")
plot(times1,ast.weekend,type = "l",xlab=xlab,ylab=ylab,main="Average Number of Steps on Weekends")
```

![](PA1_template_files/figure-html/weekends-1.png)<!-- -->

```r
dev.off()
```

```
## null device 
##           1
```
