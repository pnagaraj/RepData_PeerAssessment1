# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library("knitr")
require('knitr')
opts_chunk$set(echo=TRUE,results=TRUE)
indata<-read.csv("activity.csv")
# check summary of the data
head(indata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(indata)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
colnames(indata)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(indata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
## Calculate mean total number of steps taken per day

```r
indata<-subset(indata,!(is.na(indata$steps)))
                                 
y<-as.Date(indata$date,"%Y-%m-%d")
# average per day
aggtotal<-aggregate(indata$steps,by=list(y),FUN=sum)
hist(as.vector(aggtotal[,2]),breaks=10,main="Histogram of total steps per day",xlab="Total steps per day", ylab="Frequency")
```

![](PA1_template_files/figure-html/meantotal-1.png) 

```r
#Mean and median steps per day

aggmeanday<-aggregate(indata$steps,by=list(y),FUN=mean)
aggmedianday<-aggregate(indata$steps,by=list(y),FUN=median)
names(aggmeanday)<-c("Date","Mean_steps")
names(aggmedianday)<-c("Date","Median_steps")
aggday<-merge(aggmeanday,aggmedianday,by="Date")

# mean steps per interval
agginterval<-aggregate(indata$steps,by=list(indata$interval),FUN=mean)
```

## What is mean total number of steps taken per day?
Mean and Median total number of steps per day is listed below;

```r
print(aggday,row.names=FALSE)
```

```
##        Date Mean_steps Median_steps
##  2012-10-02  0.4375000            0
##  2012-10-03 39.4166667            0
##  2012-10-04 42.0694444            0
##  2012-10-05 46.1597222            0
##  2012-10-06 53.5416667            0
##  2012-10-07 38.2465278            0
##  2012-10-09 44.4826389            0
##  2012-10-10 34.3750000            0
##  2012-10-11 35.7777778            0
##  2012-10-12 60.3541667            0
##  2012-10-13 43.1458333            0
##  2012-10-14 52.4236111            0
##  2012-10-15 35.2048611            0
##  2012-10-16 52.3750000            0
##  2012-10-17 46.7083333            0
##  2012-10-18 34.9166667            0
##  2012-10-19 41.0729167            0
##  2012-10-20 36.0937500            0
##  2012-10-21 30.6284722            0
##  2012-10-22 46.7361111            0
##  2012-10-23 30.9652778            0
##  2012-10-24 29.0104167            0
##  2012-10-25  8.6527778            0
##  2012-10-26 23.5347222            0
##  2012-10-27 35.1354167            0
##  2012-10-28 39.7847222            0
##  2012-10-29 17.4236111            0
##  2012-10-30 34.0937500            0
##  2012-10-31 53.5208333            0
##  2012-11-02 36.8055556            0
##  2012-11-03 36.7048611            0
##  2012-11-05 36.2465278            0
##  2012-11-06 28.9375000            0
##  2012-11-07 44.7326389            0
##  2012-11-08 11.1770833            0
##  2012-11-11 43.7777778            0
##  2012-11-12 37.3784722            0
##  2012-11-13 25.4722222            0
##  2012-11-15  0.1423611            0
##  2012-11-16 18.8923611            0
##  2012-11-17 49.7881944            0
##  2012-11-18 52.4652778            0
##  2012-11-19 30.6979167            0
##  2012-11-20 15.5277778            0
##  2012-11-21 44.3993056            0
##  2012-11-22 70.9270833            0
##  2012-11-23 73.5902778            0
##  2012-11-24 50.2708333            0
##  2012-11-25 41.0902778            0
##  2012-11-26 38.7569444            0
##  2012-11-27 47.3819444            0
##  2012-11-28 35.3576389            0
##  2012-11-29 24.4687500            0
```

## What is the average daily activity pattern?


```r
# average daily activity vs interval
agginterval<-aggregate(indata$steps,by=list(indata$interval),FUN=mean)
plot(agginterval[,1],agginterval[,2],type="l",main="Average daily activity pattern",xlab="five minute interval",ylab="Average steps")
```

![](PA1_template_files/figure-html/pattern-1.png) 

```r
index<-which.max(agginterval[,2])
```
The average daily activity pattern is maximum during the interval 835.

## Imputing missing values

```r
indatafull<-read.csv("activity.csv")
a<-is.na(indatafull$steps)
nimpute<-sum(a)
#fill missing values by means
indatafull$steps[is.na(indatafull$steps)]<-mean(indatafull$steps,na.rm=TRUE)
# average per day
yall<-as.Date(indatafull$date,"%Y-%m-%d")
aggtotalall<-aggregate(indatafull$steps,by=list(yall),FUN=sum)
hist(as.vector(aggtotalall[,2]),breaks=10,main="Histogram of total steps per day after Imputing",xlab="Total steps per day", ylab="Frequency")
```

![](PA1_template_files/figure-html/impute-1.png) 

```r
str(aggtotalall)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ Group.1: Date, format: "2012-10-01" "2012-10-02" ...
##  $ x      : num  10766 126 11352 12116 13294 ...
```

```r
aggallmeanday<-aggregate(indatafull$steps,by=list(yall),FUN=mean)
aggallmedianday<-aggregate(indatafull$steps,by=list(yall),FUN=median)
names(aggallmeanday)<-c("Date","Mean steps per day")
names(aggallmedianday)<-c("Date","Median steps per day")
aggallday<-merge(aggallmeanday,aggallmedianday,by="Date")
```
The number of rows with data missing on steps are 2304.
After imputing, the mean and the median of the total number of steps per day is  shown below;

```r
print(aggallday,rownames="FALSE")
```

```
##          Date Mean steps per day Median steps per day
## 1  2012-10-01         37.3825996              37.3826
## 2  2012-10-02          0.4375000               0.0000
## 3  2012-10-03         39.4166667               0.0000
## 4  2012-10-04         42.0694444               0.0000
## 5  2012-10-05         46.1597222               0.0000
## 6  2012-10-06         53.5416667               0.0000
## 7  2012-10-07         38.2465278               0.0000
## 8  2012-10-08         37.3825996              37.3826
## 9  2012-10-09         44.4826389               0.0000
## 10 2012-10-10         34.3750000               0.0000
## 11 2012-10-11         35.7777778               0.0000
## 12 2012-10-12         60.3541667               0.0000
## 13 2012-10-13         43.1458333               0.0000
## 14 2012-10-14         52.4236111               0.0000
## 15 2012-10-15         35.2048611               0.0000
## 16 2012-10-16         52.3750000               0.0000
## 17 2012-10-17         46.7083333               0.0000
## 18 2012-10-18         34.9166667               0.0000
## 19 2012-10-19         41.0729167               0.0000
## 20 2012-10-20         36.0937500               0.0000
## 21 2012-10-21         30.6284722               0.0000
## 22 2012-10-22         46.7361111               0.0000
## 23 2012-10-23         30.9652778               0.0000
## 24 2012-10-24         29.0104167               0.0000
## 25 2012-10-25          8.6527778               0.0000
## 26 2012-10-26         23.5347222               0.0000
## 27 2012-10-27         35.1354167               0.0000
## 28 2012-10-28         39.7847222               0.0000
## 29 2012-10-29         17.4236111               0.0000
## 30 2012-10-30         34.0937500               0.0000
## 31 2012-10-31         53.5208333               0.0000
## 32 2012-11-01         37.3825996              37.3826
## 33 2012-11-02         36.8055556               0.0000
## 34 2012-11-03         36.7048611               0.0000
## 35 2012-11-04         37.3825996              37.3826
## 36 2012-11-05         36.2465278               0.0000
## 37 2012-11-06         28.9375000               0.0000
## 38 2012-11-07         44.7326389               0.0000
## 39 2012-11-08         11.1770833               0.0000
## 40 2012-11-09         37.3825996              37.3826
## 41 2012-11-10         37.3825996              37.3826
## 42 2012-11-11         43.7777778               0.0000
## 43 2012-11-12         37.3784722               0.0000
## 44 2012-11-13         25.4722222               0.0000
## 45 2012-11-14         37.3825996              37.3826
## 46 2012-11-15          0.1423611               0.0000
## 47 2012-11-16         18.8923611               0.0000
## 48 2012-11-17         49.7881944               0.0000
## 49 2012-11-18         52.4652778               0.0000
## 50 2012-11-19         30.6979167               0.0000
## 51 2012-11-20         15.5277778               0.0000
## 52 2012-11-21         44.3993056               0.0000
## 53 2012-11-22         70.9270833               0.0000
## 54 2012-11-23         73.5902778               0.0000
## 55 2012-11-24         50.2708333               0.0000
## 56 2012-11-25         41.0902778               0.0000
## 57 2012-11-26         38.7569444               0.0000
## 58 2012-11-27         47.3819444               0.0000
## 59 2012-11-28         35.3576389               0.0000
## 60 2012-11-29         24.4687500               0.0000
## 61 2012-11-30         37.3825996              37.3826
```


Imputing the missing values, the median steps per day was non-zero for some days. Before imputing, the median steps per day was zero for all days. The mean values did not vary significantly after imputing. 

## Are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)
# get weekday information
indatafull$day<-weekdays(as.Date(indatafull$date,"%Y-%m-%d"))
#indatafull$period<-ifelse(indatafull$day=="Saturday" | indatafull$day=="Sunday","Weekend","Weekday")
# create factor variable using Weekday information
indatafull$period<-factor(indatafull$day=="Saturday" | indatafull$day=="Sunday")
levels(indatafull$period)[levels(indatafull$period)=="TRUE"]<-"Weekend"
levels(indatafull$period)[levels(indatafull$period)=="FALSE"]<-"Weekday"
# check the new structure of the imputed data after adding new column
str(indatafull)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : chr  "Monday" "Monday" "Monday" "Monday" ...
##  $ period  : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
# compute average activity per interval
aggperiod<-aggregate(indatafull$steps,by=list(indatafull$interval,indatafull$period),FUN=mean)
str(aggperiod)
```

```
## 'data.frame':	576 obs. of  3 variables:
##  $ Group.1: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ Group.2: Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  $ x      : num  7.01 5.38 5.14 5.16 5.07 ...
```

```r
#plot
qplot(Group.1,x,data=aggperiod,facets=.~Group.2,geom=c("line","smooth"),method="lm",main="Comparison of activity over Weekend and Weekdays",xlab="Interval",ylab="Average steps")
```

![](PA1_template_files/figure-html/days-1.png) 

Looking at the regression fit lines, the average activity over the weekend appears to be higher than that over the weekdays. The time-series plot during the weekdays shows that the average activity during the early intervals is higher than the later intervals. The time-series plot during the weekends, suggest that the average activity remains similar throughout the day.
