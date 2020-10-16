----------------------------
title: "PA1_template"
author: "Maxwell"
date: "13/10/2020"
output: 
html_document: default
  keep_md: true
  pdf_document: default
  

---------------------------
# Peer-graded Assignment: Course Project 1



# Overall Assignment Steps
1. Code for reading in the dataset and/or processing the data

2.Calculate the total number of steps taken per day 

3.Histogram of the total number of steps taken each day

4. Mean and median number of steps taken each day

5. Calculate the average steps per interval across All days

6. Time series plot of the average number of steps taken

7.The 5-minute interval that, on average, contains the maximum number of steps

8.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA\color{red}{\verb|NA|}NAs)

9. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

10. Create a new dataset that is equal to the original dataset but with the missing data filled in.

11.Histogram of the total number of steps taken each day after missing values are imputed

12.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

13.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

## Loading and preprocessing the data

### Steps 1  - Loading and preprocessing the data
The code bellow download the data, unzip it into a certain directory,and finally processes it in order to transform the date variable from String to Date Object. The final database is called **database1**

```r
# Installing packages and libraries
#install.packages("dplyr")
#library(dplyr)

# Download and unzip
if(!file.exists("./data/activity.csv")){
target<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(target,destfile="./Factivity.zip",method="curl")
unzip(zipfile = "./Factivity.zip",exdir ="./data" )
}

# mapping the desired files
f<-file.path("./data","activity.csv")

# Loading the data
database1<-read.csv(f,header = TRUE)

# Formating Date from string to Datye Obj
database1[,2]<-as.Date(database1[,2],format="%Y-%m-%d")

# checking data structure
summary(database1)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

### Step 2 Calculate the total number of steps taken per day 
Here we should group data by date, and summarise the other columns, using the total(sum).
As result we have a new dataframe called **total_steps_aggregated_by_date**

```r
   total_steps_aggregated_by_date<-aggregate(steps~date,data =database1,sum,na.rm=TRUE )
```

### Step 3. Make a histogram of the total number of steps taken each day
In this step we are going to use the **aggregated_by_date** dataframe in order to produce a histogram of steps taken per day


```r
par(mar=c(5,5,8,2))

h<-hist(total_steps_aggregated_by_date$steps,cex=1.8,col="steelblue",main="",ylab = "frequency",xlab ="steps per day")


 text(h$mids,h$counts,labels=h$counts,cex=0.75, adj=c(0.5, -0.5))
 title(main = " Histogram Total steps per Day",outer = FALSE)
```

![plot of chunk Histogramoftepsperday](figure/Histogramoftepsperday-1.png)

### Step 4. Calculate and report the mean and median of the total number of steps taken per day

The bellow code calculates the **aggregated_by_date$steps** mean and median 


```r
mean(total_steps_aggregated_by_date$steps,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(total_steps_aggregated_by_date$steps,na.rm = TRUE)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

## Step 5. Calculate the average steps per interval across All days

The bellow code groups data by intervals, and summarizes steps using the mean.
As result we have the **average_steps_aggregated_by_intervals** dataframe  


```r
average_steps_aggregated_by_intervals<-aggregate(steps~interval,database1,mean,na.rm=TRUE)
```

## step 6.Make a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

The graph bellow was made from the **average_steps_aggregated_by_intervals** dataframe  


```r
plot(average_steps_aggregated_by_intervals$interval,average_steps_aggregated_by_intervals$steps,type="l",col="darkblue",ylab = "Average steps per interval",xlab =" Interval")
title(main="Average daily activity pattern")
```

![plot of chunk time_series1](figure/time_series1-1.png)


## step 7 The 5-minute interval that, on average, contains the maximum number of steps
Once again I used the **average_steps_aggregated_by_intervals** dataframe in order to find the  index of maximum average steps interval

```r
intv<-average_steps_aggregated_by_intervals[which.max(average_steps_aggregated_by_intervals$step_av),1]
print(intv)
```

```
## integer(0)
```

## Imputing missing values

### step 8 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA\color{red}{\verb|NA|}NAs)

As only the variable steps has NAs we need to calculate  NAs on colum 1

```r
sum(is.na(database1$steps))
```

```
## [1] 2304
```

### step 9  Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

As there are some days in which all intervals have  steps  with  NAs\color{red}, It is more appropriated  filling NAs with the mean for that 5-minute interval. So We have made a function in which the NA is replaced by the mean of steps in similar intervals. The function namw is **replaceNA**

```r
replaceNA<-function(x=is.data.frame()){
  #Aggregtion  of steps by interval, summarizing by mean
  ag<-aggregate(steps~interval,data=x,mean,na.rm=TRUE )
  
  # Loop to find and replace NAs
  for(i in 1:length(x$steps)){
     
       if(is.na(x$steps[i])){
             interv<-x$interval[i]
            val<-ag$steps[ag$interval==interv]
            x$steps[i]<-val
       } 
  }
  
  x
}
```

### step 10 Create a new dataset that is equal to the original dataset but with the missing data filled



```r
# Generaing the new database without NAs
database2<-replaceNA(x=database1)
#checking the new database
sum(is.na(database2$steps))
```

```
## [1] 0
```

### step 11 Histogram of the total number of steps taken each day after missing values are imputed

First we are going to aggregated the new database by date, the result is the dataframe **total_steps_aggregated_by_date2** 

```r
total_steps_aggregated_by_date2<-aggregate(steps~date,data =database2,sum,na.rm=TRUE )
```




```r
par(mar=c(5,5,5,2))

h<-hist(total_steps_aggregated_by_date2$steps,cex=1.8,col="darkmagenta",main="",ylab = "frequency",xlab ="total steps per day without NAs")


 text(h$mids,h$counts,labels=h$counts,cex=.75, adj=c(0.5, -0.5))
 title(main = " Histogram Total steps per Day without NAs",outer = FALSE)
```

![plot of chunk hist2](figure/hist2-1.png)



## Are there differences in activity patterns between weekdays and weekends?

### step12 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_ALL","English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
database2$day <-ifelse(weekdays(database2$date,abbreviate = TRUE) %in% c("sat","sun"), "weekend", "weekday")
```


### step 13 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Aggregating **database2(with factor“weekday” and “weekend”)** by mean steps per interval

```r
average_steps_aggregated_by_intervals2<-aggregate(steps~interval,database2,mean,na.rm=TRUE)
```


```r
par(mfrow=c(1,2),mar=c(4,4,2,2))
database3<-aggregate(steps~interval+day,data=database2,mean,na.rm=TRUE)
database3_weekday<-subset(database3,day=="weekday")
database3_weekend<-subset(database3,day=="weekend")


plot(database3_weekend$interval,database3_weekend$steps,type="l",col="red",ylab = "Average steps per interval",xlab ="interval",ylim = c(0,300),xlim = c(0,2500))
title(main ="weekend activity")

plot(database3_weekday$interval,database3_weekday$steps,type="l",col="darkblue",ylab = "Average steps per interval",xlab ="interval",ylim = c(0,300),xlim = c(0,2500) )
title(main ="weekday activity")
```

![plot of chunk compare](figure/compare-1.png)

