#R Markdown Assignment 1

We will start step by step by solving each question in the assignment.

###Question1: 
Code for reading in the dataset and/or processing the data
Solution: As the data is in csv format, we will be using read.csv and store the data into a variable called data


```r
setwd("C:/Course era/Assignments/Reproducible Research Week2 Ass")
data <- read.csv("activity.csv")
head(data)
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
 
Now we will process the data for further questions by store data fields into respective variables to be used for plotting


```r
steps<-data$steps
data$date<-as.Date(as.character(data$date),"%Y-%m-%d")
interval<-data$interval
```

###Question2:
Histogram of the total number of steps taken each day
Solution: we will plot a histogram using hist function with date on x axis and steps on y axis


```r
Steps_per_day <- with(data,tapply(data$steps,data$date,sum))
hist(Steps_per_day,main="Total No of Steps Per Day",xlab = "Steps per day", ylab = "Frequency", w = 10)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-14-1.png" width="672" />

###Question3:
Mean and median number of steps taken each day
Solution: Just like we calculated total, now we will calculate mean as well as median of steps each day


```r
Steps_per_day_mean <- mean(Steps_per_day,na.rm= TRUE)
Steps_per_day_median <- median(Steps_per_day, na.rm= TRUE)
```

Median is 10765 and Mean is 10765.

###Question4:
Time series plot of the average number of steps taken
Solution: We will first calculate the average of steps taken each day grouped by 5 minute intervals and show it using plot with type = "l" for the time series.


```r
library(dplyr)
Steps_per_day1 <- data %>% group_by(interval) %>% summarise(mean= mean(steps, na.rm = TRUE))
plot(Steps_per_day1$interval,Steps_per_day1$mean, type = "l", xlab = "5 Min Interval", ylab = "Mean", main = "average number of steps taken")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-16-1.png" width="672" />

###Question5:
The 5-minute interval that, on average, contains the maximum number of steps
Solution: Out of the chart prepared abouve we need to find the interval where max occurs


```r
max_of_mean<-max(Steps_per_day1$mean)
Steps_per_day2<- Steps_per_day1[Steps_per_day1$mean==max_of_mean,]
Steps_per_day2
```

```
## # A tibble: 1 x 2
##   interval     mean
##      <int>    <dbl>
## 1      835 206.1698
```

###Question6:
Code to describe and show a strategy for imputing missing data
Solution:
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
data_impute <- data
for(i in 1:as.numeric(nrow(data_impute))){
      if(is.na(data_impute$steps[i])){
      interval_impute <- data_impute$interval[i]
      step_impute <- Steps_per_day1[Steps_per_day1$interval==interval_impute,]
      data_impute$steps[i]<-step_impute$mean
      }
}

#Now there are no NA values left in the data
sum(nrow(is.na(data_impute$steps)))
```

```
## [1] 0
```

###Question7:
Histogram of the total number of steps taken each day after missing values are imputed


```r
Steps_per_day <- with(data_impute,tapply(data_impute$steps,data_impute$date,sum))
hist(Steps_per_day,main="Total No of Steps Per Day after NA imputation",xlab = "Steps per day", ylab = "Frequency", w = 10)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-19-1.png" width="672" />

###Question8:
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
Solution:
Using the weekday function we can find the day of the week coreesponding to the given dates and then filter out weekdays and weekends and then plot a time series chart on the same.


```r
data_impute$day <- weekdays(data_impute$date)
data_weekday <- data_impute[data_impute$day != c("Saturday","Sunday"),]
data_weekend <- data_impute[data_impute$day == c("Saturday","Sunday"),]
Steps_per_day_weekday <- data_weekday %>% group_by(interval) %>% summarise(mean= mean(steps, na.rm = TRUE))
Steps_per_day_weekend <- data_weekend %>% group_by(interval) %>% summarise(mean= mean(steps, na.rm = TRUE))
```

Now plotting!!


```r
par(mfrow=c(2,1))
plot(Steps_per_day_weekday$interval,Steps_per_day_weekday$mean, type = "l", xlab = "5 Min Interval", ylab = "Mean", main = "average number of steps taken on Weekday")

plot(Steps_per_day_weekend$interval,Steps_per_day_weekend$mean, type = "l", xlab = "5 Min Interval", ylab = "Mean", main = "average number of steps taken on weekend")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-21-1.png" width="672" />

```r
par(mfrow=c(2,1))
```



