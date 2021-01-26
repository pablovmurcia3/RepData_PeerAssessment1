---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: yes
---


## **Loading and preprocessing the data**

The following code unzip and read the data

```r
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```

In order to achieve better manipulation of the data is required to make some transformations. First, we change the class of the date variable, then, using this variable we create a new factor variable that only shows the day of the observation 


```r
data$date <- as.Date(data$date)
data$day <- as.factor(weekdays(data$date))
```


## **What is mean total number of steps taken per day?**     

With the tapply function we get a vector in which each element is the total number of steps taken each day of the two months


```r
stepsperday <-tapply(data$steps, data$date, sum, na.rm =TRUE)
```

Then, with the hist function we can make an histogram of the total number of steps taken each day 


```r
hist(stepsperday, col = "wheat", main = "Histogram of 
     the total number steps taken each day")
abline(v = mean(stepsperday), col = "blue", lwd = 4)
abline(v = median(stepsperday), col = "red", lwd = 4)
legend("topright", lty = 1, col = c("blue","red"), 
       legend = c("Mean", "Median"),
       cex = 0.75)
```

<img src="PA1_template_files/figure-html/histogram-1.png" style="display: block; margin: auto;" />

As you can see, the blue and the read lines represents the mean and the median, resrespectively.



```r
mean <- mean(stepsperday) 

median <- median(stepsperday)
```

The mean is 9354.2295082 and the median 10395.

  
## **What is the average daily activity pattern?**   

With the tapply function, we get a vector in which each element is the average  number of steps (across all days) taken in each 5-minute average.


```r
avstepsinterval  <-tapply(data$steps, data$interval, mean, na.rm =TRUE)
```

With this vector we can plot a time series that shows the daily pattern activity

```r
par(mar=c(4,6,4,4))
plot(names(avstepsinterval),avstepsinterval, type = "l", ylab = "average steps
     across all the days", xlab = "5-minute interval", main ="Average daily 
     pattern activity")
```

<img src="PA1_template_files/figure-html/plot-1.png" style="display: block; margin: auto;" />

As we can see, there is a significant peak somewhere between the intervals 600 and 1000. 

```r
maxinte <- names(avstepsinterval)[which.max(avstepsinterval)]
maxinte
```

```
## [1] "835"
```

So the 5-minute interval that contains the maximum number of steps, the peak in the plot, is 835


## **Imputing missing values**   
 
The original dataset have some missing observations. This can induce bias in some calculations
 

```r
numnas <- sum(is.na(data))
numnas
```

```
## [1] 2304
```

there are 2304 missing values.


With an imput strategy we can fill all the missing values. In the following code chunk a create a new dataset (identical to the original) *dataimpute* and use a foor loop to assign to each missing value of the new dataset the mean of the corresponding 5-minute interval.


```r
dataimpute <- data
for (i in seq_along(dataimpute$steps)) {
        if (is.na(dataimpute$steps[i])){
                dataimpute$steps[i] <- avstepsinterval[dataimpute$interval[i] ==                            names(avstepsinterval)]
        }
}
```



```r
sum(is.na(dataimpute))
```

```
## [1] 0
```

We can see that in *dataimpute* there are 0 missing values



```r
stepsperdaynew <-tapply(dataimpute$steps, dataimpute$date, sum)
hist(stepsperdaynew, col = "wheat", main = "Histogram of 
     the total number steps taken each day")
abline(v = mean(stepsperdaynew), col = "blue", lwd = 4)
abline(v = median(stepsperdaynew), col = "red", lwd = 4)
legend("topright", lty = 1, col = c("blue","red"), 
       legend = c("Mean", "Median"),
       cex = 0.75)
```

<img src="PA1_template_files/figure-html/histogram_Impute-1.png" style="display: block; margin: auto;" />



## Are there differences in activity patterns between weekdays and weekends?   

