---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: yes
---


## **Loading and preprocessing the data**

The following code unzips and read the data.


```r
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```

To achieve better manipulation of the data is required to make some transformations. First, we change the class of the date variable.


```r
data$date <- as.Date(data$date)
```


## **What is mean total number of steps taken per day?**     

With the tapply function we get a vector in which each element is the total number of steps taken each day of the two months.


```r
stepsperday <- tapply(data$steps, data$date, sum)
head(stepsperday)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420
```

Then, with the hist function we can make  histogram of the total number of steps taken each day.


```r
hist(stepsperday, col = "wheat",main = "Histogram of 
     the total number steps taken each day")
abline(v = mean(stepsperday, na.rm =TRUE), col = "blue", lwd = 6)
abline(v = median(stepsperday, na.rm =TRUE), col = "red", lwd = 4)
legend("topright", lty = 1, col = c("blue","red"), 
       legend = c("Mean", "Median"),
       cex = 0.75)
```

<img src="PA1_template_files/figure-html/histogram-1.png" style="display: block; margin: auto;" />

As you can see, the blue and the red lines represent the mean and the median, respectively.


```r
mean <- mean(stepsperday, na.rm = TRUE) 
median <- median(stepsperday, na.rm = TRUE)
mean
```

```
## [1] 10766.19
```

```r
median
```

```
## [1] 10765
```

The mean is 1.0766189\times 10^{4} and the median 10765.

  
## **What is the average daily activity pattern?**   

With the tapply function, we get a vector in which each element is the average number of steps (across all days) taken in each 5-minute average.


```r
avstepsinterval  <-tapply(data$steps, data$interval, mean, na.rm =TRUE)
```

With this vector we can plot a time series that shows the daily pattern activity.


```r
par(mar=c(4,6,4,4))
plot(names(avstepsinterval),avstepsinterval, type = "l", ylab = "average steps
     across all the days", xlab = "5-minute interval", main ="Daily 
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

So the 5-minute interval that contains the maximum number of steps, where is the peak, is 835.


## **Imputing missing values**   
 
The original dataset has some missing observations. This can induce bias in some calculations.
 

```r
numnas <- sum(is.na(data))
numnas
```

```
## [1] 2304
```

there are 2304 missing values.

With an imput strategy we can fill all the missing values. In the following code chunk I create a new dataset (identical to the original) *dataimpute* and use a for loop to assign to each missing value of the new dataset the mean of the corresponding 5-minute interval.


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

We can see that in *dataimpute* there are 0 missing values.

With this new dataset we proceed to make a histogram of the total number of steps taken each day.


```r
stepsperdaynew <-tapply(dataimpute$steps, dataimpute$date, sum)
hist(stepsperdaynew, col = "wheat", main = "Histogram of 
     the total number steps taken each day")
abline(v = mean(stepsperdaynew), col = "blue", lwd = 6)
abline(v = median(stepsperdaynew), col = "red", lwd = 4)
legend("topright", lty = 1, col = c("blue","red"), 
       legend = c("Mean", "Median"),
       cex = 0.75)
```

<img src="PA1_template_files/figure-html/histogram_Impute-1.png" style="display: block; margin: auto;" />


```r
meannew <- mean(stepsperdaynew) 
medianew <- median(stepsperdaynew)
meannew
```

```
## [1] 10766.19
```

```r
medianew
```

```
## [1] 10766.19
```

The mean is 1.0766189\times 10^{4} and the median 1.0766189\times 10^{4}.

As we can see in the histogram, the main impacts of imputing the missing data are that the median and the mean fully equalize and the form distribution gets more symmetric.


## **Are there differences in activity patterns between weekdays and weekends?**

In order to determine if there are differences between the patterns of the weekdays and weekends, we need first to create a new dummy variable that indicates if the observation was taken over the weekdays or the weekends. 


```r
dataimpute$day <- as.factor(weekdays(dataimpute$date))
dataimpute$weektype <- ifelse(dataimpute$day %in% c("sábado","domingo"),
                              "weekend", "weekday")
```

For the following computations is necessary to install and call two packages, lattice and dplr.


```r
install.packages("lattice")
install.packages("dplyr")
library(lattice)
library(dplyr)
```

With the dplyr package we summarise the original dataset. The idea is to have an average (across all days) of the number of steps taken in each 5-minute average on the weekdays and on the weekends. 


```r
subdata <- dataimpute  %>% group_by(weektype, interval)  %>% summarise(meansteps = mean(steps))
```

With the summarised dataset, *subdata*, we proceed to plot a two-panel time series using the lattice package.


```r
xyplot(meansteps ~ interval | weektype,
       data = subdata,
       xlab = "5-minute interval", 
       ylab = "average steps across all the days",
       type ="l",
       lwd= 2,
       layout = c(1, 2),
       main = "Daily pattern activity - Difference between weekdays and 
       weekends"
)
```

![](PA1_template_files/figure-html/lattice-1.png)<!-- -->
