
# Reproducible Research Assignment 1 - Fitness Steps Data 





This is an R Markdown document to descibe the steps taken to produce the analyses (i.e. code for reading the data and or processing the data, describe and show a strategy for imputing missing data and reproduce the results) and discuss the ouput/findings. 

Plots and calculated numbers required by this assessment piece:
1. Histogram of the total no. of steps taken each day
2. Mean and median no. of steps taken each day
3. Time series plot of the average no. of steps taken
4. The 5-minute interval that, on avg, contains the max no. of steps.
5. Historgram of total no. of steps taken each day after the missing values are imputed
6. Panel plot comparing the avg no. of steps taken per 5-minute interval across weekdays and weekends

## Prepping data
First need to read the data into object data and create another object where the NAs have been removed


```r
library(dplyr)
library(ggplot2)
data <- read.csv("activity.csv",header=TRUE,colClasses=c("numeric","Date","numeric"))
```

## Sneak peak of data
Taking a sneak peak of the data

```r
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

Removing the NAs and grouping the data to be able to prep for histogram and prep for calculations

```r
subdata <- subset(data, data$steps != "NA")
dRMsum <- subdata %>% group_by(date) %>% summarise(TotalSteps=sum(steps))
```

## 1.Histogram of the total no. of steps taken each day
We are going to do a count on the number of total steps taken per day. Here we are going to use the hist function to plot the data. 


```r
hist(dRMsum$TotalSteps,
     breaks=10,
     xlab="Total Steps taken per day"
     ,main="Histogram of the total no. of steps taken per day - frequency"
     ,col="aquamarine3")
```

![plot of chunk Histogram, ](https://raw.githubusercontent.com/MPBBi/RepData_PeerAssessment1/master/figure/Histogram%2C%20-1.png)

Out of interest we are going to do sum the total of steps for each day with a bar plot 

```r
ggplot(dRMsum,aes(x=date,y=TotalSteps,fill=TotalSteps)) +
        geom_bar(stat="identity") +
        scale_fill_gradient(low = "blue", high = "green") +
        labs(title="Total number of steps taken for each day", x="Date",y="Total number of steps")
```

![plot of chunk bar plot, ](https://github.com/MPBBi/RepData_PeerAssessment1/blob/master/figure/bar%20plot%2C%20-1.png)

## 2.Mean and median steps per day 
We are now calculating the mean and meadian of the total no. of steps taken per day 


```r
mean_steps <- mean(dRMsum$TotalSteps)
median_steps <- median(dRMsum$TotalSteps)
```

Printing the mean 

```r
print(mean_steps)
```

```
## [1] 10766.19
```

Printing the median 

```r
print(median_steps)
```

```
## [1] 10765
```

## 3. Time series plot of the average no. of steps taken.
This will be average of the 5 minute interval (x-axis) and average no. of steps taken, average across all days. 
First we will group the data by date like before except for sum, instead we will use mean for the average and have a sneak look


```r
dRMmean <- subdata %>% group_by(interval) %>% summarise(AverageSteps=mean(steps))
head(dRMmean)
```

```
## # A tibble: 6 x 2
##   interval AverageSteps
##      <dbl>        <dbl>
## 1        0       1.72  
## 2        5       0.340 
## 3       10       0.132 
## 4       15       0.151 
## 5       20       0.0755
## 6       25       2.09
```

Now we will plot the data using type ="l"

```r
par(mfrow=c(1,1))
plot(dRMmean,type="l",
     main="Time Series for average number of steps taken per interval",
     xlab="Date",
     ylab="Average no. of steps taken per interval",
     col="darkgoldenrod",
     lwd=2
     )
```

![plot of chunk time series plot](https://raw.githubusercontent.com/MPBBi/RepData_PeerAssessment1/master/figure/time%20series%20plot-1.png)

Out of interest, see the average steps per day 

```r
dRMmeanday <- subdata %>% group_by(date) %>% summarise(AverageSteps=mean(steps))
par(mfrow=c(1,1))
plot(dRMmeanday,type="l",
     main="Time Series for average number of steps taken per day",
     xlab="Date",
     ylab="Average no. of steps taken per interval",
     col="cyan4",
     lwd=2
     )
```

![plot of chunk time series plot by day](https://raw.githubusercontent.com/MPBBi/RepData_PeerAssessment1/master/figure/time%20series%20plot%20by%20day-1.png)

## 4.The 5-minute interval that, on average, contains the maximum number of steps
We want to know which interval on average had the max number of steps, so we need to use the which.max function 

```r
dRMmean[which.max(dRMmean$AverageSteps),]
```

```
## # A tibble: 1 x 2
##   interval AverageSteps
##      <dbl>        <dbl>
## 1      835         206.
```
So we can see that the interval 835 has the highest average no. of steps

## 5. Historgram of total no. of steps taken each day after the missing values are imputed
In this section, we will be dealing with missing values coded as NA and implementing a strategy for them 
First let's see how many NAs are missing. 

```r
nrow(subset(data,is.na(data$steps)))
```

```
## [1] 2304
```
We can see that there is 2304 rows that have missing values 

The strategy we will take to replace the missing values will be the average of that row's interval that was calcualted previously. 
So now we will create a new data set with the missing data replaced.

```r
# first getting a subset of the data that has missing values and merging to the object dRMmean that has the average steps for the intervals
NArow <- merge(subset(data,is.na(data$steps)),dRMmean,by="interval",x.all=TRUE)
# removing the steps with the NAs and renaming the average steps to steps so we can do a rowbind with the object dRM that has no missing values 
NArow <- NArow %>% select(AverageSteps,date,interval) %>% rename(steps=AverageSteps)
dataR <- rbind(NArow,subdata)
```

Now we are going to create a histogram and calculate the mean and median post missing values imputted. 
We are going to do a count on the number of total steps taken per day. Here we are going to use the hist function to plot the data. 


```r
dRMsumR <- dataR %>% group_by(date) %>% summarise(TotalSteps=sum(steps))
hist(dRMsumR$TotalSteps,
     breaks=10,
     xlab="Total Steps taken per day"
     ,main="Histogram of the total no. of steps taken per day with NA replaced - frequency"
     ,col="darkseagreen4")
```

![plot of chunk HistogramR, ](https://raw.githubusercontent.com/MPBBi/RepData_PeerAssessment1/master/figure/HistogramR%2C%20-1.png)

Now the mean and median 

```r
mean_stepsR <- mean(dRMsumR$TotalSteps)
median_stepsR <- median(dRMsumR$TotalSteps)
```

Printing the mean 

```r
print(mean_stepsR)
```

```
## [1] 10766.19
```

Printing the median 

```r
print(median_stepsR)
```

```
## [1] 10766.19
```

## 6. Panel plot comparing the avg no. of steps taken per 5-minute interval across weekdays and weekends
With the complete data we have in object dataR, we are now going to see if there is a difference between weekdays and weekends. 
First we need to add a new variable that flags if the date in the row is a weekday or a weekend 

```r
# adding new column to see the names of the day of the week
dataR$WkFlag <- weekdays(dataR$date)
# creating a reference dataframe to tell us which day is a weekday or weekend
DayofWeek <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
DayofWeekFlag <- c("Weekday","Weekday","Weekday","Weekday","Weekday","Weekend","Weekend")
WeekFlag <- data.frame(DayofWeek,DayofWeekFlag)
# now merging complete data set dataR with reference dataframe with a left outer join 
dataR <- merge(dataR,WeekFlag,x.by="WkFlag",y.by="DayofWeek",x.all=TRUE)
# keep the column we only need 
dataR <- dataR %>% select(steps,date,interval,DayofWeekFlag)
```

Now creating the time series plot with type ="l"
First we have to create two minidatasets for weekday and weekend and then summarise the data to get the averages

```r
WeekdayMean <- subset(dataR,WeekFlag=="Weekday") %>% group_by(interval) %>% summarise(AverageSteps=mean(steps))
WeekendMean <- subset(dataR,WeekFlag=="Weekend") %>% group_by(interval) %>% summarise(AverageSteps=mean(steps))
```

Now we will plot the data using type ="l"

```r
par(mfrow=c(2,1))
plot(WeekdayMean,type="l",
     main="Weekdays - average no. of steps taken per day by interval",
     xlab="Interval",
     ylab="Average no of steps",
     col="darkgoldenrod",
     lwd=1
     )
plot(WeekendMean,type="l",
     main="Weekends - average no. of steps taken per day by interval ",
     xlab="Interval",
     ylab="Average no of steps",
     col="darkgoldenrod",
     lwd=1
     )
```

![plot of chunk time series plot for weekend and weekday](https://raw.githubusercontent.com/MPBBi/RepData_PeerAssessment1/master/figure/time%20series%20plot%20for%20weekend%20and%20weekday-1.png)

