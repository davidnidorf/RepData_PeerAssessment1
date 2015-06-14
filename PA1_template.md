---
title: "Activity Monitoring Data Analysis"
author: "David Nidorf"
date: "Saturday, June 13, 2015"
output: html_document
keep_md: yes
---

### Objectives

In this analysis, we will attempt to answer the following four questions:
1. What are the mean and median total number of steps taken per day?
2. What is the average daily activity pattern?
3. With missing values imputed, what are the mean and median total number of steps taken per day?
4. With missing values imputed, are there differences between activity patterns between weekdays and weekends?

#### Loading the data

Before starting, we need to ensure that knitr is loaded and that the right options are set to allow this R Markdown to work correctly in RStudio with the requested directories etc.

Extract and load the data; we expect the data to be in a file called 'activity.csv' which should either be in the working directory, or in a zip of the same name.

```{r setoptions, echo=FALSE}
require(knitr)
opts_chunk$set(echo = TRUE, cache = TRUE, cache.path = "cache/", fig.path ="figure/")

if (!file.exists("activity.csv"))
{
    unzip("activity.zip")
}

activityData <- read.csv("activity.csv")
```

#### Data variables
The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* **date**: The date on which the measurement was taken in YYYY-MM-DD format
* **interval**: Identifier for the 5-minute interval in which measurement was taken

There are a total of 17,568 observations in this dataset.

### Question 1: What are the mean and median total number of steps taken per day?

To get a better sense of the data, we can first calculate the total number of steps per day, and then plot that data in a histogram:

```{r, echo=FALSE}
totalStepsPerDay<-aggregate(steps ~ date, activityData, sum)
hist(totalStepsPerDay$steps, main="Histogram of Steps per Day", xlab="Steps per Day")
```

We can then use this aggregated data to calculate that the mean  number of steps per day is `r prettyNum(mean(totalStepsPerDay$steps))`, and the median number of steps per day is `r median(totalStepsPerDay$steps)`.

### Question 2: What is the average daily activity pattern?

Next, we want to understand the average activity patterns over the course of a day. This means averaging the number of steps by their interval to see which time periods see the most and least activity.  We can then plot this as a time series.

```{r, echo=FALSE}
averageStepsPerInterval<-aggregate(steps ~ interval, activityData, mean)
plot(averageStepsPerInterval, type='l')
```

```{r, echo=FALSE}
maxSteps<-max(averageStepsPerInterval$steps)
intervalOfMaxSteps<-averageStepsPerInterval[which.max(averageStepsPerInterval$steps),"interval"]
timeOfMaxSteps<-prettyNum(intervalOfMaxSteps, big.mark=':', big.interval=2)
```

From this time series, we can see that the peak average number of steps is `r prettyNum(maxSteps)`, which happens during interval `r intervalOfMaxSteps` (which corresponds to `r timeOfMaxSteps`).

### Question 3: With missing values imputed, what are the mean and median total number of steps taken per day?

#### Imputing missing values

In the previous analyses, we simply ignored any missing/NA values.  To understand whether or not this could cause data skew, we first want to calculate the number of NA values in the dataset.

```{r, echo=TRUE}
naCount<-sum(is.na(activityData$steps))
naPercent<-100*(naCount/dim(activityData)[1])
```

Given that number of missing values is `r naCount` which represents `r naPercent`% of the data, for the following analyses we will attempt to interpolate these values in order to ensure that the data is less biased towards certain dates or times.

#### Recalculating the mean and median total number of steps

To attempt to account for both the variance over days and the variance over time-of-day, we will simply replace the missing values with the averaged daily activity pattern values.

```{r, echo=FALSE}
imputedStepsTaken<-ifelse(is.na(activityData$steps), averageStepsPerInterval$steps,activityData$steps)
imputedActivityData<-activityData
imputedActivityData$steps<-imputedStepsTaken
```

With the above algorithm applied, the histogram of the total steps taken has changed to:

```{r, echo=FALSE}
imputedTotalStepsPerDay<-aggregate(steps ~ date, imputedActivityData, sum)
hist(imputedTotalStepsPerDay$steps, main="Histogram of Steps per Day (imputed)", xlab="Steps per Day (imputed)")
```

Our new mean total number of steps per day is `r prettyNum(mean(imputedTotalStepsPerDay$steps))` and our new median total number of steps per day is `r median(imputedTotalStepsPerDay$steps)`.  As expected, this does not differ greatly from the original mean and median, though now the median value is the same as the mean as there are more instances of the mean.

### Question 4: With missing values imputed, are there differences between activity patterns between weekdays and weekends?

First, we add a column to our (imputed) dataset which tells us whether the date is a weekday or a weekend.

```{r, echo=FALSE}
isWeekday<-weekdays(as.Date(imputedActivityData$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
imputedActivityData$isWeekday<-isWeekday
```

Then we can aggregate the data by interval and plot it using two panels in a time series plot:

```{r, echo=FALSE}
averageImputedWeekdayStepsPerInterval<-aggregate(steps ~ interval, imputedActivityData[imputedActivityData$isWeekday,], mean)
averageImputedWeekendStepsPerInterval<-aggregate(steps ~ interval, imputedActivityData[!imputedActivityData$isWeekday,], mean)
par(mfrow=c(2,1))
plot(averageImputedWeekdayStepsPerInterval, type='l', main="Weekend")
plot(averageImputedWeekendStepsPerInterval, type='l', main="Weekday")
```

We can see that based on the average activity per interval, subjects were generally slightly more active on weekends but had a higher peak activity during the week, generally in the morning.

### Addendum

Another approach for imputing the missing values would have been to use the following logic:

1. If data for an entire day is missing, use the averaged daily activity pattern calculated in Question 2.
2. If data for an interval is missing, but there is other data for the day, use the averaged daily activity pattern multiplied by the average steps for that day divided by the averaged steps across all days.

This would have helped ensure that when *some* data was available for a given day, the day would have been given a more appropriate set of data for its missing values.  That said, this is probably overkill for the level of analysis done here.