---
title: "Personal Activity"
author: "Mao Soldevilla"
date: "30/7/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
Sys.setlocale("LC_TIME", "English")
library(lubridate)
library(dplyr)
library(ggplot2)
```

## Introduction

This project use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment were downloaded from the course web site:

Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]
The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file *activity.csv* and there are a total of 17,568 observations in this dataset.

## 1. Code for reading in the dataset and/or processing the data
Loading the data Personal Activity Monitoring as *pam*
```{r}
pam <- read.csv("activity.csv")
str(pam)
```

Due to date is stored as character we need to change to Date.
```{r}
pam <- mutate(pam, "date" = as.Date(date, "%Y-%m-%d"))
str(pam)
```

## 2. Histogram of the total number of steps taken each day
Grouping the data by date ommiting NAs values.
```{r}
by_date <- group_by(pam[!is.na(pam$steps), ], date)
sumsteps <- summarise(by_date, "sum" = sum(steps, na.rm = FALSE))
```

Plotting the histogram from the previous table
```{r}
f <- ggplot(sumsteps, aes(date, sum), na.rm = FALSE)
print(f + geom_bar(stat = "identity") + ggtitle("Number of steps per day") + ylab("steps"))
```

## 3. Mean and median number of steps taken each day

Generating a new table where we have the *mean* and *median* values
The table shows us the summarize of *mean* and *median* for each day
```{r}
mmsteps <- summarise(by_date, "mean" = mean(steps, na.rm = FALSE), "median" = median(steps, na.rm = FALSE))
print(as.data.frame(mmsteps))
```

## 4. Time series plot of the average number of steps taken

With this code we can summarize the data grouping by interval and calculate the numbers of steps per day as an average 
```{r}
by_timing <- group_by(pam[!is.na(pam$steps), ], interval)
meansteps <- summarise(by_timing, "steps" = mean(steps))
plot(meansteps$interval, meansteps$steps, type = "l", xlab = "interval", ylab = "steps", main = "Average of steps all day")
```

## 5. The 5-minute interval that, on average, contains the maximum number of steps
Code for filter the maximum number of steps
```{r}
five <- filter(meansteps, steps == max(steps))
print(five)
```
For the interval `r five$interval` we have `r five$steps` steps that is the maximum number of steps on average per day

## 6. Code to describe and show a strategy for imputing missing data
First, we need to locate where are the NAs values
```{r}
missing <- sum(is.na(pam))
print(missing)
```
The total number of missing values NAs is: `r missing` across the table

In order to check in what days or intervals are missing data.
```{r}
fillnas <- pam %>%
        filter(is.na(steps)) %>%
        with(table(date)) %>%
        print()
```

There are `r fillnas[1]` NA values in each date. Then if we multiply by `r length(fillnas)` we have `r missing`.
That means, there are `r length(fillnas)` days complete with missing values
After identifying where are the missing values, we can fill it in the next question.

Now, we need to replace the NAs values with the means obtained in the question number 4
```{r}
newpam <- pam
for (i in 1:nrow(newpam)){
        if(is.na(newpam$steps[i])){
                j <- i + 287
                newpam$steps[i:j] <- round(meansteps$steps)
        }
}
```

## 7. Histogram of the total number of steps taken each day after missing values are imputed
Getting the new histogram after missing values imputed
```{r}
by_date <- group_by(newpam, date)
sumsteps <- summarise(by_date, "sum" = sum(steps))
fn <- ggplot(sumsteps, aes(date, sum))
print(fn + geom_bar(stat = "identity") + ggtitle("Number of steps per day") + ylab("steps"))
```

Report of the mean and median total number of steps taken per day.
```{r}
mmsteps <- summarise(by_date, "mean" = mean(steps, na.rm = FALSE), "median" = median(steps, na.rm = FALSE))
print(as.data.frame(mmsteps))
```

According this report we can conclude that now there are median values in all the dates where we inputted data and we can see the increase in the total steps per day.


## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

With this code we can insert a new variable called *tday* that is  *type of day*
```{r}
npam <- newpam %>%
        mutate(day = weekdays(date)) %>%
        mutate(tday = factor(1 * (day == "Sunday" | day == "Saturday"), labels = c("weekday", "weekend"))) %>%
        group_by(tday, interval) %>%
        summarise(steps = mean(steps)) %>% 
        print()
```

Plotting the average of steps per type of day weekdays and weekend
```{r}
t <- ggplot(npam, aes(interval, steps)) + geom_line()
print(t + ggtitle("Average number of steps") + facet_grid(tday ~ .))
```
