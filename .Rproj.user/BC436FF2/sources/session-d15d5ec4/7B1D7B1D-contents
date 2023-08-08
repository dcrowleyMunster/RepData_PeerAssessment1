library(ggplot2)
library(scales)
library(Hmisc)
library(knitr)
opts_chunk$set(echo = TRUE, results = TRUE, cache = TRUE)

## What is mean total number of steps taken per day?
#1. Calculate the total number of steps taken per day
# Read data into object variable excelDataSourceFile

excelDataSourceFile <- read.csv("activity.csv")
excelDataSourceFile$date <- as.Date(as.character(excelDataSourceFile$date))
excelDataSourceFileNAs <- is.na(excelDataSourceFile$steps)
cleanexcelDataSourceFile <- excelDataSourceFile[!excelDataSourceFileNAs,]
stepsDaily <- aggregate(steps ~ date, excelDataSourceFile, sum, na.rm = TRUE)
print(stepsDaily)

#2. Create histogram of the total number of steps taken each day

totStepsPerDayHistogram <- ggplot(data = na.omit(stepsDaily), aes(x = steps)) + 
        geom_histogram(fill = "green", binwidth = 1000) +
        xlab("Total number of steps Daily") +
        ylab("Frequency") +
        ggtitle("Histogram of the Total Number of Steps Taken Each Day")
print(totStepsPerDayHistogram)


#3. Calculate and report the mean and median of the total number of steps taken per day

stepsDailyMean <- mean(stepsDaily$steps, na.rm = TRUE)
print(stepsDailyMean)
stepsDailyMedian <- median(stepsDaily$steps, na.rm = TRUE)
print(stepsDailyMedian)

#####################
###### What is the average daily activity pattern?

#1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, average  across all days (y-axis)

average <- aggregate(steps ~ interval, excelDataSourceFile, mean, na.rm = TRUE)
library(ggplot2)
timePlot <- ggplot(data = average, aes(x = interval, y = steps)) +
        geom_line(color = "green") +
        xlab("5-minute interval") +
        ylab("average steps") +
        ggtitle("average steps in 5-minute interval")
print(timePlot)


#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

names(average)[1] = "Intervals"
names(average)[2] = "Average_steps"
head(average, 15)

intervalMax <- average[which.max(average$Average_steps),]
intervalMax


## Imputing missing values

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

totalNas <- sum(excelDataSourceFileNAs)
totalNas


#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

missingValues <- is.na(excelDataSourceFile)
table(missingValues)

# Replace missing data with mean of steps
# Use the Hmisc package to impute mean i.e. install.packages("Hmisc"). Then load the package.
library(Hmisc)
imputing <- excelDataSourceFile
imputing$steps <- impute(excelDataSourceFile$steps, mean)
sum(is.na(imputing$steps))


#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
        

sumImputing <- aggregate(steps ~ date, imputing, sum)
names(sumImputing)[1] = "date"
names(sumImputing)[2] = "Imputedsteps"
head(sumImputing, 20)
q4HistsumImpute <- ggplot(data = sumImputing, aes(x = Imputedsteps)) +
        geom_histogram(fill = "red", binwidth = 1000) +
        xlab("Total number of steps each day") +
        ylab("Frequency") +
        ggtitle("Histogram of the Total Number of Steps Taken Each Day")
print(q4HistsumImpute)



# Mean of sumIputing
mean(sumImputing$Imputedsteps)

# Median of sumIputing
median(sumImputing$Imputedsteps)



#### The mean and median of the original data with NAs are 10766.19 and 10765 respectively.
#### Likewise the mean and median of the imputed data without NAs are 10766.19 and 10766.19.
#### Thus, there is no real different between the two sets of data.

## Are there differences in activity patterns between weekdays and weekends?
#1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

imputing$dating <- ifelse(as.POSIXlt(imputing$date)$wday %in% c(0,6), "Weekend", "Weekday")
head(imputing)


#2. Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

meanImputing <- aggregate(steps ~ interval + dating, imputing, mean)
head(meanImputing)
panelPlot <- ggplot(data = meanImputing, aes(x = interval, y = steps)) +
        geom_line(color = "red") +
        facet_grid(dating ~ .) +
        xlab("5-minute interval") +
        ylab("Average steps") +
        ggtitle("Average steps in 5-minute interval")
print(panelPlot)



