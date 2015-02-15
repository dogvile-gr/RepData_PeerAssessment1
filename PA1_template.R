## ----loaddata------------------------------------------------------------
unzip(zipfile="repdata_data_activity.zip")
data <- read.csv("activity.csv")
## ------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
data.sum = aggregate(x=list(steps=data$steps),by=list(Date=data$date),FUN=sum,na.rm=T)
qplot(data.sum$steps, xlab = "total number of steps taken each day",binwidth = 1000)
mean_withNA <- mean(data.sum$steps, na.rm=TRUE)
median_withNA <- median(data.sum$steps, na.rm=TRUE)
## ------------------------------------------------------------------------

library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)

ggplot(data=averages, aes(x=interval, y=steps,ymax=max(averages$steps))) +
        geom_line(colour="coral1", size=1.0) +
        xlab("5-minute interval") +
        ylab("average number of steps taken")
##

## ------------------------------------------------------------------------
averages[which.max(averages$steps),]

## ----how_many_missing----------------------------------------------------
all.na <- sum(is.na(data$steps))


## ------------------------------------------------------------------------
# Replacing each missing value with the mean value of its 5-minute interval

fill.value <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
                filled <- c(steps)
        else
                filled <- (averages[averages$interval==interval, "steps"])
        return(filled)
}

##  new dataset with filled data of missing values
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)




## ------------------------------------------------------------------------
#A histogram of the total number of steps taken each day
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")


#mean and median total number of steps taken per day 
mean_noNA <- mean(total.steps)
median_noNA <- median(total.steps)
mean_withNA <- mean(data.sum$steps, na.rm=TRUE)
median_withNA <- median(data.sum$steps, na.rm=TRUE)

#Differences
mean_withNA-mean_noNA
median_withNA-median_noNA

## ------------------------------------------------------------------------

Day <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                return("weekday")
        else if (day %in% c("Saturday", "Sunday"))
                return("weekend")
        else
                stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=Day)
## ------------------------------------------------------------------------
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")