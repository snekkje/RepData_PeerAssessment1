library(ggplot2)
library(dplyr)

# --- Loading and preprocessing the data ---
#############################################

# Read in activity file, supplied in assignment github repository
activity <- read.csv("activity.csv")


# --- What is mean total number of steps taken per day? ---
############################################################

# Sum the number of steps corresponding to a particular day
aggDate <- aggregate(activity$steps,list(day = activity$date),sum, na.rm = TRUE)

# Plot a histogram of steps taken
qplot(aggDate$x, geom = "histogram") + ggtitle("Histogram of steps taken") + xlab("steps") 

# Find mean and median of the total number of steps taken per day
summary(aggDate$x, na.rm = TRUE)


# --- What is the average daily activity pattern? ---
#####################################################

# Take the mean of the number of steps taken corresponding to a particular interval
aggInterval <- aggregate(activity$steps, list(interval = activity$interval), mean, na.rm = TRUE)

# Plotting a time series showing  interval (x-axis) and the average number of 
# steps taken, averaged across all days (y-axis)
#ggplot(data = aggInterval, aes(interval, x)) + geom_line() + xlab("interval number") +
              #ylab("average step amount")

# The interval which, on average over all days, records the most steps
aggInterval$interval[which.max(aggInterval$x)]


# --- Imputing missing values ---
#################################
my.na <- is.na(activity$steps)

# Find number of NA values
sum(my.na)

# Replace NA "step" values for an interval in the "activity" dataframe with average steps calculated
# using all other days in the same interval
avgValues <- do.call("rbind", replicate(61, aggInterval, simplify = FALSE)) # There are 61 days
activity$steps[my.na] <- avgValues$x[my.na]

# Repeat the steps taken in finding the mean and median of steps taken per day, create a
# histogram
aggDate <- aggregate(activity$steps,list(day = activity$date),sum, na.rm = TRUE)
#qplot(aggDate$x, geom = "histogram") + ggtitle("Histogram of steps taken") + xlab("steps") 
summary(aggDate$x, na.rm = TRUE)


# --- Are there differences in activity patterns between weekdays and weekends? ---
###################################################################################

activityweek <- mutate(activity, Day = {ifelse (weekdays(as.Date(date)) %in% c('Saturday','Sunday'),
                                            "weekend", "weekday")})

# Take the mean of the number of steps taken corresponding to a particular interval conditioned
# on whether it is a weekday / weekend
aggWeek <- aggregate(activityweek$steps, list(interval = activityweek$interval,
                                                day = activityweek$Day), mean)


ggplot(data = aggWeek, aes(interval, x)) + facet_grid(day ~.) + geom_line() + xlab("interval number") +
      ylab("average step amount")




