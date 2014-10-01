# Matthew Miesle
# CUNY SPS - IS 607 SECT 01
# Project 2
# Due: EOD 2014-09-30
# Professor: Mike Schulte

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Remember that your ultimate goal is to tell a story from the data. Include 
# basic visualizations where appropriate.
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



##### Loading Data #####
# library(dplyr)
filelocation <- "C:/Users/MattM/Downloads/2014-02 - Citi Bike trip data.csv"
bike.data <- read.table(filelocation, sep=",", header=TRUE, stringsAsFactors = FALSE)
# alternative to read table with tbl_df() :
# bike.data <- tbl_df(read.table(filelocation, sep=",", header=TRUE, stringsAsFactors = FALSE))

# alternative with more specifications of table read:
# bike.data <- read.table(filelocation, sep=",", header=TRUE, quote="", comment="", stringsAsFactors = FALSE)

# http://www.citibikenyc.com/system-data
# https://s3.amazonaws.com/tripdata/201402-citibike-tripdata.zip
# February 2014 trip data
    # https://s3.amazonaws.com/tripdata/201310-citibike-tripdata.zip
    # October 2013 trip data

##### PART 1 - Variable Analysis #####
# You should include analysis of each variable. Summarize the values, 
# identify any questionable values or outliers, and explain the (possible)
# significance of any missing values in the column.

# 224,736 rides started in the month of February in 2014
# str(bike.data)
# 15 variables, 224736 observations 
# summary(bike.data)
bd.colsummary <- lapply(bike.data, summary)

# box plot each variable in which it makes sense

cat(colnames(bike.data), sep = "\n")
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/colnames.html
# http://victor.barger.us/2010/12/21/printing-the-elements-of-a-vector-one-per-line-in-r/



# tripduration
# starttime
# stoptime
# start.station.id
# start.station.name
# start.station.latitude
# start.station.longitude
# end.station.id
# end.station.name
# end.station.latitude
# end.station.longitude
# bikeid
# usertype
# birth.year
# gender

# Individual variable info:
# Trip Duration (seconds): continuous, integer/numeric
#     minimum is 60 seconds, anything less than 1 minute seems to be counted as 1 full minute
#     !!!!!!!!!!!!!
#     provide more detail about the detail here - min, max, outliers
#     !!!!!!!!!
#     outliers are essentially anything above 200,000 sec (200,000/60 seconds)
#     the max value is over a week long.  there are under 1.3% that are
#     over 45 minutes long
#     users begin to pay fees after trips of a certain length.  for the extremely long
#     trips users would be paying $XXXX.XX amount
#     possible explanations:
#     user didn't properly set bike into return station
#     user took bike home and didn't understand how the system works
#     bike was stolen from user and finally returned later
#     bike was not properly returned and system employees didn't check bike in
#     broken station didn't recognize bike was returned

bd.colsummary$tripduration
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 60.0    360.0    544.0    874.5    845.0 766100.0 
# boxplot without outliers:
boxplot(bike.data$tripduration, ylab = "Trip duration in seconds", 
        main = "Boxplot of Trip Duration without Outliers", outline = FALSE)
# Outliers apear to be above 1600 seconds (27 minutes)
# This number seems low to me.  About 6% of values are outliers by this.
# I would expect outliers to be above 60 minutes, or much higher
# It doesn't seem like there are too many low numbers, but I am suspicious
# of some of the very short trips (under 4 minutes).  It's possible that some users
# are using the system to replace walking in short distances, but it's also possible
# that bikes are returned to the same station in a short period because they need
# maintenance or there was some other issue
# under 2% of rides have the same start and end station.  those rides have a median/mean
# of 6.7min/8.33min so the rides aren't all that short
# length(bike.data$start.station.id[which(bike.data$start.station.id == bike.data$end.station.id)])
# summary(bike.data$start.station.id[which(bike.data$start.station.id == bike.data$end.station.id)])

# user info about the longest trips
bike.data[bike.data$tripduration > 500000, 13:15]
# I would have expected it to be Customers that were over 65, but they're all
# Subscribers and the oldest offender is 70, but the rest are 51 o under and as
# young as 24

# Start Time and Date: continuous, date + time
# Stop Time and Date: continuous, date + time
# Start Station Name: categorical
# End Station Name: categorical
# Station ID: categorical/integer/numeric
# Station Lat/Long: continuous numeric(?)/categorical
# bike.data.uniq.count$start.station.id
#     329 stations
#     this was confirmed by combining the start and end station columns and finding
#     unique values, as well
# Bike ID: categorical/integer/numeric
#     5699 bikes
# User Type: categorical (Customer = 24-hour pass or 7-day pass user; Subscriber = Annual Member)
# Gender: categorical (0=unknown; 1=male; 2=female)
#     unkown values shouldn't have significant impact.  it might be assumed
#     that that there's a 50% chance of being one gender and 50% chance of being
#     the other gender.
# Year of Birth: integer (continuous?) ("\\N" represents NA)
#     doesn't seem that unknown values should have a significant impact
#     1997 is the birth year of the youngest user
#     1899 is the birth year of the oldest user
#     doesn't seem that a 114+ year old would be riding a bike on NYC streets,
#     in the middle of winter, no less
#     might be a default value, user input error, or special recording value
#     6717 rides were made by users for which birth year was not provided
#     no user id available so can't tell how many riders reported being born
#     in a certain year.  can only tell the number of rides made by users of
#     a certain age
#     there are riders whose birth year would make them over 80 at the time of riding
#     there are riders whose birth year would make them over 90 at the time of riding
#     there are riders whose birth year would make them over 100 at the time of riding
#     this can skew calculations regarding age of rider
#     it's possible that the system didn't expect/accept young user input
#         i.e. 1899 should be 1999 and the user is 15 years old

# all of the station information is not necessary to keep in this table
# all that information could be stored in a table of 329 observations
# and any calculations using that information can refer to the separate table

# consider residuals (resid()) for outliers?
# OUTLIERS???
# there's an outliers package available to install

uniqlength <- function(x) length(unique(x))
bike.data.uniq.count <- lapply(bike.data, uniqlength)
# Number of unique values of each variable
# $tripduration
# [1] 4933
# 
# $starttime
# [1] 204437
# 
# $stoptime
# [1] 204781
# 
# $start.station.id
# [1] 329
# 
# $start.station.name
# [1] 329
# 
# $start.station.latitude
# [1] 329
# 
# $start.station.longitude
# [1] 329
# 
# $end.station.id
# [1] 329
# 
# $end.station.name
# [1] 329
# 
# $end.station.latitude
# [1] 329
# 
# $end.station.longitude
# [1] 329
# 
# $bikeid
# [1] 5699
# 
# $usertype
# [1] 2
# 
# $birth.year
# [1] 78
# 
# $gender
# [1] 3

# length(bike.data$start.station.id[which(bike.data$start.station.id == bike.data$end.station.id)])
#     how many trips are made that start and end at the same station?
# seems like could be a problem, but if trip is long in enough in time then might be returning
# to the same station


##### PART 2  - Correlations/patterns #####
# In addition, consider the possibilities of correlations among the variables.
# Look for any interesting patterns. (Do two columns correlate perfectly? 
# Do missing values appear consistent across observations? These are just two 
# such interesting possibilities.)

# use filter from dplyer???
bike.data$gender[which(is.na(bike.data$birth.year))]
unique.gender.age <- unique(cbind(bike.data$birth.year, bike.data$gender))
unique.gender.age[which(unique.gender.age[, 2] == 0), ]
# outside of NA, there are only 2 birth year values that riders have an 
# unknown gender: 1991 and 1951
# !!!!!
# provide the number of rides for each of these combinations?
#     this seems like a good place to use a select or filter

#     !!!!!!!!!!!!!!!!!!!
# do a pairs plot to look for correlations?
# compute the correlation coeff with each pair of columns?
#     !!!!!!!!!!!!!!!!!!!
# initial correlation available is only trip duration to birth year
# there's a correlation coefficient close to 0 so there's not a good
# correlation here
# cor(bike.data$tripduration, bike.data$birth.year, use = "complete.obs")
# -0.009187352
# !!!!!!!!!!!!!
# Could use infogain along with user gender or usertype
# !!!!!!!!!!!!!

##### PART 3 - Recoding/binning #####
#     !!!!!!!!!!!!!!!!!!!
# Consider whether there are any variables that should be recoded or binned. 
# Do such transformations lead to further insights into the data set?
#     !!!!!!!!!!!!!!!!!!!
# binning?: 
#     by age groups (5 year, 10 year, split into 2 ?)
#     trip length (X minute increments, 5, 6, 10, 15, 30, 60, 180, 240 ???)
#     day of week (sort of done if performing conversion)
#     day of month (sort of done if performing conversion)
#     start time (X minute increments, 5, 6, 10, 15, 30, 60, 180, 240 ???)
#     end time (X minute increments, 5, 6, 10, 15, 30, 60, 180, 240 ???)


# Conversion of original data as follows:
# 1. Change the values as necessary to NA in Year of Birth and convert to integer
# library(stringr)
# !!!!!!!!!!
# use dplyr here for filter?
bike.data$birth.year[which(bike.data$birth.year == "\\N")] <- NA
bike.data$birth.year <- as.integer(bike.data$birth.year)
# this was done early on to make some calculation simpler
# should this be repeated for gender == 0 ? it's more of a no response from user
# than unobserved

# 2. split time and date fields to separate into start date, start time, 
# end date, stop time, and add days of week columns
library(lubridate)
# know that all values are
bike.data$startdate <- as.Date(bike.data$starttime)
bike.data$stopdate <- as.Date(bike.data$stoptime)
# use apply function?
bike.data$starttime <- as.POSIXct(bike.data$starttime)
bike.data$stoptime <- as.POSIXct(bike.data$stoptime)
# if there were many more columns to convert would have used an apply function

# want as number (1-7, Sunday = 1 -> label = FALSE default) or as string (label = TRUE)?
bike.data$startdow <- wday(bike.data$starttime, label = FALSE)
bike.data$stopdow <- wday(bike.data$stoptime, label = FALSE)

bike.data$starthour <- hour(bike.data$starttime)
bike.data$stophour <- hour(bike.data$stoptime)

starthour <- data.frame(cbind(0:23, rep(0, 24)))
for(i in 0:23)
{
    starthour[i, ] <- c(i, length(bike.data$starthour[bike.data$starthour == i]))
}
colnames(starthour) <- c("Hour", "Trips")


library(ggplot2)
hourly.plot <- qplot(x = Hour, y = Trips, data = starthour,
                            geom = "bar", stat = "identity") + 
    ggtitle("Trips started within each hour, Feb 2014")
hourly.plot

# It appears that rides peak in the 5pm hour.  Riders biking
# home from work, to social gatherings, or running errands.
# This seems reasonable.  In the morning, folks might use an
# alternative mode of transportation such as the subway to
# get to work on time as opposed to arriving to work after applying
# some physical exertion.  And there's more time for a more leisurely
# ride in the evening.


startday <- data.frame(cbind(1:7, rep(0, 7)))
for(i in 1:7)
{
    startday[i, ] <- c(i, length(bike.data$startdow[bike.data$startdow == i]))
}
colnames(startday) <- c("DayofWeek", "Trips")

# changing the x labels does not work well because the plot alphabetizes
# the order instead of keeping the chronological order
# startday[, 1] <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")

startday.plot <- qplot(x = DayofWeek, y = Trips, data = startday,
                     geom = "bar", stat = "identity") + 
    ggtitle("Trips started on each day of week, Feb 2014")
startday.plot

# It's interesting to see that Monday's have the least ridership
# while Sundays and Tuesdays have the highest.  I think tying this
# information with weather history could help explain this.
# February seems like an odd month to ride a bike frequently so
# it is probably very sensitive to weather

# add speed column
# e.rad <- 6371
# earth's radius in km is 6,371
# delta.lat <- (pi / 180) * (bike.data$end.station.latitude - bike.data$start.station.latitude)
# delta.long <- (pi / 180) * bike.data$end.station.longitude - bike.data$start.station.longitude
# a <- sin(delta.lat / 2) ^ 2 + cos()

# earth's radius in km is 6,371
e.rad <- 6371
lat1 <- (pi / 180) * bike.data$start.station.latitude
lat2 <- (pi / 180) * bike.data$end.station.latitude
long1 <- (pi / 180) * bike.data$start.station.longitude
long2 <- (pi / 180) * bike.data$end.station.longitude

bike.data$disp <- acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(long2 - long1)) * e.rad
# http://www.ig.utexas.edu/outreach/googleearth/latlong.html

bike.data$disp[is.na(bike.data$disp)] <- 0
# some NAs resulted.  those were on some of the rides where
# the start and stop station was the same.  Unsure why it wasn't
# like this for all rides that started and stopped at the same station
# reset the NA values to 0, since they are ultimately a displacement of 0

# average speed of ride is displacement / time
# speed is in kilometers per hour
bike.data$speed <- bike.data$disp / (bike.data$tripduration / 3600)


# need to get start times as HH:MM:SS
# bike.data$starthms <- XXX(bike.data$starttime)
# bike.data$stophms <- XXX(bike.data$stoptime)

# barplots binning by day of week or division of day?
# filter by genders or age groups or subscription type?

# age vs. start time
# plot(x = bike.data$starttime, y = bike.data$birth.year)

# 3. Remove rows in which start and stop are same station
# 4374 rows
# not appropriate because user can ride around and return bike to same station

# 4. Add column with displacement = end point - start point
# formula for displacement from here:
# http://www.ig.utexas.edu/outreach/googleearth/latlong.html


# 5. Add column with speed
# displacement / trip duration

# compare speed with age (consider weekday vs weekend)
# compare speed with start time (consider weekday vs weekend)

# Optional:
# 6. care about direction of travel? (bearing)
# http://www.ig.utexas.edu/outreach/googleearth/latlong.html
# 7. Add fees earned (know that certain riders pay certain rate when
# bike is not returned after certain trip length)