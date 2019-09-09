# http://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html
# https://www.kaggle.com/c/bike-sharing-demand
# Load libraries
library(bsts)     # load the bsts package
library(ggplot2)
library(tidyquant)
library(lubridate)
library(tidyverse)
library(dplyr)
theme_set(theme_minimal())

##########################################################
# Train on 20 days for a given month, and predict 10 days 
##########################################################
# Read CSV into R
MyData <- read.csv(file="c:/Users/yourpath/train.csv", header=TRUE, sep=",")
# Basic maniuplation of datetime
MyData$datetime <- strptime(MyData$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
MyData$datetime <- as.POSIXct(MyData$datetime)
MyData

mysubset <- subset(MyData  , datetime >= "2012-11-01 00:00:00")
mysubset <- subset(mysubset, datetime <= "2012-11-19 23:00:00")
mysubset$hour <- hour(mysubset$datetime)
mysubset <- mysubset %>% select(hour, season, holiday, count)
mysubset

# bsts model
ss <- AddLocalLinearTrend(    list(), mysubset$count)
#ss <- AddSemilocalLinearTrend(list(), mysubset$count)
ss <- AddSeasonal(ss, mysubset$count, nseasons = 24)
ss <- AddSeasonal(ss, mysubset$count, nseasons = 168)
#ss <- AddSeasonal(ss, mysubset$count, nseasons = 7, season.duration = 24)
model1 <- bsts(mysubset$count,
               state.specification = ss,
               niter = 1000)
#plot(model1)
pred1 <- predict(model1, horizon = 240)
plot(pred1, plot.original = 480)




##############################################################
# Train on the average values of one week and predict 10 days
##############################################################
# Read CSV into R
MyData <- read.csv(file="c:/Users/yourpath/train.csv", header=TRUE, sep=",")
# Basic maniuplation of datetime
MyData$datetime <- strptime(MyData$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
MyData$datetime <- as.POSIXct(MyData$datetime)
MyData$day      <- weekdays(MyData$datetime)
MyData$hour     <- hour(    MyData$datetime)
MyData

averaged <- MyData %>% group_by(day, hour) %>% summarize(a_mean = mean(count) )
averaged$day <- factor(averaged$day, levels= c("Sunday", "Monday", 
                                               "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
averaged <- averaged[order(averaged$day), ]
averaged$day_hour <- paste(averaged$day, averaged$hour)
head(averaged)


# bsts model
ss <- AddLocalLinearTrend(    list(), averaged$a_mean)
#ss <- AddSemilocalLinearTrend(list(), averaged$a_mean)
ss <- AddSeasonal(ss, averaged$a_mean, nseasons = 24)
ss <- AddSeasonal(ss, averaged$a_mean, nseasons = 168)
#ss <- AddSeasonal(ss, averaged$a_mean, nseasons = 7, season.duration = 24)
model2 <- bsts(averaged$a_mean,
               state.specification = ss,
               niter = 1000)
#plot(model2)
pred2 <- predict(model2, horizon = 240)
plot(pred2, plot.original = 168)



