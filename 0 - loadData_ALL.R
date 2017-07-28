#### INSTALLING R PACKAGES FROM CRAN ####

library("lubridate", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(Matrix)
library(reshape2)
library(devtools)
#slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
#install_url(slam_url)
#remove(slam_url)
library(slam_url)
library(tm)
library(SnowballC)
library(wordcloud)
remove(slam_url)



##path
setwd("C:\\Users\\Mariana\\Desktop\\tweeter_april\\Politoscope")

import_hashtags <- fread("flot_hashtag.txt",
                         colClasses = c(
                           "V1"="numeric",
                           "V2"="numeric",
                           "V3"="numeric",
                           "V4"="factor",
                           "V5"="character",
                           "V6"="numeric"))

names(import_hashtags) <- c("Followed", "TweetID", "TweetType","UserID", "Hashtag", "Time")

head(import_hashtags)
class(import_hashtags$Time)
max(import_hashtags$Time)


## 1490054400 is 21 of march of 2017 qt 0h - will restart from here
#import_hashtags <- subset(import_hashtags, import_hashtags$Time>=1490054400)

annotated_users <- fread("C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\cleanAprilFiles\\tracked_userid",
                         colClasses = c(
                           "UserID"="numeric",
                           "Party"="numeric"))

import_hashtags <- merge(x=import_hashtags, y= annotated_users, by = c("UserID"), all.x = TRUE)
import_hashtags <- import_hashtags[complete.cases(import_hashtags),]

###treat the time data
import_hashtags$date <- as.POSIXct(as.numeric(import_hashtags$Time), origin = '1970-01-01')
# 1. use as.POSIXlt - extract date parameters
import_hashtags$hour <- as.POSIXlt(import_hashtags$date)$hour
import_hashtags$day <- as.POSIXlt(import_hashtags$date)$mday
import_hashtags$month <- as.POSIXlt(import_hashtags$date)$mon + 1
import_hashtags$year <- as.POSIXlt(import_hashtags$date)$year + 1900

#1470002400 for 1 august 2016 0h
#1480546800 1 december 2016 0h
#1488322800 1 march 2017 0h
#1494194400 for 8 may 2017 0h
import_hashtags <- subset(import_hashtags, import_hashtags$Time>=1470002400 & import_hashtags$Time<1494194400)

setwd("C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\cleanAprilFiles")

import_hashtags1 <- subset(import_hashtags, import_hashtags$Time>=1470002400 & import_hashtags$Time<1480546800 )
import_hashtags2 <- subset(import_hashtags, import_hashtags$Time>=1480546800 & import_hashtags$Time<1488322800 )
import_hashtags3 <- subset(import_hashtags, import_hashtags$Time>=1488322800 & import_hashtags$Time<1494194400 )
fwrite(import_hashtags1, file="august_november_filteredparty")
fwrite(import_hashtags2, file="december_february_filteredparty")
fwrite(import_hashtags3, file="march_may_filteredparty")

remove(import_hashtags1)
remove(import_hashtags2)
remove(import_hashtags3)

