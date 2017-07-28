# Prepare Data
library(reshape)
library(dplyr)
library(lubridate)
library(dplyr)
library(data.table)

#######################################SLIDING WINDOWS
##### 1 week = 7 days = 604800 seconds - 86400 seconds a day

###for each day on the hashtagsAnnotated
###get all values of the day up to minus 604800 seconds
dates <- aggregate(hashtagsAnnotated$Hashtag, by=list(hashtagsAnnotated$year, hashtagsAnnotated$month, hashtagsAnnotated$day ), FUN=length)
names(dates) <- c("Year","Month","Day","count")
###get the time stamp of midnight and add a day to make 23h59 of that day (day = 86400 seconds)
dates$timestamp <-   as.numeric(strptime( paste(dates$Year,dates$Month, dates$Day, sep="-"), format = "%Y-%m-%d")) + 86400
dates$timestampMIN28 <- dates$timestamp-(604800*4)
###just keep the dates after the 7th day
dateminiplus28 <- min(dates$timestamp) + (604800*4)
dates <- subset(dates, dates$timestamp>=dateminiplus28)
remove(dateminiplus28)

dates <- dates[order(dates$timestamp),]

##separate the time frames
if (exists("hashtagsAnnotated_Sliding")){  rm(hashtagsAnnotated_Sliding) }
for (i in 1:length(dates$Year)){
  if(!exists("hashtagsAnnotated_Sliding")){
    hashtagsAnnotated_Sliding <- subset(hashtagsAnnotated, hashtagsAnnotated$Time<dates[i,]$timestamp & hashtagsAnnotated$Time>dates[i,]$timestampMIN28)
    hashtagsAnnotated_Sliding$begin <- dates[i,]$timestampMIN28
    hashtagsAnnotated_Sliding$end <- dates[i,]$timestamp
    hashtagsAnnotated_Sliding <- list(hashtagsAnnotated_Sliding)
  }
  else{
    aux <- subset(hashtagsAnnotated, hashtagsAnnotated$Time<dates[i,]$timestamp & hashtagsAnnotated$Time>dates[i,]$timestampMIN28)
    aux$begin <- dates[i,]$timestampMIN28
    aux$end <- dates[i,]$timestamp
    aux <- list(aux)    
    hashtagsAnnotated_Sliding <- append(hashtagsAnnotated_Sliding, aux)
  }
}
remove(aux)


###aggregations to compute at the week level - SLIDING##########################################################


if (exists("hash_._tSlidingWeek_.")){  rm(hash_._tSlidingWeek_.) }
if (exists("hash_._tSlidingWeek_h")){  rm(hash_._tSlidingWeek_h) }
if (exists("hash_u_tSlidingWeek_.")){  rm(hash_u_tSlidingWeek_.) }
if (exists("hash_u_tSlidingWeek_h")){  rm(hash_u_tSlidingWeek_h) }
for (i in 1:length(dates$Year)){
  aux_hash_._tSlidingWeek_. <- aggregate(hashtagsAnnotated_Sliding[[i]]$Hashtag, by=list(hashtagsAnnotated_Sliding[[i]]$end), FUN=length)
  aux_hash_._tSlidingWeek_.$year <-dates[i,]$Year
  aux_hash_._tSlidingWeek_.$month <-  dates[i,]$Month
  aux_hash_._tSlidingWeek_.$day  <- dates[i,]$Day
  
  aux_hash_._tSlidingWeek_h <- aggregate(hashtagsAnnotated_Sliding[[i]]$Hashtag, by=list(hashtagsAnnotated_Sliding[[i]]$Hashtag), FUN=length)
  aux_hash_._tSlidingWeek_h$year <-dates[i,]$Year
  aux_hash_._tSlidingWeek_h$month <-  dates[i,]$Month
  aux_hash_._tSlidingWeek_h$day  <- dates[i,]$Day
  
  aux_hash_u_tSlidingWeek_. <- aggregate(hashtagsAnnotated_Sliding[[i]]$UserID, by=list(hashtagsAnnotated_Sliding[[i]]$UserID, hashtagsAnnotated_Sliding[[i]]$UserName, hashtagsAnnotated_Sliding[[i]]$Party), FUN=length)
  aux_hash_u_tSlidingWeek_.$year <-dates[i,]$Year
  aux_hash_u_tSlidingWeek_.$month <-  dates[i,]$Month
  aux_hash_u_tSlidingWeek_.$day  <- dates[i,]$Day
  
  aux_hash_u_tSlidingWeek_h <- aggregate(hashtagsAnnotated_Sliding[[i]]$UserID, by=list(hashtagsAnnotated_Sliding[[i]]$UserID, hashtagsAnnotated_Sliding[[i]]$UserName, hashtagsAnnotated_Sliding[[i]]$Party , hashtagsAnnotated_Sliding[[i]]$Hashtag), FUN=length)
  aux_hash_u_tSlidingWeek_h$year <-dates[i,]$Year
  aux_hash_u_tSlidingWeek_h$month <-  dates[i,]$Month
  aux_hash_u_tSlidingWeek_h$day  <- dates[i,]$Day
  
  names(aux_hash_._tSlidingWeek_.) <- c("endTimeframe", "count_T", "Year", "Month", "Day")
  names(aux_hash_._tSlidingWeek_h) <- c("Hashtag", "count_TH", "Year", "Month", "Day")
  names(aux_hash_u_tSlidingWeek_.) <- c("UserID", "UserName", "Party", "count_UT", "Year", "Month", "Day")
  names(aux_hash_u_tSlidingWeek_h) <- c("UserID", "UserName",  "Party", "Hashtag", "count_UTH", "Year", "Month", "Day")
  
  if(i==1){
    ##initialize the data frames
    
    hash_._tSlidingWeek_. <- list(aux_hash_._tSlidingWeek_.)
    hash_._tSlidingWeek_h <- list(aux_hash_._tSlidingWeek_h)
    hash_u_tSlidingWeek_. <- list(aux_hash_u_tSlidingWeek_.)
    hash_u_tSlidingWeek_h <- list(aux_hash_u_tSlidingWeek_h)
  }
  else{
    aux_hash_._tSlidingWeek_. <- list(aux_hash_._tSlidingWeek_.)
    aux_hash_._tSlidingWeek_h <- list(aux_hash_._tSlidingWeek_h)
    aux_hash_u_tSlidingWeek_. <- list(aux_hash_u_tSlidingWeek_.)
    aux_hash_u_tSlidingWeek_h <- list(aux_hash_u_tSlidingWeek_h)
    
    hash_._tSlidingWeek_. <- append(hash_._tSlidingWeek_., aux_hash_._tSlidingWeek_.)
    hash_._tSlidingWeek_h <- append(hash_._tSlidingWeek_h, aux_hash_._tSlidingWeek_h)
    hash_u_tSlidingWeek_. <- append(hash_u_tSlidingWeek_., aux_hash_u_tSlidingWeek_.)
    hash_u_tSlidingWeek_h <- append(hash_u_tSlidingWeek_h, aux_hash_u_tSlidingWeek_h)
  }
}
remove(aux_hash_._tSlidingWeek_.)
remove(aux_hash_._tSlidingWeek_h)
remove(aux_hash_u_tSlidingWeek_.)
remove(aux_hash_u_tSlidingWeek_h)




##save the cube
notFiltered_hash_._tSlidingWeek_. <- hash_._tSlidingWeek_.
notFiltered_hash_._tSlidingWeek_h <- hash_._tSlidingWeek_h
notFiltered_hash_u_tSlidingWeek_. <- hash_u_tSlidingWeek_.
notFiltered_hash_u_tSlidingWeek_h <- hash_u_tSlidingWeek_h 

####on the next files:
###create filters for the the case where we are going  to create a USER graph
  #filter the aggregations on it
  #do asm
  #do correlations and save to file
  #update the aggregations for not filtered ones
###create filters for the the case where we are going  to create a HASHTAG graph
  #filter the aggregations on it
  #do asm
  #do correlations and save to file













