
library(lubridate)
library(dplyr)
library(data.table)

########################################## Read the cleaned subsets of data produced in openrefine after previous R step####
#####################################then filter just the annotated ones and in the end export one merged file##################


##path
dir <- "C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\cleanAprilFiles"
setwd(dir)
import_tracked_userID <- read.csv("tracked_userid", header = TRUE, encoding='UTF-8')




##import all pieces of the file and filter to get just the annotated users
setwd("C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\cleanAprilFiles\\cleaned")
remove(hashtagsAnnotated)
file_list <- list.files( pattern="*.csv", full.names=TRUE)
for (file in file_list){
  # if the merged dataset doesn't exist, create it
  if (!exists("hashtagsAnnotated")){
    cleanAux <-  fread(file, header=TRUE, sep=",")
    hashtagsAnnotated <- cleanAux
  }
  # if the merged dataset does exist, append to it
  else if(exists("hashtagsAnnotated")){
    cleanAux <-  fread(file, header=TRUE, sep=",")
    hashtagsAnnotated <-rbind(hashtagsAnnotated, cleanAux)
  }
}
remove(cleanAux)

setwd("C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\cleanAprilFiles")
fwrite(hashtagsAnnotated, file="hashtags_cleanedALLv1")
remove(dir)
remove(file)
remove(file_list)
remove(annotated_users)

remove(import_hashtags)


#############################################################################################
###########After last cleaning process - import last file
setwd("C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\cleanAprilFiles")
hashtagsAnnotated <- fread("hashtags_cleanedALLv2.csv", sep=",", header = TRUE,
                           colClasses = c(
                             "UserID"="numeric",
                             "Followed"="numeric",
                             "TweetID"="numeric",
                             "TweetType"="numeric",
                             "Hashtag"="character",
                             "Time"="numeric",
                             "Party"="character",
                             "date"="numeric",
                             "hour"="numeric",
                             "day"="numeric",
                             "month"="numeric",
                             "year"="numeric"), encoding='UTF-8')
hashtagsAnnotated$date <- as.POSIXct(as.numeric(hashtagsAnnotated$Time), origin = '1970-01-01')
hashtagsAnnotated$UserIDFactor <- as.factor(hashtagsAnnotated$UserID)




####get users names
dir <- "C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\cleanAprilFiles"
setwd(dir)
usersNames <- fread("id_snms.txt", header = FALSE, 
                    colClasses = c(
                      "V1"="factor",
                      "V2"="character"), encoding='UTF-8')
names(usersNames) <- c("UserIDFactor","UserName")
hashtagsAnnotated <- merge(x = hashtagsAnnotated, y = usersNames, by = "UserIDFactor", all.x = TRUE)



####using the data just from OCTOBER afterwards
## november on 1477954800

hashtagsAnnotated <- subset(hashtagsAnnotated, hashtagsAnnotated$Time>1475272800)
