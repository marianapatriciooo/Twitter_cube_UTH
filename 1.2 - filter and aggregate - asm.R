# Prepare Data
library(reshape)
library(dplyr)
library(lubridate)
library(dplyr)

######################################
setwd("C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\cleanAprilFiles")

# 
# ##make sure there is not more than one user id per user
# test1 <- aggregate(UserID ~ User, hashtagsAnnotated, function(x) length(unique(x)))
# head(test1[order(-test1$UserID),])
# remove(test1)

##make sure no user has more than one affiliation#####
test2 <- aggregate(import_tracked_userID$Party, by= list(import_tracked_userID$UserID), function(x) length(unique(x)))
head(test2[order(-test2$x),])
remove(test2)
##ids 2469543068 and 2785667063 were repeated, so deleted one line of each from the tracked userid
##subset(hashtagsUserID, as.character(userID)==as.character('2785667063'))


##############################################Do the aggregations######################################
##the aggregarions will be performed in all the data from our subset data


##create the 0 dimension value - v_._._.
hash_._._. <- nrow(hashtagsAnnotated)

##Aggregate by 1 dimension
hash_._._h <- aggregate(hashtagsAnnotated$Hashtag, by=list(hashtagsAnnotated$Hashtag), FUN=length)
hash_u_._. <- aggregate(hashtagsAnnotated$UserID, by=list(hashtagsAnnotated$UserID, hashtagsAnnotated$UserName, hashtagsAnnotated$Party), FUN=length)

names(hash_._._h)[names(hash_._._h)=="Group.1"] <- "Hashtag"
names(hash_._._h)[names(hash_._._h)=="x"] <- "count"

names(hash_u_._.)[names(hash_u_._.)=="Group.1"] <- "UserID"
names(hash_u_._.)[names(hash_u_._.)=="Group.2"] <- "UserName"
names(hash_u_._.)[names(hash_u_._.)=="Group.3"] <- "Party"
names(hash_u_._.)[names(hash_u_._.)=="x"] <- "count"


##Aggregate by 2 dimensions

hash_u_._h <- aggregate(hashtagsAnnotated$UserID, by=list(hashtagsAnnotated$UserID, hashtagsAnnotated$UserName, hashtagsAnnotated$Party, hashtagsAnnotated$Hashtag), FUN=length)

names(hash_u_._h)[names(hash_u_._h)=="Group.1"] <- "UserID"
names(hash_u_._h)[names(hash_u_._h)=="Group.2"] <- "UserName"
names(hash_u_._h)[names(hash_u_._h)=="Group.3"] <- "Party"
names(hash_u_._h)[names(hash_u_._h)=="Group.4"] <- "Hashtag"
names(hash_u_._h)[names(hash_u_._h)=="x"] <- "count"


##save the cubes
notFiltered_hash_._._h <- hash_._._h
notFiltered_hash_u_._. <- hash_u_._.
notFiltered_hash_u_._h <- hash_u_._h 







############################### Do the filters for the hashtag graph ASM

hash_u_._h <- notFiltered_hash_u_._h
hash_u_._. <- notFiltered_hash_u_._.
hash_._._h <- notFiltered_hash_._._h

#######################################----FILTERS - USER GRAPH---################################################

##users that have posted more hahstags - top 30% users
usersTop <- aggregate(hash_u_._h$count, by=list(hash_u_._h$UserID) ,FUN=sum)
names(usersTop)[names(usersTop)=="Group.1"] <- "UserID"
names(usersTop)[names(usersTop)=="x"] <- "nbHashtagsSUM"
usersTop[order(-usersTop$nbHashtagsSUM),]
threshold <- quantile(usersTop$nbHashtagsSUM, 0.5)
usersTop <- subset(usersTop, usersTop$nbHashtagsSUM>threshold) 
remove(threshold)


##create list of hashtags and list of users in our annotated data
##filter the aggregations to just have the most present users and hashtags
hashtagsCount <- hash_u_._h [(hash_u_._h$UserID %in% usersTop$UserID),]
hashtagsCount <- aggregate(hashtagsCount$count, by=list(hashtagsCount$Hashtag) ,FUN=sum)
hashtagsCount[order(-hashtagsCount$x),]
hashtagsCount2 <- hash_u_._h [(hash_u_._h$UserID %in% usersTop$UserID),]
hashtagsCount2 <- aggregate(hashtagsCount2$UserID, by=list(hashtagsCount2$Hashtag) ,function(x) FUN=length(unique(x))) #nb of distinct users per hashtag
names(hashtagsCount)[names(hashtagsCount)=="Group.1"] <- "Hashtag"
names(hashtagsCount)[names(hashtagsCount)=="x"] <- "nbOcurrences"
names(hashtagsCount2)[names(hashtagsCount2)=="Group.1"] <- "Hashtag"
names(hashtagsCount2)[names(hashtagsCount2)=="x"] <- "countUsers"
hashtagsCount <-  merge(x = hashtagsCount, y = hashtagsCount2, by = "Hashtag", all.x = TRUE)
remove(hashtagsCount2)
head(hashtagsCount)
##most used hahstags and hashtags used by most users
head(hashtagsCount[order(-hashtagsCount$nbOcurrences),])
head(hashtagsCount[order(-hashtagsCount$countUsers),])


#filter the hashtags that appear more than X times and are used by at least Y users
#take them out of the whole list of hashtags annotated, aggregate again on the users
#keep just the users that posted more than Z times on the filtered by hashatgs
filter_hashtags <- subset(hashtagsCount , nbOcurrences>750 & countUsers>60)
remove(hashtagsCount)
remove(usersTop)



###############Filter on the users###########
usersCount <- hash_u_._h [(hash_u_._h$Hashtag %in% filter_hashtags$Hashtag),]
#usersCount <- aggregate(usersCount$UserID, by=list(usersCount$UserID) ,FUN=length)
usersCount <- aggregate(usersCount$count, by=list(usersCount$UserID) ,FUN=sum)
names(usersCount)[names(usersCount)=="Group.1"] <- "UserID"
names(usersCount)[names(usersCount)=="x"] <- "nbHashtagsCountHsubset"




usersCount2 <- hash_u_._h[hash_u_._h$Hashtag %in% filter_hashtags$Hashtag,]
usersCount2 <- subset(usersCount2, usersCount2$count>0)
usersCount2 <- aggregate(usersCount2$Hashtag, by=list(usersCount2$UserID) , function(x) FUN=length(unique(x))) #nb of distinct hashtags per user
names(usersCount2) <- c("UserID","NbDiffHashtags")


usersCount3 <- aggregate(hash_u_._h$count, by=list(hash_u_._h$UserID) ,FUN=sum)
names(usersCount3)[names(usersCount3)=="Group.1"] <- "UserID"
names(usersCount3)[names(usersCount3)=="x"] <- "nbHashtagsCountofUser"
head(usersCount)
head(usersCount3)


usersCount <- merge(x=usersCount, y=usersCount2, by = c("UserID"), all.x = TRUE)
usersCount <- merge(x=usersCount, y=usersCount3, by = c("UserID"), all.x = TRUE)
usersCount$representedProp <- usersCount$nbHashtagsCountHsubset/usersCount$nbHashtagsCountofUser

head(usersCount)
filter_users <- subset(usersCount, nbHashtagsCountHsubset>50 & NbDiffHashtags>10 & representedProp>0.5)
remove(usersCount)
remove(usersCount2)
remove(usersCount3)


####Confirm that the hashtags kept are all used by at least one user after the users filter
hashtagsUsed <- notFiltered_hash_u_._h [notFiltered_hash_u_._h$UserID %in% filter_users$UserID,]
hashtagsUsed <- hashtagsUsed [hashtagsUsed$Hashtag %in% filter_hashtags$Hashtag,]
##FOR the first week of august some users that passed the filters only have posted one type of hashtags - FILTER ON THAT CRITERIA ALSO?
a <- aggregate(hashtagsUsed$count, by=list(hashtagsUsed$UserID), FUN=length)
a[order(-a$x),]
head(a[order(a$x),])
remove(a)
h <- aggregate(hashtagsUsed$count, by=list(hashtagsUsed$Hashtag), FUN=sum)
names(h) <- c("Hashtag","countTotalUses")
h[order(h$countTotalUses),]
h <- h[h$countTotalUses>0,]
filter_hashtags <-  filter_hashtags [filter_hashtags$Hashtag %in% h$Hashtag,]
remove(h)


##################################################APPLY the filters #############################################
###by one dimension will be the same as the grouped filtered lists
hash_._._h <- hash_._._h [(hash_._._h$Hashtag %in% filter_hashtags$Hashtag),]
hash_u_._. <- hash_u_._. [(hash_u_._.$UserID %in% filter_users$UserID),]
##filter the two dimension aggregates
hash_u_._h <- hash_u_._h[(hash_u_._h $UserID %in% filter_users$UserID),]
hash_u_._h <- hash_u_._h[(hash_u_._h $Hashtag %in% filter_hashtags$Hashtag),]






################################### ASM MODEL ################################################################################
###DO the joins to calculate v* - STATIC APPROACH

#calculate for all combinations of user and hashtag
ASM_Static_users <- merge(x= hash_u_._., y = hash_._._h,  by= NULL)
names(ASM_Static_users)[names(ASM_Static_users)=="count.x"] <- "count_u"
names(ASM_Static_users)[names(ASM_Static_users)=="count.y"] <- "count_h"
ASM_Static_users <- merge(x= ASM_Static_users, y = hash_u_._h, by = c("UserID","UserName" ,"Party", "Hashtag"), all.x = TRUE)
names(ASM_Static_users)[names(ASM_Static_users)=="count"] <- "V"

##if it is NA means the observation is zero
ASM_Static_users$V[is.na(ASM_Static_users$V)] <- 0
##how many combinations user x hashtag have no observations
nrow(ASM_Static_users[ASM_Static_users$V==0,])

ASM_Static_users$div <- (ASM_Static_users$count_h/hash_._._.) 
ASM_Static_users$vStar <- ASM_Static_users$div * ASM_Static_users$count_u
head(ASM_Static_users)

##percentage of the ALL records that were kept for the ASM model (accounting also with deleted users and hashtags)
sum(ASM_Static_users$V) / nrow(hashtagsAnnotated)
##sum the v and vstar to check if the add up to the same in the end
##in the case of hashtagsAnnotated_U_T_H, it has all the hasshtags for all users annotated, before the filtering
sum(notFiltered_hash_u_._h$count)
sum(ASM_Static_users$V)
sum(ASM_Static_users$vStar)
###also see the sum of the observed values for a user is different from the aggregate value for that user (aggregated value is before FILTERING)
head(aggregate(ASM_Static_users$V, by=list(ASM_Static_users$UserID), FUN="sum"))
head(aggregate(hash_u_._.$count, by=list(hash_u_._.$UserID), FUN="sum"))



##Calculate observations-estimatives and observations/estimatives and poisson probabilities
ASM_Static_users$Diff_obs_est <- ASM_Static_users$V-ASM_Static_users$vStar
ASM_Static_users$Prop_obs_est <- ASM_Static_users$V/ASM_Static_users$vStar

ASM_Static_users$Positive_obs_est <- ifelse(ASM_Static_users$Diff_obs_est>=0,"Positive","Negative")

ASM_Static_users$PoissonProb <- ifelse(ASM_Static_users$Diff_obs_est>=0, 1-(ppois( ASM_Static_users$V, ASM_Static_users$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Static_users$V, ASM_Static_users$vStar)), ppois( ASM_Static_users$V, ASM_Static_users$vStar, lower.tail=TRUE, log.p=FALSE)-1)
ASM_Static_users$PoissonProb_log <- ifelse(ASM_Static_users$Diff_obs_est>=0, -log(ppois(ASM_Static_users$V, ASM_Static_users$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Static_users$V, ASM_Static_users$vStar, log = FALSE)), ppois( ASM_Static_users$V, ASM_Static_users$vStar, lower.tail=TRUE, log.p=TRUE))

ASM_Static_users$PoissonProbDensity <- ifelse(ASM_Static_users$Diff_obs_est>=0, 1- dpois (ASM_Static_users$V, ASM_Static_users$vStar), dpois (ASM_Static_users$V, ASM_Static_users$vStar)-1)
ASM_Static_users$PoissonProbDensityLOG <- ifelse(ASM_Static_users$Diff_obs_est>=0, - dpois ( ASM_Static_users$V, ASM_Static_users$vStar, log = TRUE), dpois ( ASM_Static_users$V, ASM_Static_users$vStar, log = TRUE))

ASM_Static_users$PoissonProbLOG_size <- ifelse(ASM_Static_users$Diff_obs_est>=0, -log(ppois(ASM_Static_users$V, ASM_Static_users$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Static_users$V, ASM_Static_users$vStar, log = FALSE)), -ppois( ASM_Static_users$V, ASM_Static_users$vStar, lower.tail=TRUE, log.p=TRUE))
ASM_Static_users$PoissonProb_original <- ifelse(ASM_Static_users$Diff_obs_est>=0, ppois(ASM_Static_users$V, ASM_Static_users$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Static_users$V, ASM_Static_users$vStar, log = FALSE), ppois( ASM_Static_users$V, ASM_Static_users$vStar, lower.tail=TRUE, log.p=FALSE))

head(ASM_Static_users)





##calculate the chi2 metric for each value user-hashtag
#this value, if summed all, gives the answer to/ ios user and hashtag independent?
#H0: they are independent
#H1 they are not independent
##significance level (usually 0,5) - we can mention the result with 95% of confidence
##see the probability of chi2(distribution) with DegreesFredoom = (r - 1) * (c - 1) > found value
##if this probability <0,05 we reject the null hipothesis, ie; we say that they are NOT independent

###the numerator of chi2 is to the power of 2, so will be always positive independent of if V is smaller or greater than vStar
###add the negative signal in the case the observation is lower than the expected - chi2POSNEG
ASM_Static_users$chi2 <- ((ASM_Static_users$V-ASM_Static_users$vStar)^2)/ASM_Static_users$vStar
ASM_Static_users$chi2POSNEG <- ifelse(ASM_Static_users$Diff_obs_est>=0,ASM_Static_users$chi2 ,-ASM_Static_users$chi2 )
ASM_Static_users$sqRootchi2POSNEG <- ifelse(ASM_Static_users$chi2POSNEG>=0, sqrt(ASM_Static_users$chi2) , -sqrt(ASM_Static_users$chi2)  )


####ASM with null observations - if we want just the zeros just do  subset(ASM_Static_users, V>0)
####IMPORTANT: if  subset(ASM_Static_users, V>0) the minimum will be the minimum observed - not zeros if the min is positive!!!!!
asmnonzero <- ASM_Static_users

##############Get normalization values by hashtag - analysis of the users
normalizationAUX_forUSERS <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$Hashtag) ,FUN="max")
names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="x"] <- "MaxHashtag"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$Hashtag) ,FUN="min") 
normalizationAUX_forUSERS <- merge(normalizationAUX_forUSERS, aux, by = c("Group.1"))
names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="x"] <- "MinHashtag"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$Hashtag) ,FUN="mean") 
normalizationAUX_forUSERS <- merge(normalizationAUX_forUSERS, aux, by = c("Group.1"))
names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="x"] <- "MeanHashtag"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$Hashtag) ,FUN="sd") 
normalizationAUX_forUSERS <- merge(normalizationAUX_forUSERS, aux, by = c("Group.1"))
names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="x"] <- "SDHashtag"
names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="Group.1"] <- "Hashtag"
remove(aux)

##############Get normalization values by Users - analysis of the hashtags
####disregard observations zero asmnonzero
normalizationAUX_forHASHTAGS <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$UserID) ,FUN="max")
names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="x"] <- "MaxUser"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$UserID) ,FUN="min") 
normalizationAUX_forHASHTAGS <- merge(normalizationAUX_forHASHTAGS, aux, by = c("Group.1"))
names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="x"] <- "MinUser"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$UserID) ,FUN="mean") 
normalizationAUX_forHASHTAGS <- merge(normalizationAUX_forHASHTAGS, aux, by = c("Group.1"))
names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="x"] <- "MeanUser"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$UserID) ,FUN="sd") 
normalizationAUX_forHASHTAGS <- merge(normalizationAUX_forHASHTAGS, aux, by = c("Group.1"))
names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="x"] <- "SDUser"
names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="Group.1"] <- "UserID"
remove(aux)
remove(asmnonzero)


#################NORMALIZED SQUARE ROOT and STANDARDIZED SQUARE ROOT
###first put the chi2 as zero for v==0
##then try to normalize and standardize
##NORMALIZED - USERS
ASM_Static_users <- merge(ASM_Static_users, normalizationAUX_forUSERS, by = c("Hashtag"), all.x = TRUE)
remove(normalizationAUX_forUSERS)
ASM_Static_users$sqRootMinHashtag <- ifelse(ASM_Static_users$MinHashtag>=0, sqrt(abs(ASM_Static_users$MinHashtag)) , -sqrt(abs(ASM_Static_users$MinHashtag))  )
ASM_Static_users$sqRootMaxHashtag <- ifelse(ASM_Static_users$MaxHashtag>=0, sqrt(abs(ASM_Static_users$MaxHashtag)) , -sqrt(abs(ASM_Static_users$MaxHashtag))  )
ASM_Static_users$sqRootSDHashtag <- sqrt(ASM_Static_users$SDHashtag)
ASM_Static_users$sqRootmeanHashtag <- ifelse(ASM_Static_users$MeanHashtag>=0, sqrt(abs(ASM_Static_users$MeanHashtag)) , -sqrt(abs(ASM_Static_users$MeanHashtag))  )


ASM_Static_users$chi2POSNEG_standBYhashtags <- (ASM_Static_users$sqRootchi2POSNEG - ASM_Static_users$sqRootmeanHashtag)/ASM_Static_users$sqRootSDHashtag
ASM_Static_users$chi2POSNEG_normBYhashtags <- ifelse(ASM_Static_users$V>0, (ASM_Static_users$sqRootchi2POSNEG-ASM_Static_users$sqRootMinHashtag)/(ASM_Static_users$sqRootMaxHashtag-ASM_Static_users$sqRootMinHashtag),0)


##NORMALIZED - HASHTAGS
ASM_Static_users <- merge(ASM_Static_users, normalizationAUX_forHASHTAGS, by = c("UserID"), all.x = TRUE)
remove(normalizationAUX_forHASHTAGS)
ASM_Static_users$sqRootMinUser <- ifelse(ASM_Static_users$MinUser>=0, sqrt(abs(ASM_Static_users$MinUser)) , -sqrt(abs(ASM_Static_users$MinUser))  )
ASM_Static_users$sqRootMaxUser <- ifelse(ASM_Static_users$MaxUser>=0, sqrt(abs(ASM_Static_users$MaxUser)) , -sqrt(abs(ASM_Static_users$MaxUser)) )
ASM_Static_users$sqRootSDUser <- sqrt(ASM_Static_users$SDUser)
ASM_Static_users$sqRootmeanUser <- ifelse(ASM_Static_users$MeanUser>=0, sqrt(abs(ASM_Static_users$MeanUser)) , -sqrt(abs(ASM_Static_users$MeanUser))  )

ASM_Static_users$chi2POSNEG_standBYUsers <- (ASM_Static_users$sqRootchi2POSNEG - ASM_Static_users$sqRootmeanUser)/ASM_Static_users$sqRootSDUser
ASM_Static_users$chi2POSNEG_normBYUsers <- ifelse(ASM_Static_users$V>0, (ASM_Static_users$sqRootchi2POSNEG-ASM_Static_users$sqRootMinUser)/(ASM_Static_users$sqRootMaxUser-ASM_Static_users$sqRootMinUser),0)


###standardize by all
# ASM_Static_users$chi2POSNEG_normHashtags <- scale(ASM_Static_users$chi2POSNEG)
# ASM_Static_users$chi2POSNEG_normUSERS <- scale(ASM_Static_users$chi2POSNEG)










#########################################################################################################


############################### Do the filters for the hashtag graph ASM

hash_u_._h <- notFiltered_hash_u_._h
hash_u_._. <- notFiltered_hash_u_._.
hash_._._h <- notFiltered_hash_._._h

################################create filters


if (exists("filter_hashtags")){  rm(filter_hashtags) }
if (exists("filter_users")){  rm(filter_users) }
  ##do the filters 
  ###############Filter on the hashtags - build the filters on the most active hashtags###########
  ##hashtags that have been posted by more users - top x% hashtags
  hashtagsTop <- aggregate(hash_._._h$count, by=list(hash_._._h$Hashtag) ,FUN=sum)
  names(hashtagsTop)[names(hashtagsTop)=="Group.1"] <- "Hashtag"
  names(hashtagsTop)[names(hashtagsTop)=="x"] <- "nbOccurencesSUM"
  threshold <- quantile(hashtagsTop$nbOccurencesSUM, 0.99) ##take the x percent more active hashtags to see their users in this period
  hashtagsTop <- subset(hashtagsTop, hashtagsTop$nbOccurencesSUM>threshold) 
  remove(threshold)
  hashtagsTop[order(hashtagsTop$nbOccurencesSUM),]
  
  
  ##create list of hashtags and list of users in our annotated data
  ##filter the aggregations to just have the most present users and hashtags
  usersCount <- hash_u_._h [(hash_u_._h$Hashtag %in% hashtagsTop$Hashtag),]
  usersCount <- aggregate(usersCount$count, by=list(usersCount$UserID) ,FUN=sum)
  usersCount[order(-usersCount$x),]
  usersCount2 <- hash_u_._h [(hash_u_._h$Hashtag %in% hashtagsTop$Hashtag),]
  usersCount2 <- subset(usersCount2, usersCount2$count>0)
  usersCount2 <- aggregate(usersCount2$Hashtag, by=list(usersCount2$UserID) , function(x) FUN=length(unique(x))) #nb of distinct users per hashtag
  names(usersCount)[names(usersCount)=="Group.1"] <- "UserID"
  names(usersCount)[names(usersCount)=="x"] <- "nbOcurrences"
  names(usersCount2)[names(usersCount2)=="Group.1"] <- "UserID"
  names(usersCount2)[names(usersCount2)=="x"] <- "countHashtags"
  usersCount <-  merge(x = usersCount, y = usersCount2, by = "UserID", all.x = TRUE)
  head(usersCount)
  ##most used hahstags and hashtags used by most users
  head(usersCount[order(-usersCount$nbOcurrences),])
  head(usersCount[order(-usersCount$countHashtags),])
  
  
  #filter the users that appear more than X times and are used by at least Y hashtags
    filter_users <- subset(usersCount , nbOcurrences>300 & countHashtags>75)

  remove(hashtagsTop)
  remove(usersCount)
  remove(usersCount2)
  
  
  
  
  ###############Filter on the hashtags###########
  ###############Filter on the hastags, starting on the hashtags that have been posted by the kept users after the filter###########
  hashtagsCount <- hash_u_._h [(hash_u_._h$UserID %in% filter_users$UserID),]
  hashtagsCount <- aggregate(hashtagsCount$count, by=list(hashtagsCount$Hashtag) ,FUN=sum)
  names(hashtagsCount) <- c("Hashtag","nbUsersCountUsubset")
  
  hashtagsCount2 <- hash_u_._h [(hash_u_._h$UserID %in% filter_users$UserID),]
  hashtagsCount2 <- subset(hashtagsCount2, hashtagsCount2$count>0)
  hashtagsCount2 <- aggregate(hashtagsCount2$UserID, by=list(hashtagsCount2$Hashtag) , function(x) FUN=length(unique(x))) #nb of distinct hashtags per user
  names(hashtagsCount2) <- c("Hashtag","NbDiffUsers")
  
  
  ##the not filtered counts by user - usersCount2 - To have the percentage of the user activity that is portrayed by the kept hashtags
  hashtagsCount3 <- aggregate( hash_u_._h$count, by=list( hash_u_._h $Hashtag) ,FUN=sum)
  names(hashtagsCount3) <- c("Hashtag","nbUsersCountofHashtag")
  
  hashtagsCount <- merge(x=hashtagsCount, y=hashtagsCount2, by = c("Hashtag"), all.x = TRUE)
  hashtagsCount <- merge(x=hashtagsCount, y=hashtagsCount3, by = c("Hashtag"), all.x = TRUE)
  hashtagsCount$representedProp <- hashtagsCount$nbUsersCountUsubset/hashtagsCount$nbUsersCountofHashtag
  head(hashtagsCount)
  
  ##filter the users that have posted at least X times the kept hashtags and that those hashtags account for at least Y% of their activity
  filter_hashtags <- subset(hashtagsCount, nbUsersCountUsubset>300 & NbDiffUsers>30 & representedProp>0.5)
  

  remove(hashtagsCount)
  remove(hashtagsCount2)
  remove(hashtagsCount3)
  
  
  ####Confirm that the users kept all used at least one hashtag after the hashtags filter
  usersUsed <- hash_u_._h [hash_u_._h$UserID %in% filter_users$UserID,]
  usersUsed <- usersUsed [usersUsed$Hashtag %in% filter_hashtags$Hashtag,]
  h <- aggregate(usersUsed$count, by=list(usersUsed$UserID), FUN=sum)
  names(h) <- c("UserID","countTotalUses")
  h[order(h$countTotalUses),]
  h <- h[h$countTotalUses>0,]
  filter_users <- filter_users [filter_users$UserID %in% h$UserID,]
  
  
  ####APPLY FILTERS
  hash_._._h <- subset(hash_._._h, hash_._._h$Hashtag %in% filter_hashtags$Hashtag)
  hash_u_._. <- subset(hash_u_._., hash_u_._.$UserID %in% filter_users$UserID)
  hash_u_._h <-  subset(hash_u_._h, hash_u_._h$UserID %in% filter_users$UserID)
  hash_u_._h <-  subset(hash_u_._h, hash_u_._h$Hashtag %in% filter_hashtags$Hashtag)














################################### ASM MODEL - Hashtgag graph ################################################################################
###DO the joins to calculate v* - STATIC APPROACH

#calculate for all combinations of user and hashtag
ASM_Static_hashtags <- merge(x= hash_u_._., y = hash_._._h,  by= NULL)
names(ASM_Static_hashtags)[names(ASM_Static_hashtags)=="count.x"] <- "count_u"
names(ASM_Static_hashtags)[names(ASM_Static_hashtags)=="count.y"] <- "count_h"
ASM_Static_hashtags <- merge(x= ASM_Static_hashtags, y = hash_u_._h, by = c("UserID", "UserName","Party", "Hashtag"), all.x = TRUE)
names(ASM_Static_hashtags)[names(ASM_Static_hashtags)=="count"] <- "V"

##if it is NA means the observation is zero
ASM_Static_hashtags$V[is.na(ASM_Static_hashtags$V)] <- 0
##how many combinations user x hashtag have no observations
nrow(ASM_Static_hashtags[ASM_Static_hashtags$V==0,])

ASM_Static_hashtags$div <- (ASM_Static_hashtags$count_h/hash_._._.) 
ASM_Static_hashtags$vStar <- ASM_Static_hashtags$div * ASM_Static_hashtags$count_u
head(ASM_Static_hashtags)

##percentage of the ALL records that were kept for the ASM model (accounting also with deleted users and hashtags)
sum(ASM_Static_hashtags$V) / nrow(hashtagsAnnotated)
##sum the v and vstar to check if the add up to the same in the end
##in the case of hashtagsAnnotated_U_T_H, it has all the hasshtags for all users annotated, before the filtering
sum(notFiltered_hash_u_._h$count)
sum(ASM_Static_hashtags$V)
sum(ASM_Static_hashtags$vStar)
###also see the sum of the observed values for a user is different from the aggregate value for that user (aggregated value is before FILTERING)
head(aggregate(ASM_Static_hashtags$V, by=list(ASM_Static_hashtags$UserID), FUN="sum"))
head(aggregate(hash_u_._.$count, by=list(hash_u_._.$UserID), FUN="sum"))



##Calculate observations-estimatives and observations/estimatives and poisson probabilities
ASM_Static_hashtags$Diff_obs_est <- ASM_Static_hashtags$V-ASM_Static_hashtags$vStar
ASM_Static_hashtags$Prop_obs_est <- ASM_Static_hashtags$V/ASM_Static_hashtags$vStar

ASM_Static_hashtags$Positive_obs_est <- ifelse(ASM_Static_hashtags$Diff_obs_est>=0,"Positive","Negative")

ASM_Static_hashtags$PoissonProb <- ifelse(ASM_Static_hashtags$Diff_obs_est>=0, 1-(ppois( ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Static_hashtags$V, ASM_Static_hashtags$vStar)), ppois( ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, lower.tail=TRUE, log.p=FALSE)-1)
ASM_Static_hashtags$PoissonProb_log <- ifelse(ASM_Static_hashtags$Diff_obs_est>=0, -log(ppois(ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, log = FALSE)), ppois( ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, lower.tail=TRUE, log.p=TRUE))

ASM_Static_hashtags$PoissonProbDensity <- ifelse(ASM_Static_hashtags$Diff_obs_est>=0, 1- dpois (ASM_Static_hashtags$V, ASM_Static_hashtags$vStar), dpois (ASM_Static_hashtags$V, ASM_Static_hashtags$vStar)-1)
ASM_Static_hashtags$PoissonProbDensityLOG <- ifelse(ASM_Static_hashtags$Diff_obs_est>=0, - dpois ( ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, log = TRUE), dpois ( ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, log = TRUE))

ASM_Static_hashtags$PoissonProbLOG_size <- ifelse(ASM_Static_hashtags$Diff_obs_est>=0, -log(ppois(ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, log = FALSE)), -ppois( ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, lower.tail=TRUE, log.p=TRUE))
ASM_Static_hashtags$PoissonProb_original <- ifelse(ASM_Static_hashtags$Diff_obs_est>=0, ppois(ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, log = FALSE), ppois( ASM_Static_hashtags$V, ASM_Static_hashtags$vStar, lower.tail=TRUE, log.p=FALSE))

head(ASM_Static_hashtags)





##calculate the chi2 metric for each value user-hashtag
#this value, if summed all, gives the answer to/ ios user and hashtag independent?
#H0: they are independent
#H1 they are not independent
##significance level (usually 0,5) - we can mention the result with 95% of confidence
##see the probability of chi2(distribution) with DegreesFredoom = (r - 1) * (c - 1) > found value
##if this probability <0,05 we reject the null hipothesis, ie; we say that they are NOT independent

###the numerator of chi2 is to the power of 2, so will be always positive independent of if V is smaller or greater than vStar
###add the negative signal in the case the observation is lower than the expected - chi2POSNEG
ASM_Static_hashtags$chi2 <- ((ASM_Static_hashtags$V-ASM_Static_hashtags$vStar)^2)/ASM_Static_hashtags$vStar
ASM_Static_hashtags$chi2POSNEG <- ifelse(ASM_Static_hashtags$Diff_obs_est>=0,ASM_Static_hashtags$chi2 ,-ASM_Static_hashtags$chi2 )
ASM_Static_hashtags$sqRootchi2POSNEG <- ifelse(ASM_Static_hashtags$chi2POSNEG>=0, sqrt(ASM_Static_hashtags$chi2) , -sqrt(ASM_Static_hashtags$chi2)  )


####ASM with null observations - if we want just the zeros just do  subset(ASM_Static_hashtags, V>0)
####IMPORTANT: if  subset(ASM_Static_hashtags, V>0) the minimum will be the minimum observed - not zeros if the min is positive!!!!!
asmnonzero <- ASM_Static_hashtags

##############Get normalization values by hashtag - analysis of the users
normalizationAUX_forUSERS <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$Hashtag) ,FUN="max")
names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="x"] <- "MaxHashtag"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$Hashtag) ,FUN="min") 
normalizationAUX_forUSERS <- merge(normalizationAUX_forUSERS, aux, by = c("Group.1"))
names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="x"] <- "MinHashtag"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$Hashtag) ,FUN="mean") 
normalizationAUX_forUSERS <- merge(normalizationAUX_forUSERS, aux, by = c("Group.1"))
names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="x"] <- "MeanHashtag"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$Hashtag) ,FUN="sd") 
normalizationAUX_forUSERS <- merge(normalizationAUX_forUSERS, aux, by = c("Group.1"))
names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="x"] <- "SDHashtag"
names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="Group.1"] <- "Hashtag"
remove(aux)

##############Get normalization values by Users - analysis of the hashtags
####disregard observations zero asmnonzero
normalizationAUX_forHASHTAGS <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$UserID) ,FUN="max")
names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="x"] <- "MaxUser"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$UserID) ,FUN="min") 
normalizationAUX_forHASHTAGS <- merge(normalizationAUX_forHASHTAGS, aux, by = c("Group.1"))
names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="x"] <- "MinUser"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$UserID) ,FUN="mean") 
normalizationAUX_forHASHTAGS <- merge(normalizationAUX_forHASHTAGS, aux, by = c("Group.1"))
names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="x"] <- "MeanUser"
aux <- aggregate(asmnonzero$chi2POSNEG, by=list(asmnonzero$UserID) ,FUN="sd") 
normalizationAUX_forHASHTAGS <- merge(normalizationAUX_forHASHTAGS, aux, by = c("Group.1"))
names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="x"] <- "SDUser"
names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="Group.1"] <- "UserID"
remove(aux)
remove(asmnonzero)


#################NORMALIZED SQUARE ROOT and STANDARDIZED SQUARE ROOT
###first put the chi2 as zero for v==0
##then try to normalize and standardize
##NORMALIZED - USERS
ASM_Static_hashtags <- merge(ASM_Static_hashtags, normalizationAUX_forUSERS, by = c("Hashtag"), all.x = TRUE)
remove(normalizationAUX_forUSERS)
ASM_Static_hashtags$sqRootMinHashtag <- ifelse(ASM_Static_hashtags$MinHashtag>=0, sqrt(abs(ASM_Static_hashtags$MinHashtag)) , -sqrt(abs(ASM_Static_hashtags$MinHashtag))  )
ASM_Static_hashtags$sqRootMaxHashtag <- ifelse(ASM_Static_hashtags$MaxHashtag>=0, sqrt(abs(ASM_Static_hashtags$MaxHashtag)) , -sqrt(abs(ASM_Static_hashtags$MaxHashtag))  )
ASM_Static_hashtags$sqRootSDHashtag <- sqrt(ASM_Static_hashtags$SDHashtag)
ASM_Static_hashtags$sqRootmeanHashtag <- ifelse(ASM_Static_hashtags$MeanHashtag>=0, sqrt(abs(ASM_Static_hashtags$MeanHashtag)) , -sqrt(abs(ASM_Static_hashtags$MeanHashtag))  )


ASM_Static_hashtags$chi2POSNEG_standBYhashtags <- (ASM_Static_hashtags$sqRootchi2POSNEG - ASM_Static_hashtags$sqRootmeanHashtag)/ASM_Static_hashtags$sqRootSDHashtag
ASM_Static_hashtags$chi2POSNEG_normBYhashtags <- ifelse(ASM_Static_hashtags$V>0, (ASM_Static_hashtags$sqRootchi2POSNEG-ASM_Static_hashtags$sqRootMinHashtag)/(ASM_Static_hashtags$sqRootMaxHashtag-ASM_Static_hashtags$sqRootMinHashtag),0)


##NORMALIZED - HASHTAGS
ASM_Static_hashtags <- merge(ASM_Static_hashtags, normalizationAUX_forHASHTAGS, by = c("UserID"), all.x = TRUE)
remove(normalizationAUX_forHASHTAGS)
ASM_Static_hashtags$sqRootMinUser <- ifelse(ASM_Static_hashtags$MinUser>=0, sqrt(abs(ASM_Static_hashtags$MinUser)) , -sqrt(abs(ASM_Static_hashtags$MinUser))  )
ASM_Static_hashtags$sqRootMaxUser <- ifelse(ASM_Static_hashtags$MaxUser>=0, sqrt(abs(ASM_Static_hashtags$MaxUser)) , -sqrt(abs(ASM_Static_hashtags$MaxUser)) )
ASM_Static_hashtags$sqRootSDUser <- sqrt(ASM_Static_hashtags$SDUser)
ASM_Static_hashtags$sqRootmeanUser <- ifelse(ASM_Static_hashtags$MeanUser>=0, sqrt(abs(ASM_Static_hashtags$MeanUser)) , -sqrt(abs(ASM_Static_hashtags$MeanUser))  )

ASM_Static_hashtags$chi2POSNEG_standBYUsers <- (ASM_Static_hashtags$sqRootchi2POSNEG - ASM_Static_hashtags$sqRootmeanUser)/ASM_Static_hashtags$sqRootSDUser
ASM_Static_hashtags$chi2POSNEG_normBYUsers <- ifelse(ASM_Static_hashtags$V>0, (ASM_Static_hashtags$sqRootchi2POSNEG-ASM_Static_hashtags$sqRootMinUser)/(ASM_Static_hashtags$sqRootMaxUser-ASM_Static_hashtags$sqRootMinUser),0)


###standardize by all
# ASM_Static_hashtags$chi2POSNEG_normHashtags <- scale(ASM_Static_hashtags$chi2POSNEG)
# ASM_Static_hashtags$chi2POSNEG_normUSERS <- scale(ASM_Static_hashtags$chi2POSNEG)












###### Put on ASM_Static the one we want to use

ASM_Static <- ASM_Static_hashtags
##OR
ASM_Static <- ASM_Static_users
