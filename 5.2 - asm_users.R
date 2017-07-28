library(reshape)
library(dplyr)
library(data.table)
library(Hmisc)
library(sos)
library(lsa)



###############################################################FILTERS##############################################
####do the filters so we dont have cases where the expected is very low
filterForUsersGraph <- function(){
  if (exists("filter_hashtagsSliding")){  rm(filter_hashtagsSliding, envir = globalenv()) }
  if (exists("filter_usersSliding")){  rm(filter_usersSliding, envir = globalenv()) }
  for (i in 1:length(dates$Year)){
    ##do the filters 
    ###############Filter on the users - build the filters on the most active users###########
    ##users that have posted more hahstags - top 30% users
    usersTopSliding <- aggregate(data.frame(hash_u_tSlidingWeek_.[i])$count_UT, by=list(data.frame(hash_u_tSlidingWeek_.[i])$UserID) ,FUN=sum)
    names(usersTopSliding)[names(usersTopSliding)=="Group.1"] <- "UserID"
    names(usersTopSliding)[names(usersTopSliding)=="x"] <- "nbHashtagsSUM"
    usersTopSliding[order(-usersTopSliding$nbHashtagsSUM),]
    threshold <- quantile(usersTopSliding$nbHashtagsSUM, 0.5) ##take the x percent more active users to see their hashatgs in this period
    usersTopSliding <- subset(usersTopSliding, usersTopSliding$nbHashtagsSUM>threshold) 
    remove(threshold)
    
    ##create list of hashtags and list of users in our annotated data
    ##filter the aggregations to just have the most present users and hashtags
    hashtagsCountSliding <- data.frame(hash_u_tSlidingWeek_h[i]) [(data.frame(hash_u_tSlidingWeek_h[i])$UserID %in% usersTopSliding$UserID),]
    hashtagsCountSliding <- aggregate(hashtagsCountSliding$count_UTH, by=list(hashtagsCountSliding$Hashtag) ,FUN=sum)
    hashtagsCountSliding[order(-hashtagsCountSliding$x),]
    hashtagsCountSliding2 <- data.frame(hash_u_tSlidingWeek_h[i]) [(data.frame(hash_u_tSlidingWeek_h[i])$UserID %in% usersTopSliding$UserID),]
    hashtagsCountSliding2 <- subset(hashtagsCountSliding2, hashtagsCountSliding2$count_UTH>0)
    hashtagsCountSliding2 <- aggregate(hashtagsCountSliding2$UserID, by=list(hashtagsCountSliding2$Hashtag) , function(x) FUN=length(unique(x))) #nb of distinct users per hashtag
    names(hashtagsCountSliding)[names(hashtagsCountSliding)=="Group.1"] <- "Hashtag"
    names(hashtagsCountSliding)[names(hashtagsCountSliding)=="x"] <- "nbOcurrences"
    names(hashtagsCountSliding2)[names(hashtagsCountSliding2)=="Group.1"] <- "Hashtag"
    names(hashtagsCountSliding2)[names(hashtagsCountSliding2)=="x"] <- "countUsers"
    hashtagsCountSliding <-  merge(x = hashtagsCountSliding, y = hashtagsCountSliding2, by = "Hashtag", all.x = TRUE)
    head(hashtagsCountSliding)
    ##most used hahstags and hashtags used by most users
    head(hashtagsCountSliding[order(-hashtagsCountSliding$nbOcurrences),])
    head(hashtagsCountSliding[order(-hashtagsCountSliding$countUsers),])
    
    
    #filter the hashtags that appear more than X times and are used by at least Y users
    #take them out of the whole list of hashtags annotated, aggregate again on the users
    #keep just the users that posted more than Z times on the filtered by hashatgs
    if (!exists("filter_hashtagsSliding")){ 
      filter_hashtagsSliding <<- list(subset(hashtagsCountSliding , nbOcurrences>120 & countUsers>30))
    } else{
      aux <- list(subset(hashtagsCountSliding , nbOcurrences>120 & countUsers>30))
      filter_hashtagsSliding <<- append(filter_hashtagsSliding, aux)
    }
    remove(usersTopSliding)
    remove(hashtagsCountSliding)
    remove(hashtagsCountSliding2)
    
    
    
    ###############Filter on the users###########
    ###############Filter on the users, starting on the users that have posted the kept hashtags after the filter###########
    usersCountSliding <- data.frame(hash_u_tSlidingWeek_h[i]) [(data.frame(hash_u_tSlidingWeek_h[i])$Hashtag %in% data.frame(filter_hashtagsSliding[i])$Hashtag),]
    usersCountSliding <- aggregate(usersCountSliding$count_UTH, by=list(usersCountSliding$UserID) ,FUN=sum)
    names(usersCountSliding) <- c("UserID","nbHashtagsCountHsubset")
    
    usersCountSliding2 <- data.frame(hash_u_tSlidingWeek_h[i]) [(data.frame(hash_u_tSlidingWeek_h[i])$Hashtag %in% data.frame(filter_hashtagsSliding[i])$Hashtag),]
    usersCountSliding2 <- subset(usersCountSliding2, usersCountSliding2$count_UTH>0)
    usersCountSliding2 <- aggregate(usersCountSliding2$Hashtag, by=list(usersCountSliding2$UserID) , function(x) FUN=length(unique(x))) #nb of distinct hashtags per user
    names(usersCountSliding2) <- c("UserID","NbDiffHashtags")
    
    
    ##the not filtered counts by user - usersCount2 - To have the percentage of the user activity that is portrayed by the kept hashtags
    usersCountSliding3 <- aggregate( data.frame(hash_u_tSlidingWeek_h[i])$count_UTH, by=list( data.frame(hash_u_tSlidingWeek_h[i])$UserID) ,FUN=sum)
    names(usersCountSliding3) <- c("UserID","nbHashtagsCountofUser")
    
    usersCountSliding <- merge(x=usersCountSliding, y=usersCountSliding2, by = c("UserID"), all.x = TRUE)
    usersCountSliding <- merge(x=usersCountSliding, y=usersCountSliding3, by = c("UserID"), all.x = TRUE)
    usersCountSliding$representedProp <- usersCountSliding$nbHashtagsCountHsubset/usersCountSliding$nbHashtagsCountofUser
    head(usersCountSliding)
    
    ##filter the users that have posted at least X times the kept hashtags and that those hashtags account for at least Y% of their activity
    if (!exists("filter_usersSliding")){ 
      filter_usersSliding <<- list(subset(usersCountSliding, nbHashtagsCountHsubset>30 & NbDiffHashtags>5 & representedProp>0.33))
    }else{
      aux <- list(subset(usersCountSliding, nbHashtagsCountHsubset>30 & NbDiffHashtags>5 & representedProp>0.33))
      filter_usersSliding <<- append(filter_usersSliding, aux)
    }
    remove(usersCountSliding)
    remove(usersCountSliding2)
    remove(usersCountSliding3)
    
    
    ####Confirm that the hashtags kept are all used by at least one user after the users filter
    hashtagsUsedSliding <- data.frame(hash_u_tSlidingWeek_h[i]) [(data.frame(hash_u_tSlidingWeek_h[i])$UserID %in% data.frame(filter_usersSliding[i])$UserID),]
    hashtagsUsedSliding <- hashtagsUsedSliding [hashtagsUsedSliding$Hashtag %in% data.frame(filter_hashtagsSliding[i])$Hashtag,]
    ##FOR the first week of august some users that passed the filters only have posted one type of hashtags - FILTER ON THAT CRITERIA ALSO?
    ##a <- aggregate(hashtagsUsedSliding$count_UTH, by=list(hashtagsUsedSliding$UserID), FUN=length)
    ##a[order(-a$x),]
    ##hashtagsUsedSliding[hashtagsUsedSliding$UserID=="cotetoulouse",]
    ##cotetoulouse
    ##Philippe_Goujon
    h <- aggregate(hashtagsUsedSliding$count_UTH, by=list(hashtagsUsedSliding$Hashtag), FUN=sum)
    names(h) <- c("Hashtag","countTotalUses")
    h[order(-h$countTotalUses),]
    h <- h[h$countTotalUses>0,]
    filter_hashtagsSliding[i] <<-   list(data.frame(filter_hashtagsSliding[i]) [data.frame(filter_hashtagsSliding[i])$Hashtag %in% h$Hashtag,])
    
    
    ####APPLY FILTERS
    hash_._tSlidingWeek_h[i] <<- list(subset(data.frame(hash_._tSlidingWeek_h[i]), data.frame(hash_._tSlidingWeek_h[i])$Hashtag %in% data.frame(filter_hashtagsSliding[i])$Hashtag))
    hash_u_tSlidingWeek_.[i] <<- list(subset(data.frame(hash_u_tSlidingWeek_.[i]), data.frame(hash_u_tSlidingWeek_.[i])$UserID %in% data.frame(filter_usersSliding[i])$UserID))
    hash_u_tSlidingWeek_h[i] <<- list(subset(data.frame(hash_u_tSlidingWeek_h[i]), data.frame(hash_u_tSlidingWeek_h[i])$Hashtag %in% data.frame(filter_hashtagsSliding[i])$Hashtag))
    hash_u_tSlidingWeek_h[i] <<- list(subset(data.frame(hash_u_tSlidingWeek_h[i]), data.frame(hash_u_tSlidingWeek_h[i])$UserID %in% data.frame(filter_usersSliding[i])$UserID))
  }
}

###################################################################---ASM---######################################################################
####Build all combinations existent for each time frame###################################################################################
ASM_U <- function(){
  if (exists("UTH_each_T_finallinksU")){  remove(UTH_each_T_finallinksU , envir = globalenv()) }
  for (i in 1:length(dates$Year)){
    print(i)
    aux <- merge(x= data.frame(hash_u_tSlidingWeek_.[i]), y = data.frame(hash_._tSlidingWeek_h[i]),  by= NULL)
    aux$Year.y <- NULL
    aux$Month.y <- NULL
    aux$Day.y <- NULL
    aux <- merge(x= aux , y = data.frame(hash_._tSlidingWeek_.[i]),  by= NULL)
    aux$Year.x <- NULL
    aux$Month.x <- NULL
    aux$Day.x <- NULL
    aux <- merge(x= aux, y = data.frame(hash_u_tSlidingWeek_h[i]), by = c("UserID", "UserName","Party", "Hashtag"), all.x = TRUE)
    aux$Year.y <- NULL
    aux$Month.y <- NULL
    aux$Day.y <- NULL
    
    ##if it is NA means the observation is zero
    aux$count_UTH[is.na(aux$count_UTH)] <- 0
    ##how many combinations user x hashtag have no observations
    nrow(aux[aux$count_UTH==0,])
    
    names(aux)<-c("UserID", "UserName","Party",  "Hashtag", "count_UT",  "count_TH", "endTimeframe", "count_T", "Year", "Month", "Day", "V")
    
    aux$div <- (aux$count_TH/aux$count_T) 
    aux$vStar <- aux$div * aux$count_UT
    
    
    ##Calculate observations-estimatives and observations/estimatives and poisson probabilities
    aux$Diff_obs_est <- aux$V-aux$vStar
    aux$Prop_obs_est <- aux$V/aux$vStar
    
    aux$Positive_obs_est <- ifelse(aux$Diff_obs_est>=0,"Positive","Negative")
    
    head(aux)
    
    ##calculate the chi2 metric for each value user-hashtag
    #this value, if summed all, gives the answer to/ ios user and hashtag independent?
    #H0: they are independent
    #H1 they are not independent
    ##significance level (usually 0,5) - we can mention the result with 95% of confidence
    ##see the probability of chi2(distribution) with DegreesFredoom = (r - 1) * (c - 1) > found value
    ##if this probability <0,05 we reject the null hipothesis, ie; we say that they are NOT independent
    
    ###the numerator of chi2 is to the power of 2, so will be always positive independent of if V is smaller or greater than vStar
    ###add the negative signal in the case the observation is lower than the expected - chi2POSNEG
    aux$chi2 <- ((aux$V-aux$vStar)^2)/aux$vStar
    aux$chi2POSNEG <- ifelse(aux$Diff_obs_est>=0,aux$chi2 ,-aux$chi2 )
    aux$sqRootchi2POSNEG <- ifelse(aux$chi2POSNEG>=0, sqrt(aux$chi2) , -sqrt(aux$chi2)  )
    
    
    ###################################################### NORMALIZATION
    ##############Get normalization values by hashtag - analysis of the users
    normalizationAUX_forUSERS <- aggregate(aux$chi2POSNEG, by=list(aux$Hashtag) ,FUN="max")
    names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="x"] <- "MaxHashtag"
    aid <- aggregate(aux$chi2POSNEG, by=list(aux$Hashtag) ,FUN="min") 
    normalizationAUX_forUSERS <- merge(normalizationAUX_forUSERS, aid, by = c("Group.1"))
    names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="x"] <- "MinHashtag"
    names(normalizationAUX_forUSERS)[names(normalizationAUX_forUSERS)=="Group.1"] <- "Hashtag"
    remove(aid)

    
    #################NORMALIZED SQUARE ROOT 
    ###first put the chi2 as zero for v==0
    ##then try to normalize and standardize
    ##NORMALIZED - USERS
    aux <- merge(aux, normalizationAUX_forUSERS, by = c("Hashtag"), all.x = TRUE)
    remove(normalizationAUX_forUSERS)
    aux$sqRootMinHashtag <- ifelse(aux$MinHashtag>=0, sqrt(abs(aux$MinHashtag)) , -sqrt(abs(aux$MinHashtag))  )
    aux$sqRootMaxHashtag <- ifelse(aux$MaxHashtag>=0, sqrt(abs(aux$MaxHashtag)) , -sqrt(abs(aux$MaxHashtag))  )
    
    aux$chi2POSNEG_normBYhashtags <- ifelse(aux$V>0, (aux$sqRootchi2POSNEG-aux$sqRootMinHashtag)/(aux$sqRootMaxHashtag-aux$sqRootMinHashtag),0)
    
    
    ####COMPLAINS ABOUT NA's but there is no incomplete cases  
    aux[!complete.cases(aux),]
    
    doCorrelationsForEachTimeFrameUsers(aux)
    remove(aux)
  }
  
}



####################################### Correlation FUNCTION DEFINITION ######################################################################



###do correlations for each time frame
doCorrelationsForEachTimeFrameUsers <- function(aux){
    ##do a pivot table
    pivotU <- cast(aux, Hashtag~UserID, value = 'chi2POSNEG_normBYhashtags')
    namesU <- colnames(pivotU[2:length(pivotU)])
    pivotU <- as.matrix(pivotU[2:length(pivotU)])
    
    # ##############################cosine similarity############################################
    corrMatrixU<- cosine(pivotU)
    
    
    #############################pearson correlation###########################################
    # corrMatrixU <- cor(pivotU, use="pairwise.complete.obs", method="pearson")
    # corrMatrix_significanceU <- data.frame(rcorr(as.matrix(pivotU), type="pearson")[3])
    # remove(pivotU)
    # for(a in 1:dim(corrMatrixU)[1]) {
    #   for(b in 1:dim(corrMatrixU)[2]) {
    #     corr <- ifelse(corrMatrix_significanceU[a,b]<=0.05 | is.na(corrMatrix_significanceU[a,b]) ,corrMatrixU[a,b],0 )
    #     corrMatrixU[a,b] <- corr
    #   }
    #   remove(corr)
    # }
    
    
    
    ###eliminate correlations to self and simetric links
    ###outputs one list with an edge list of the most relevant links per time frame
    ############################################################filter high relationships- hashatags and users
    
    corrMatrixU <- data.frame(corrMatrixU)
    colnames(corrMatrixU) <- namesU
    corrMatrixU$UserID <- namesU
    
    ############################Users
    melted <-  melt(corrMatrixU)
    melted <- melted[,1:3]
    ##remove correlations to self
    melted <- subset(melted, melted$UserID != melted$variable)
    ##see the 20% most relevant correlations are above which value
    quantile <- quantile(melted$value, 0.8)
    melted <- subset(melted, melted$value>0.5 
                     #& melted$value>quantile
    )
    melted <- data.frame(t(apply(melted,1,sort)))
    melted <- melted[!duplicated(melted),]
    melted$User1 <- melted$X2
    melted$User2 <- melted$X3
    melted$Correlation <- melted$X1
    melted$X1 <- NULL
    melted$X2 <- NULL
    melted$X3 <- NULL
    remove(quantile)
    ##get the party and name for each user
    melted <- merge(x = melted, y = import_tracked_userID, , by.x=c("User1"), by.y=c("UserID"), all.x = TRUE)
    names(melted)[names(melted)=="Party"] <- "PartyUser1"
    melted <- merge(x = melted, y = import_tracked_userID, , by.x=c("User2"), by.y=c("UserID"), all.x = TRUE)
    names(melted)[names(melted)=="Party"] <- "PartyUser2"
    melted$User1_IDFactor <- as.factor(melted$User1)
    melted$User2_IDFactor <- as.factor(melted$User2)
    melted <- merge(x=melted, y=usersNames, by.x=c("User1_IDFactor") , by.y= c("UserIDFactor"), all.x = TRUE)
    names(melted)[names(melted)=="UserName"] <- "User1_UserName"
    melted <- merge(x=melted, y=usersNames, by.x=c("User2_IDFactor") , by.y= c("UserIDFactor"), all.x = TRUE)
    names(melted)[names(melted)=="UserName"] <- "User2_UserName"
    melted$User1_IDFactor <- NULL
    melted$User2_IDFactor <- NULL
    
    if(!exists("UTH_each_T_finallinksU")){
      UTH_each_T_finallinksU <<- list(melted) 
    }  else{
      melted <- list(melted)
      UTH_each_T_finallinksU <<- append(UTH_each_T_finallinksU, melted)
    }
    remove(melted)
    
    
    ####clean auxiliary variables
    remove(corrMatrixU)
    remove(namesU)
    

}





######################################## PROCESS ###########################################################################
#### - do the correlations between users having the values of chi2POSNEG_norm for each hashtag as attributes
#then
#### - remove the self links
#### - get the correlations higher than 0,5
#### - remove the simetrical links (reduces to half since they have the same value both ways)


##############################################################################################################
########################################PROCESS FOR SLIDING WINDOW###########################


#FUNCTIONS
filterForUsersGraph()
ASM_U() ##ASM function can be used both for the user graph and hashtag graph
head(UTH_each_T_finallinksU)


###write to files
setwd("C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\files_correlations_SlidingWeek\\Users")

for (i in 1:length(UTH_each_T_finallinksU)){
  print(i)
  write.table(UTH_each_T_finallinksU[i],paste("strongestLinksUsers","_", i,".csv", sep=""),sep=";", row.names = FALSE) 
}

####See some relqted ones example
#subset(data.frame(hash_u_tSlidingWeek_h[1]), data.frame(hash_u_tSlidingWeek_h[1])$UserID==124391791)
#subset(data.frame(hash_u_tSlidingWeek_h[1]), data.frame(hash_u_tSlidingWeek_h[1])$UserID==111323397)

###############put the aggregations back without filters

hash_._tSlidingWeek_. <- notFiltered_hash_._tSlidingWeek_.
hash_._tSlidingWeek_h <- notFiltered_hash_._tSlidingWeek_h 
hash_u_tSlidingWeek_. <- notFiltered_hash_u_tSlidingWeek_. 
hash_u_tSlidingWeek_h <- notFiltered_hash_u_tSlidingWeek_h 






