library(reshape)
library(dplyr)
library(data.table)
library(Hmisc)
library(sos)
library(lsa)



###############################################################FILTERS##############################################
####do the filters so we dont have cases where the expected is very low
filterForHashtagsGraph <- function(){
  if (exists("filter_hashtagsSliding")){  rm(filter_hashtagsSliding, envir = globalenv()) }
  if (exists("filter_usersSliding")){  rm(filter_usersSliding, envir = globalenv()) }
  for (i in 1:length(dates$Year)){
    
    ##do the filters 
    ###############Filter on the hashtags - build the filters on the most active hashtags###########
    ##hashtags that have been posted by more users - top x% hashtags
    hashtagsTopSliding <- aggregate(data.frame(hash_._tSlidingWeek_h[i])$count_TH, by=list(data.frame(hash_._tSlidingWeek_h[i])$Hashtag) ,FUN=sum)
    names(hashtagsTopSliding)[names(hashtagsTopSliding)=="Group.1"] <- "Hashtag"
    names(hashtagsTopSliding)[names(hashtagsTopSliding)=="x"] <- "nbOccurencesSUM"
    threshold <- quantile(hashtagsTopSliding$nbOccurencesSUM, 0.9) ##take the x percent more active hashtags to see their users in this period
    hashtagsTopSliding <- subset(hashtagsTopSliding, hashtagsTopSliding$nbOccurencesSUM>threshold) 
    remove(threshold)
    hashtagsTopSliding[order(hashtagsTopSliding$nbOccurencesSUM),]
    
    
    ##create list of hashtags and list of users in our annotated data
    ##filter the aggregations to just have the most present users and hashtags
    usersCountSliding <- data.frame(hash_u_tSlidingWeek_h[i]) [(data.frame(hash_u_tSlidingWeek_h[i])$Hashtag %in% hashtagsTopSliding$Hashtag),]
    usersCountSliding <- aggregate(usersCountSliding$count_UTH, by=list(usersCountSliding$UserID) ,FUN=sum)
    usersCountSliding[order(-usersCountSliding$x),]
    usersCountSliding2 <- data.frame(hash_u_tSlidingWeek_h[i]) [(data.frame(hash_u_tSlidingWeek_h[i])$Hashtag %in% hashtagsTopSliding$Hashtag),]
    usersCountSliding2 <- subset(usersCountSliding2, usersCountSliding2$count_UTH>0)
    usersCountSliding2 <- aggregate(usersCountSliding2$Hashtag, by=list(usersCountSliding2$UserID) , function(x) FUN=length(unique(x))) #nb of distinct users per hashtag
    names(usersCountSliding)[names(usersCountSliding)=="Group.1"] <- "UserID"
    names(usersCountSliding)[names(usersCountSliding)=="x"] <- "nbOcurrences"
    names(usersCountSliding2)[names(usersCountSliding2)=="Group.1"] <- "UserID"
    names(usersCountSliding2)[names(usersCountSliding2)=="x"] <- "countHashtags"
    usersCountSliding <-  merge(x = usersCountSliding, y = usersCountSliding2, by = "UserID", all.x = TRUE)
    head(usersCountSliding)
    ##most used hahstags and hashtags used by most users
    head(usersCountSliding[order(-usersCountSliding$nbOcurrences),])
    head(usersCountSliding[order(-usersCountSliding$countHashtags),])
    
    
    #filter the users that appear more than X times and are used by at least Y hashtags
    if (!exists("filter_usersSliding")){ 
      filter_usersSliding <<- list(subset(usersCountSliding , nbOcurrences>50 & countHashtags>15))
    } else{
      aux <- list(subset(usersCountSliding , nbOcurrences>50 & countHashtags>15))
      filter_usersSliding <<- append(filter_usersSliding, aux)
    }
    remove(hashtagsTopSliding)
    remove(usersCountSliding)
    remove(usersCountSliding2)
    
    
    
    
    ###############Filter on the hashtags###########
    ###############Filter on the hastags, starting on the hashtags that have been posted by the kept users after the filter###########
    hashtagsCountSliding <- data.frame(hash_u_tSlidingWeek_h[i]) [(data.frame(hash_u_tSlidingWeek_h[i])$UserID %in% data.frame(filter_usersSliding[i])$UserID),]
    hashtagsCountSliding <- aggregate(hashtagsCountSliding$count_UTH, by=list(hashtagsCountSliding$Hashtag) ,FUN=sum)
    names(hashtagsCountSliding) <- c("Hashtag","nbUsersCountUsubset")
    
    hashtagsCountSliding2 <- data.frame(hash_u_tSlidingWeek_h[i]) [(data.frame(hash_u_tSlidingWeek_h[i])$UserID %in% data.frame(filter_usersSliding[i])$UserID),]
    hashtagsCountSliding2 <- subset(hashtagsCountSliding2, hashtagsCountSliding2$count_UTH>0)
    hashtagsCountSliding2 <- aggregate(hashtagsCountSliding2$UserID, by=list(hashtagsCountSliding2$Hashtag) , function(x) FUN=length(unique(x))) #nb of distinct hashtags per user
    names(hashtagsCountSliding2) <- c("Hashtag","NbDiffUsers")
    
    
    ##the not filtered counts by user - usersCount2 - To have the percentage of the user activity that is portrayed by the kept hashtags
    hashtagsCountSliding3 <- aggregate( data.frame(hash_u_tSlidingWeek_h[i])$count_UTH, by=list( data.frame(hash_u_tSlidingWeek_h[i])$Hashtag) ,FUN=sum)
    names(hashtagsCountSliding3) <- c("Hashtag","nbUsersCountofHashtag")
    
    hashtagsCountSliding <- merge(x=hashtagsCountSliding, y=hashtagsCountSliding2, by = c("Hashtag"), all.x = TRUE)
    hashtagsCountSliding <- merge(x=hashtagsCountSliding, y=hashtagsCountSliding3, by = c("Hashtag"), all.x = TRUE)
    hashtagsCountSliding$representedProp <- hashtagsCountSliding$nbUsersCountUsubset/hashtagsCountSliding$nbUsersCountofHashtag
    head(hashtagsCountSliding)
    
    ##filter the users that have posted at least X times the kept hashtags and that those hashtags account for at least Y% of their activity
    if (!exists("filter_hashtagsSliding")){ 
      filter_hashtagsSliding <<- list(subset(hashtagsCountSliding, nbUsersCountUsubset>50 & NbDiffUsers>15 & representedProp>0.5))
    }else{
      aux <- list(subset(hashtagsCountSliding, nbUsersCountUsubset>50 & NbDiffUsers>15 & representedProp>0.5))
      filter_hashtagsSliding <<- append(filter_hashtagsSliding, aux)
    }
    remove(hashtagsCountSliding)
    remove(hashtagsCountSliding2)
    remove(hashtagsCountSliding3)
    
    
    ####Confirm that the users kept all used at least one hashtag after the hashtags filter
    usersUsedSliding <- data.frame(hash_u_tSlidingWeek_h[i]) [(data.frame(hash_u_tSlidingWeek_h[i])$UserID %in% data.frame(filter_usersSliding[i])$UserID),]
    usersUsedSliding <- usersUsedSliding [usersUsedSliding$Hashtag %in% data.frame(filter_hashtagsSliding[i])$Hashtag,]
    h <- aggregate(usersUsedSliding$count_UTH, by=list(usersUsedSliding$UserID), FUN=sum)
    names(h) <- c("UserID","countTotalUses")
    h[order(-h$countTotalUses),]
    h <- h[h$countTotalUses>0,]
    filter_usersSliding[i] <<- list(data.frame(filter_usersSliding[i]) [data.frame(filter_usersSliding[i])$UserID %in% h$UserID,])
    
    
    ####APPLY FILTERS
    hash_._tSlidingWeek_h[i] <<- list(subset(data.frame(hash_._tSlidingWeek_h[i]), data.frame(hash_._tSlidingWeek_h[i])$Hashtag %in% data.frame(filter_hashtagsSliding[i])$Hashtag))
    hash_u_tSlidingWeek_.[i] <<- list(subset(data.frame(hash_u_tSlidingWeek_.[i]), data.frame(hash_u_tSlidingWeek_.[i])$UserID %in% data.frame(filter_usersSliding[i])$UserID))
    hash_u_tSlidingWeek_h[i] <<- list(subset(data.frame(hash_u_tSlidingWeek_h[i]), data.frame(hash_u_tSlidingWeek_h[i])$Hashtag %in% data.frame(filter_hashtagsSliding[i])$Hashtag))
    hash_u_tSlidingWeek_h[i] <<- list(subset(data.frame(hash_u_tSlidingWeek_h[i]), data.frame(hash_u_tSlidingWeek_h[i])$UserID %in% data.frame(filter_usersSliding[i])$UserID))
  }
}



###################################################################---ASM---######################################################################
####Build all combinations existent for each time frame###################################################################################
ASM_H <- function(){
  
  if (exists("UTH_each_T_finallinksH")){  remove(UTH_each_T_finallinksH , envir = globalenv()) }
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
    
    
    ##############Get normalization values by Users - analysis of the hashtags
    normalizationAUX_forHASHTAGS <- aggregate(aux$chi2POSNEG, by=list(aux$UserID) ,FUN="max")
    names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="x"] <- "MaxUser"
    aid <- aggregate(aux$chi2POSNEG, by=list(aux$UserID) ,FUN="min") 
    normalizationAUX_forHASHTAGS <- merge(normalizationAUX_forHASHTAGS, aid, by = c("Group.1"))
    names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="x"] <- "MinUser"
    names(normalizationAUX_forHASHTAGS)[names(normalizationAUX_forHASHTAGS)=="Group.1"] <- "UserID"
    remove(aid)
    
    
    #################NORMALIZED SQUARE ROOT 
    ###first put the chi2 as zero for v==0
    ##then try to normalize and standardize

    ##NORMALIZED - HASHTAGS
    aux <- merge(aux, normalizationAUX_forHASHTAGS, by = c("UserID"), all.x = TRUE)
    remove(normalizationAUX_forHASHTAGS)
    aux$sqRootMinUser <- ifelse(aux$MinUser>=0, sqrt(abs(aux$MinUser)) , -sqrt(abs(aux$MinUser))  )
    aux$sqRootMaxUser <- ifelse(aux$MaxUser>=0, sqrt(abs(aux$MaxUser)) , -sqrt(abs(aux$MaxUser))  )
    
    aux$chi2POSNEG_normBYUsers <- ifelse(aux$V>0, (aux$sqRootchi2POSNEG-aux$sqRootMinUser)/(aux$sqRootMaxUser-aux$sqRootMinUser),0)
    
    ####COMPLAINS ABOUT NA's but there is no incomplete cases  
    aux[!complete.cases(aux),]
    
    doCorrelationsForEachTimeFrameHashtags(aux)
    remove(aux)
  }
  
}

####################################### Correlation FUNCTION DEFINITION ######################################################################



###do correlations for each time frame
doCorrelationsForEachTimeFrameHashtags <- function(aux){
    ##do a pivot table
    pivotH <- cast(aux, UserID~Hashtag, value = 'chi2POSNEG_normBYUsers')
    namesH <- colnames(pivotH[2:length(pivotH)])
    pivotH <- as.matrix(pivotH[2:length(pivotH)])
    
    # ##############################cosine similarity############################################
    corrMatrixH<- cosine(pivotH)
    
    
    #############################pearson correlation###########################################
    # corrMatrixH <- cor(pivotH, use="pairwise.complete.obs", method="pearson")
    # corrMatrix_significanceH <- data.frame(rcorr(as.matrix(pivotH), type="pearson")[3])
    # remove(pivotH)
    # 
    # for(a in 1:dim(corrMatrixH)[1]) {
    #   for(b in 1:dim(corrMatrixH)[2]) {
    #     corr <- ifelse(corrMatrix_significanceH[a,b]<=0.05 | is.na(corrMatrix_significanceH[a,b]) ,corrMatrixH[a,b],0 )
    #     corrMatrixH[a,b] <- corr
    #   }
    #   remove(corr)
    # }
    
    
    
    
    ###eliminate correlations to self and simetric links
    ###outputs one list with an edge list of the most relevant links per time frame
    ############################################################filter high relationships- hashatags and users
    corrMatrixH <- data.frame(corrMatrixH)
    colnames(corrMatrixH) <- namesH
    corrMatrixH$Hashtag <- namesH 
    
    
    ######################Hashtags
    melted <-  melt(corrMatrixH)
    melted <- melted[,1:3]
    ##remove correlations to self
    melted <- subset(melted, melted$Hashtag != melted$variable)
    ##see the 20% most relevant correlations are above which value
    quantile <- quantile(melted$value, 0.8)
    melted <- subset(melted, melted$value>0.5
                     #& melted$value>quantile
    )
    melted <- data.frame(t(apply(melted,1,sort)))
    melted <- melted[!duplicated(melted),]
    melted$Hashtag1 <- melted$X2
    melted$Hashtag2 <- melted$X3
    melted$Correlation <- melted$X1
    melted$X1 <- NULL
    melted$X2 <- NULL
    melted$X3 <- NULL
    remove(quantile)
    if(!exists("UTH_each_T_finallinksH")){
      UTH_each_T_finallinksH <<- list(melted) 
    }  else{
      melted <- list(melted)
      UTH_each_T_finallinksH <<- append(UTH_each_T_finallinksH, melted)
    }
    remove(melted)
    
    
    
    ####clean auxiliary variables
    remove(corrMatrixH)
    remove(namesH)

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
filterForHashtagsGraph()
ASM_H() ##ASM function can be used both for the user graph and hashtag graph (defined in previous R file)
head(UTH_each_T_finallinksH)



###write to files
setwd("C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\files_correlations_SlidingWeek\\Hashtags")


for (i in 1:length(UTH_each_T_finallinksH)){
  print(i)
  write.table(UTH_each_T_finallinksH[i],paste("strongestLinksHashtags","_", i ,".csv", sep=""),sep=";", row.names = FALSE) 
}



###############put the aggregations back without filters

hash_._tSlidingWeek_. <- notFiltered_hash_._tSlidingWeek_.
hash_._tSlidingWeek_h <- notFiltered_hash_._tSlidingWeek_h 
hash_u_tSlidingWeek_. <- notFiltered_hash_u_tSlidingWeek_. 
hash_u_tSlidingWeek_h <- notFiltered_hash_u_tSlidingWeek_h 




