
library(plotly)
###groupings by cluster and by cluster and time

aux <- merge(x= groups_hashtags, y = notFiltered_hash_u_._h, by = c("Hashtag"), all.x = TRUE)

hash_party_._. <- aggregate(aux$count, by=list(aux$Party), FUN=sum)
names(hash_party_._.)[names(hash_party_._.)=="Group.1"] <- "Party"
names(hash_party_._.)[names(hash_party_._.)=="x"] <- "count"



hash_party_._wardCorr <- aggregate(aux$count, by=list(aux$Party, aux$wardGroupsCorr), FUN=sum)
names(hash_party_._wardCorr)[names(hash_party_._wardCorr)=="Group.1"] <- "Party"
names(hash_party_._wardCorr)[names(hash_party_._wardCorr)=="Group.2"] <- "wardGroupsCorr"
names(hash_party_._wardCorr)[names(hash_party_._wardCorr)=="x"] <- "count"

hash_party_._louvain_weights <- aggregate(aux$count, by=list(aux$Party, aux$louvain_weights), FUN=sum)
names(hash_party_._louvain_weights)[names(hash_party_._louvain_weights)=="Group.1"] <- "Party"
names(hash_party_._louvain_weights)[names(hash_party_._louvain_weights)=="Group.2"] <- "louvain_weights"
names(hash_party_._louvain_weights)[names(hash_party_._louvain_weights)=="x"] <- "count"

hash_party_._louvain_NOweights <- aggregate(aux$count, by=list(aux$Party, aux$louvain_NoWeights), FUN=sum)
names(hash_party_._louvain_NOweights)[names(hash_party_._louvain_NOweights)=="Group.1"] <- "Party"
names(hash_party_._louvain_NOweights)[names(hash_party_._louvain_NOweights)=="Group.2"] <- "louvain_NOweights"
names(hash_party_._louvain_NOweights)[names(hash_party_._louvain_NOweights)=="x"] <- "count"



hash_._._wardCorr <-  aggregate(aux$count, by=list(aux$wardGroupsCorr), FUN=sum)
names(hash_._._wardCorr)[names(hash_._._wardCorr)=="Group.1"] <- "wardGroupsCorr"
names(hash_._._wardCorr)[names(hash_._._wardCorr)=="x"] <- "count"

hash_._._louvain_weights <-  aggregate(aux$count, by=list(aux$louvain_weights), FUN=sum)
names(hash_._._louvain_weights)[names(hash_._._louvain_weights)=="Group.1"] <- "louvain_weights"
names(hash_._._louvain_weights)[names(hash_._._louvain_weights)=="x"] <- "count"


hash_._._louvain_NOweights <-  aggregate(aux$count, by=list(aux$louvain_NoWeights), FUN=sum)
names(hash_._._louvain_NOweights)[names(hash_._._louvain_NOweights)=="Group.1"] <- "louvain_NOweights"
names(hash_._._louvain_NOweights)[names(hash_._._louvain_NOweights)=="x"] <- "count"


remove(aux)


##################################################################################################################################################
##for wardcorr
################################### ASM MODEL ################################################################################
###DO the joins to calculate v* - STATIC APPROACH
#calculate for all cobinations of party and cluster
ASM_StaticwardCorr <- merge(x= hash_party_._., y = hash_._._wardCorr,  by= NULL)
names(ASM_StaticwardCorr)[names(ASM_StaticwardCorr)=="count.x"] <- "count_party"
names(ASM_StaticwardCorr)[names(ASM_StaticwardCorr)=="count.y"] <- "count_wardCorr"
ASM_StaticwardCorr <- merge(x= ASM_StaticwardCorr, y = hash_party_._wardCorr, by = c("Party", "wardGroupsCorr"), all.x = TRUE)
names(ASM_StaticwardCorr)[names(ASM_StaticwardCorr)=="count"] <- "V"

##if it is NA means the observation is zero
ASM_StaticwardCorr$V[is.na(ASM_StaticwardCorr$V)] <- 0
##how many combinations user x hashtag have no observations
nrow(ASM_StaticwardCorr[ASM_StaticwardCorr$V==0,])


ASM_StaticwardCorr$div <- (ASM_StaticwardCorr$count_wardCorr/hash_._._.) 
ASM_StaticwardCorr$vStar <- ASM_StaticwardCorr$div * ASM_StaticwardCorr$count_party
head(ASM_StaticwardCorr)

##percentage of the ALL records that were kept for the ASM model (given that it only keeps relevant hashtags - just the first two filters are implemented)
sum(ASM_StaticwardCorr$V) / nrow(hashtagsAnnotated)
##ASM here keeps just users and hashtags that were present in the original ASM model
##percentage of data kept given just the filtered hashtags
sum(ASM_StaticwardCorr$V) / nrow(subset(hashtagsAnnotated, hashtagsAnnotated$Hashtag %in% filter_hashtags$Hashtag))


##Calculate observations-estimatives and observations/estimatives and poisson probabilities
ASM_StaticwardCorr$Diff_obs_est <- ASM_StaticwardCorr$V-ASM_StaticwardCorr$vStar
ASM_StaticwardCorr$Prop_obs_est <- ASM_StaticwardCorr$V/ASM_StaticwardCorr$vStar

ASM_StaticwardCorr$Positive_obs_est <- ifelse(ASM_StaticwardCorr$Diff_obs_est>=0,"Positive","Negative")

ASM_StaticwardCorr$PoissonProb <- ifelse(ASM_StaticwardCorr$Diff_obs_est>=0, 1-(ppois( ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar)), ppois( ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, lower.tail=TRUE, log.p=FALSE)-1)
ASM_StaticwardCorr$PoissonProb_log <- ifelse(ASM_StaticwardCorr$Diff_obs_est>=0, -log(ppois(ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, log = FALSE)), ppois( ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, lower.tail=TRUE, log.p=TRUE))

ASM_StaticwardCorr$PoissonProbDensity <- ifelse(ASM_StaticwardCorr$Diff_obs_est>=0, 1- dpois (ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar), dpois (ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar)-1)
ASM_StaticwardCorr$PoissonProbDensityLOG <- ifelse(ASM_StaticwardCorr$Diff_obs_est>=0, - dpois ( ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, log = TRUE), dpois ( ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, log = TRUE))

ASM_StaticwardCorr$chi <- ifelse(ASM_StaticwardCorr$Diff_obs_est>=0, -log(ppois(ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, log = FALSE)), -ppois( ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, lower.tail=TRUE, log.p=TRUE))
ASM_StaticwardCorr$PoissonProb_original <- ifelse(ASM_StaticwardCorr$Diff_obs_est>=0, ppois(ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, log = FALSE), ppois( ASM_StaticwardCorr$V, ASM_StaticwardCorr$vStar, lower.tail=TRUE, log.p=FALSE))

###the numerator of chi2 is to the power of 2, so will be always positive independent of if V is smaller or greater than vStar
###add the negative signal in the case the observation is lower than the expected - chi2POSNEG
ASM_StaticwardCorr$chi2 <- ((ASM_StaticwardCorr$V-ASM_StaticwardCorr$vStar)^2)/ASM_StaticwardCorr$vStar
ASM_StaticwardCorr[ ASM_StaticwardCorr$vStar==0 ,]$chi2 <- 0
ASM_StaticwardCorr$chi2POSNEG <- ifelse(ASM_StaticwardCorr$Diff_obs_est>=0,ASM_StaticwardCorr$chi2 ,-ASM_StaticwardCorr$chi2 )
minChi <- min(ASM_StaticwardCorr$chi2POSNEG)
maxChi <- max(ASM_StaticwardCorr$chi2POSNEG)
ASM_StaticwardCorr$chi2POSNEG_norm <-  ifelse(ASM_StaticwardCorr$Diff_obs_est>=0, sqrt(ASM_StaticwardCorr$chi2) ,-sqrt(ASM_StaticwardCorr$chi2) )	
remove(minChi)
remove(maxChi)


head(ASM_StaticwardCorr)



#get the most relevant hastags by party
#mostUsedHashOfClusterwardCorr <- subset(ASM_StaticwardCorr, (ASM_StaticwardCorr$V!=0 ) &( ASM_StaticwardCorr$PoissonProb_original<0.001))
#mostUsedHashOfClusterwardCorr <- subset(ASM_StaticwardCorr, (ASM_StaticwardCorr$V!=0 | ASM_StaticwardCorr$Diff_obs_est< (-50)) &(ASM_StaticwardCorr$Prop_obs_est<0.25 | ASM_StaticwardCorr$Prop_obs_est>4))
##only the interesting ones by being higher than expected
threshold <- quantile(ASM_StaticwardCorr$chi2POSNEG_norm, 0.75)
mostUsedHashOfClusterwardCorr <- subset(ASM_StaticwardCorr, (ASM_StaticwardCorr$V!=0 ) &( ASM_StaticwardCorr$chi2POSNEG_norm>threshold))
mostUsedHashOfClusterwardCorr$wardGroupsCorr <- as.factor(mostUsedHashOfClusterwardCorr$wardGroupsCorr)
remove(threshold)

mostUsedHashOfClusterwardCorr[order(mostUsedHashOfClusterwardCorr$wardGroups),c("wardGroupsCorr","Party", "V", "vStar", "chi2POSNEG")]
plot_ly(mostUsedHashOfClusterwardCorr, x = ~V, y = ~vStar,text= ~Party, type="scatter" , mode = "markers", color = ~wardGroupsCorr, size = ~chi2POSNEG_norm)

#### SEE FOR EACH GROUP WHAT IS THERE
groups_hashtags[groups_hashtags$wardGroupsCorr==1,]


####still not a poisson distribution
int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}
int.hist(subset(ASM_StaticwardCorr$V, ASM_StaticwardCorr$V<50))





##################################################################################################################################################
##for louvain_weights
################################### ASM MODEL ################################################################################
###DO the joins to calculate v* - STATIC APPROACH
#calculate for all cobinations of party and cluster
ASM_Staticlouvain_weights <- merge(x= hash_party_._., y = hash_._._louvain_weights,  by= NULL)
names(ASM_Staticlouvain_weights)[names(ASM_Staticlouvain_weights)=="count.x"] <- "count_party"
names(ASM_Staticlouvain_weights)[names(ASM_Staticlouvain_weights)=="count.y"] <- "count_louvain_weights"
ASM_Staticlouvain_weights <- merge(x= ASM_Staticlouvain_weights, y = hash_party_._louvain_weights, by = c("Party", "louvain_weights"), all.x = TRUE)
names(ASM_Staticlouvain_weights)[names(ASM_Staticlouvain_weights)=="count"] <- "V"

##if it is NA means the observation is zero
ASM_Staticlouvain_weights$V[is.na(ASM_Staticlouvain_weights$V)] <- 0
##how many combinations user x hashtag have no observations
nrow(ASM_Staticlouvain_weights[ASM_Staticlouvain_weights$V==0,])


ASM_Staticlouvain_weights$div <- (ASM_Staticlouvain_weights$count_louvain_weights/hash_._._.) 
ASM_Staticlouvain_weights$vStar <- ASM_Staticlouvain_weights$div * ASM_Staticlouvain_weights$count_party
head(ASM_Staticlouvain_weights)

##percentage of the ALL records that were kept for the ASM model (given that it only keeps relevant hashtags - just the first two filters are implemented)
sum(ASM_Staticlouvain_weights$V) / nrow(hashtagsAnnotated)
##ASM here keeps just users and hashtags that were present in the original ASM model
##percentage of data kept given just the filtered hashtags
sum(ASM_Staticlouvain_weights$V) / nrow(subset(hashtagsAnnotated, hashtagsAnnotated$Hashtag %in% filter_hashtags$Hashtag))


##Calculate observations-estimatives and observations/estimatives and poisson probabilities
ASM_Staticlouvain_weights$Diff_obs_est <- ASM_Staticlouvain_weights$V-ASM_Staticlouvain_weights$vStar
ASM_Staticlouvain_weights$Prop_obs_est <- ASM_Staticlouvain_weights$V/ASM_Staticlouvain_weights$vStar

ASM_Staticlouvain_weights$Positive_obs_est <- ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0,"Positive","Negative")

ASM_Staticlouvain_weights$PoissonProb <- ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0, 1-(ppois( ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar)), ppois( ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=TRUE, log.p=FALSE)-1)
ASM_Staticlouvain_weights$PoissonProb_log <- ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0, -log(ppois(ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, log = FALSE)), ppois( ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=TRUE, log.p=TRUE))

ASM_Staticlouvain_weights$PoissonProbDensity <- ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0, 1- dpois (ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar), dpois (ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar)-1)
ASM_Staticlouvain_weights$PoissonProbDensityLOG <- ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0, - dpois ( ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, log = TRUE), dpois ( ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, log = TRUE))

ASM_Staticlouvain_weights$chi <- ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0, -log(ppois(ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, log = FALSE)), -ppois( ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=TRUE, log.p=TRUE))
ASM_Staticlouvain_weights$PoissonProb_original <- ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0, ppois(ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, log = FALSE), ppois( ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=TRUE, log.p=FALSE))

###the numerator of chi2 is to the power of 2, so will be always positive independent of if V is smaller or greater than vStar
###add the negative signal in the case the observation is lower than the expected - chi2POSNEG
ASM_Staticlouvain_weights$chi2 <- ((ASM_Staticlouvain_weights$V-ASM_Staticlouvain_weights$vStar)^2)/ASM_Staticlouvain_weights$vStar
ASM_Staticlouvain_weights[ ASM_Staticlouvain_weights$vStar==0 ,]$chi2 <- 0
ASM_Staticlouvain_weights$chi2POSNEG <- ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0,ASM_Staticlouvain_weights$chi2 ,-ASM_Staticlouvain_weights$chi2 )
minChi <- min(ASM_Staticlouvain_weights$chi2POSNEG)
maxChi <- max(ASM_Staticlouvain_weights$chi2POSNEG)
ASM_Staticlouvain_weights$chi2POSNEG_norm <-  ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0, sqrt(ASM_Staticlouvain_weights$chi2) ,-sqrt(ASM_Staticlouvain_weights$chi2) )	
remove(minChi)
remove(maxChi)


head(ASM_Staticlouvain_weights)



#get the most relevant hastags by party
#mostUsedHashOfClusterlouvain_weights <- subset(ASM_Staticlouvain_weights, (ASM_Staticlouvain_weights$V!=0 ) &( ASM_Staticlouvain_weights$PoissonProb_original<0.001))
#mostUsedHashOfClusterlouvain_weights <- subset(ASM_Staticlouvain_weights, (ASM_Staticlouvain_weights$V!=0 | ASM_Staticlouvain_weights$Diff_obs_est< (-50)) &(ASM_Staticlouvain_weights$Prop_obs_est<0.25 | ASM_Staticlouvain_weights$Prop_obs_est>4))
##only the interesting ones by being higher than expected

threshold <- quantile(ASM_Staticlouvain_weights$chi2POSNEG_norm, 0.75)
mostUsedHashOfClusterlouvain_weights <- subset(ASM_Staticlouvain_weights, (ASM_Staticlouvain_weights$V!=0 ) &( ASM_Staticlouvain_weights$chi2POSNEG_norm>threshold))
mostUsedHashOfClusterlouvain_weights$louvain_weights <- as.factor(mostUsedHashOfClusterlouvain_weights$louvain_weights)
remove(threshold)

plot_ly(mostUsedHashOfClusterlouvain_weights, x = ~V, y = ~vStar,text= ~Party, type="scatter" , mode = "markers", color = ~louvain_weights, size = ~chi2POSNEG_norm)
mostUsedHashOfClusterlouvain_weights[order(mostUsedHashOfClusterlouvain_weights$louvain_weights),c("louvain_weights","Party", "V", "vStar", "chi2POSNEG")]

#### SEE FOR EACH GROUP WHAT IS THERE
groups_hashtags[groups_hashtags$louvain_weights==1,]

####still not a poisson distribution
int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}
int.hist(subset(ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$V<500))



##################################################################################################################################################
##for louvain_NOweights
################################### ASM MODEL ################################################################################
###DO the joins to calculate v* - STATIC APPROACH
#calculate for all cobinations of party and cluster
ASM_Staticlouvain_NOweights <- merge(x= hash_party_._., y = hash_._._louvain_NOweights,  by= NULL)
names(ASM_Staticlouvain_NOweights)[names(ASM_Staticlouvain_NOweights)=="count.x"] <- "count_party"
names(ASM_Staticlouvain_NOweights)[names(ASM_Staticlouvain_NOweights)=="count.y"] <- "count_louvain_NOweights"
ASM_Staticlouvain_NOweights <- merge(x= ASM_Staticlouvain_NOweights, y = hash_party_._louvain_NOweights, by = c("Party", "louvain_NOweights"), all.x = TRUE)
names(ASM_Staticlouvain_NOweights)[names(ASM_Staticlouvain_NOweights)=="count"] <- "V"

##if it is NA means the observation is zero
ASM_Staticlouvain_NOweights$V[is.na(ASM_Staticlouvain_NOweights$V)] <- 0
##how many combinations user x hashtag have no observations
nrow(ASM_Staticlouvain_NOweights[ASM_Staticlouvain_NOweights$V==0,])


ASM_Staticlouvain_NOweights$div <- (ASM_Staticlouvain_NOweights$count_louvain_NOweights/hash_._._.) 
ASM_Staticlouvain_NOweights$vStar <- ASM_Staticlouvain_NOweights$div * ASM_Staticlouvain_NOweights$count_party
head(ASM_Staticlouvain_NOweights)

##percentage of the ALL records that were kept for the ASM model (given that it only keeps relevant hashtags - just the first two filters are implemented)
sum(ASM_Staticlouvain_NOweights$V) / nrow(hashtagsAnnotated)
##ASM here keeps just users and hashtags that were present in the original ASM model
##percentage of data kept given just the filtered hashtags
sum(ASM_Staticlouvain_NOweights$V) / nrow(subset(hashtagsAnnotated, hashtagsAnnotated$Hashtag %in% filter_hashtags$Hashtag))


##Calculate observations-estimatives and observations/estimatives and poisson probabilities
ASM_Staticlouvain_NOweights$Diff_obs_est <- ASM_Staticlouvain_NOweights$V-ASM_Staticlouvain_NOweights$vStar
ASM_Staticlouvain_NOweights$Prop_obs_est <- ASM_Staticlouvain_NOweights$V/ASM_Staticlouvain_NOweights$vStar

ASM_Staticlouvain_NOweights$Positive_obs_est <- ifelse(ASM_Staticlouvain_NOweights$Diff_obs_est>=0,"Positive","Negative")

ASM_Staticlouvain_NOweights$PoissonProb <- ifelse(ASM_Staticlouvain_NOweights$Diff_obs_est>=0, 1-(ppois( ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar)), ppois( ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, lower.tail=TRUE, log.p=FALSE)-1)
ASM_Staticlouvain_NOweights$PoissonProb_log <- ifelse(ASM_Staticlouvain_NOweights$Diff_obs_est>=0, -log(ppois(ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, log = FALSE)), ppois( ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, lower.tail=TRUE, log.p=TRUE))

ASM_Staticlouvain_NOweights$PoissonProbDensity <- ifelse(ASM_Staticlouvain_NOweights$Diff_obs_est>=0, 1- dpois (ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar), dpois (ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar)-1)
ASM_Staticlouvain_NOweights$PoissonProbDensityLOG <- ifelse(ASM_Staticlouvain_NOweights$Diff_obs_est>=0, - dpois ( ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, log = TRUE), dpois ( ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, log = TRUE))

ASM_Staticlouvain_NOweights$chi <- ifelse(ASM_Staticlouvain_NOweights$Diff_obs_est>=0, -log(ppois(ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, log = FALSE)), -ppois( ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, lower.tail=TRUE, log.p=TRUE))
ASM_Staticlouvain_NOweights$PoissonProb_original <- ifelse(ASM_Staticlouvain_NOweights$Diff_obs_est>=0, ppois(ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, log = FALSE), ppois( ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$vStar, lower.tail=TRUE, log.p=FALSE))

###the numerator of chi2 is to the power of 2, so will be always positive independent of if V is smaller or greater than vStar
###add the negative signal in the case the observation is lower than the expected - chi2POSNEG
ASM_Staticlouvain_NOweights$chi2 <- ((ASM_Staticlouvain_NOweights$V-ASM_Staticlouvain_NOweights$vStar)^2)/ASM_Staticlouvain_NOweights$vStar
ASM_Staticlouvain_NOweights[ ASM_Staticlouvain_NOweights$vStar==0 ,]$chi2 <- 0
ASM_Staticlouvain_NOweights$chi2POSNEG <- ifelse(ASM_Staticlouvain_NOweights$Diff_obs_est>=0,ASM_Staticlouvain_NOweights$chi2 ,-ASM_Staticlouvain_NOweights$chi2 )



head(ASM_Staticlouvain_NOweights)



#get the most relevant hastags by party
#mostUsedHashOfClusterlouvain_NOweights <- subset(ASM_Staticlouvain_NOweights, (ASM_Staticlouvain_NOweights$V!=0 ) &( ASM_Staticlouvain_NOweights$PoissonProb_original<0.001))
#mostUsedHashOfClusterlouvain_NOweights <- subset(ASM_Staticlouvain_NOweights, (ASM_Staticlouvain_NOweights$V!=0 | ASM_Staticlouvain_NOweights$Diff_obs_est< (-50)) &(ASM_Staticlouvain_NOweights$Prop_obs_est<0.25 | ASM_Staticlouvain_NOweights$Prop_obs_est>4))
##only the interesting ones by being higher than expected

threshold <- quantile(ASM_Staticlouvain_NOweights$chi2POSNEG, 0.75)
mostUsedHashOfClusterlouvain_NOweights <- subset(ASM_Staticlouvain_NOweights, (ASM_Staticlouvain_NOweights$V!=0 ) &( ASM_Staticlouvain_NOweights$chi2POSNEG>threshold))
mostUsedHashOfClusterlouvain_NOweights$louvain_NOweights <- as.factor(mostUsedHashOfClusterlouvain_NOweights$louvain_NOweights)
remove(threshold)

plot_ly(mostUsedHashOfClusterlouvain_NOweights, x = ~V, y = ~vStar,text= ~Party, type="scatter" , mode = "markers", color = ~louvain_NOweights, size = ~chi2POSNEG)
mostUsedHashOfClusterlouvain_NOweights[order(mostUsedHashOfClusterlouvain_NOweights$louvain_NOweights),c("louvain_NOweights","Party", "V", "vStar", "chi2POSNEG")]

#### SEE FOR EACH GROUP WHAT IS THERE
groups_hashtags[groups_hashtags$louvain_NoWeights==1,]

####still not a poisson distribution
int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}
int.hist(subset(ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$V<500))







############################## save results in csv########################

####all clusters composition

groups_hashtagsSAVE <- groups_hashtags
groups_hashtagsSAVE$wardGroups <- NULL
groups_hashtagsSAVE$louvain_NoWeights <- as.numeric(groups_hashtagsSAVE$louvain_NoWeights)
groups_hashtagsSAVE <- groups_hashtagsSAVE[order(groups_hashtagsSAVE$louvain_NoWeights),]
setwd("C:\\Users\\Mariana\\Desktop\\images_report")
write.csv(groups_hashtagsSAVE, 'groups_hashtagsSAVE.csv', fileEncoding="utf-8")

remove(groups_hashtagsSAVE)

###################Surprising values

#threshold <- quantile(ASM_Staticlouvain_NOweights$chi2POSNEG, 0.9)
threshold <- 5000
mostUsedUsersOfClusterLouvainN <- subset(ASM_Staticlouvain_NOweights, (ASM_Staticlouvain_NOweights$V!=0 ) &( ASM_Staticlouvain_NOweights$chi2POSNEG>threshold))

mostUsedUsersOfClusterLouvainN <- mostUsedUsersOfClusterLouvainN[c("louvain_NOweights", "V","vStar","chi2POSNEG", "Party")]
mostUsedUsersOfClusterLouvainN$vStar <- round(mostUsedUsersOfClusterLouvainN$vStar, 2)
mostUsedUsersOfClusterLouvainN$chi2POSNEG <- round(mostUsedUsersOfClusterLouvainN$chi2POSNEG, 2)
mostUsedUsersOfClusterLouvainN$louvain_NOweights <- as.numeric(mostUsedUsersOfClusterLouvainN$louvain_NOweights)

remove(threshold)

plot_ly(mostUsedUsersOfClusterLouvainN, x = ~V, y = ~vStar,text= ~Party, type="scatter" , mode = "markers", color = ~louvain_NOweights, size = ~chi2POSNEG)
mostUsedUsersOfClusterLouvainN[,c("louvain_NOweights","Party", "V", "vStar", "chi2POSNEG")]
mostUsedUsersOfClusterLouvainN <- mostUsedUsersOfClusterLouvainN[order(mostUsedUsersOfClusterLouvainN$louvain_NOweights),]

setwd("C:\\Users\\Mariana\\Desktop\\images_report")
fwrite(mostUsedUsersOfClusterLouvainN, file="mostUsedPartiessOfClusterLouvainN.csv")


remove(mostUsedUsersOfClusterLouvainN)



#####clean the workspace
remove(hash_party_._.)
remove(hash_party_._ward)
remove(hash_party_._wardCorr)
remove(hash_party_._louvain_weights)
remove(hash_party_._louvain_NOweights)

remove(hash_._._ward)
remove(hash_._._wardCorr)
remove(hash_._._louvain_weights)
remove(hash_._._louvain_NOweights)

remove(ASM_StaticwardCorr)
remove(ASM_Staticlouvain_weights)
remove(ASM_Staticlouvain_NOweights)