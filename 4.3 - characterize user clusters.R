###groupings by cluster and by cluster and time

aux <- merge(x= groups_users, y = notFiltered_hash_u_._h, by = c("UserID"), all.x = TRUE)

hash_ward_._. <- aggregate(aux$count, by=list(aux$wardGroupsCosine), FUN=sum)
names(hash_ward_._.)[names(hash_ward_._.)=="Group.1"] <- "wardGroupsCosine"
names(hash_ward_._.)[names(hash_ward_._.)=="x"] <- "count"

hash_ward_._h <- aggregate(aux$count, by=list(aux$wardGroupsCosine, aux$Hashtag), FUN=sum)
names(hash_ward_._h)[names(hash_ward_._h)=="Group.1"] <- "wardGroupsCosine"
names(hash_ward_._h)[names(hash_ward_._h)=="Group.2"] <- "Hashtag"
names(hash_ward_._h)[names(hash_ward_._h)=="x"] <- "count"




hash_louvain_weights_._. <- aggregate(aux$count, by=list(aux$louvain_weights), FUN=sum)
names(hash_louvain_weights_._.)[names(hash_louvain_weights_._.)=="Group.1"] <- "louvain_weights"
names(hash_louvain_weights_._.)[names(hash_louvain_weights_._.)=="x"] <- "count"

hash_louvain_weights_._h <- aggregate(aux$count, by=list(aux$louvain_weights, aux$Hashtag), FUN=sum)
names(hash_louvain_weights_._h)[names(hash_louvain_weights_._h)=="Group.1"] <- "louvain_weights"
names(hash_louvain_weights_._h)[names(hash_louvain_weights_._h)=="Group.2"] <- "Hashtag"
names(hash_louvain_weights_._h)[names(hash_louvain_weights_._h)=="x"] <- "count"


hash_louvain_NOweights_._. <- aggregate(aux$count, by=list(aux$louvain_NoWeights), FUN=sum)
names(hash_louvain_NOweights_._.)[names(hash_louvain_NOweights_._.)=="Group.1"] <- "louvain_NOweights"
names(hash_louvain_NOweights_._.)[names(hash_louvain_NOweights_._.)=="x"] <- "count"

hash_louvain_NOweights_._h <- aggregate(aux$count, by=list(aux$louvain_NoWeights, aux$Hashtag), FUN=sum)
names(hash_louvain_NOweights_._h)[names(hash_louvain_NOweights_._h)=="Group.1"] <- "louvain_NOweights"
names(hash_louvain_NOweights_._h)[names(hash_louvain_NOweights_._h)=="Group.2"] <- "Hashtag"
names(hash_louvain_NOweights_._h)[names(hash_louvain_NOweights_._h)=="x"] <- "count"

remove(aux)


##################################################################################################################################################
##for ward - COSINE
################################### ASM MODEL ################################################################################
###DO the joins to calculate v* - STATIC APPROACH

#calculate for all cobinations of user and hashtag
ASM_StaticwardCosine <- merge(x= hash_ward_._., y = hash_._._h,  by= NULL)
names(ASM_StaticwardCosine)[names(ASM_StaticwardCosine)=="count.x"] <- "count_wardCosine"
names(ASM_StaticwardCosine)[names(ASM_StaticwardCosine)=="count.y"] <- "count_h"
ASM_StaticwardCosine <- merge(x= ASM_StaticwardCosine, y = hash_ward_._h, by = c("wardGroupsCosine", "Hashtag"), all.x = TRUE)
names(ASM_StaticwardCosine)[names(ASM_StaticwardCosine)=="count"] <- "V"

##if it is NA means the observation is zero
ASM_StaticwardCosine$V[is.na(ASM_StaticwardCosine$V)] <- 0
##how many combinations user x hashtag have no observations
nrow(ASM_StaticwardCosine[ASM_StaticwardCosine$V==0,])





ASM_StaticwardCosine$div <- (ASM_StaticwardCosine$count_h/hash_._._.) 
ASM_StaticwardCosine$vStar <- ASM_StaticwardCosine$div * ASM_StaticwardCosine$count_wardCosine
head(ASM_StaticwardCosine)

##percentage of the ALL records that were kept for the ASM model (given that it only keeps relevant hashtags - just the first two filters are implemented)
sum(ASM_StaticwardCosine$V) / nrow(hashtagsAnnotated)
##ASM here keeps just users and hashtags that were present in the original ASM model
##percentage of data kept given just the filtered hashtags
sum(ASM_StaticwardCosine$V) / nrow(subset(hashtagsAnnotated, hashtagsAnnotated$Hashtag %in% filter_hashtags$Hashtag))


##Calculate observations-estimatives and observations/estimatives and poisson probabilities
ASM_StaticwardCosine$Diff_obs_est <- ASM_StaticwardCosine$V - ASM_StaticwardCosine$vStar
ASM_StaticwardCosine$Prop_obs_est <- ASM_StaticwardCosine$V/ASM_StaticwardCosine$vStar

ASM_StaticwardCosine$Positive_obs_est <- ifelse(ASM_StaticwardCosine$Diff_obs_est>=0,"Positive","Negative")
ASM_StaticwardCosine$PoissonProb <- ifelse(ASM_StaticwardCosine$Diff_obs_est>=0, 1-(ppois( ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar)), ppois( ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, lower.tail=TRUE, log.p=FALSE)-1)
ASM_StaticwardCosine$PoissonProb_log <- ifelse(ASM_StaticwardCosine$Diff_obs_est>=0, -log(ppois(ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, log = FALSE)), ppois( ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, lower.tail=TRUE, log.p=TRUE))

ASM_StaticwardCosine$PoissonProbDensity <- ifelse(ASM_StaticwardCosine$Diff_obs_est>=0, 1- dpois (ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar), dpois (ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar)-1)
ASM_StaticwardCosine$PoissonProbDensityLOG <- ifelse(ASM_StaticwardCosine$Diff_obs_est>=0, - dpois ( ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, log = TRUE), dpois ( ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, log = TRUE))

ASM_StaticwardCosine$PoissonProbLOG_size <- ifelse(ASM_StaticwardCosine$Diff_obs_est>=0, -log(ppois(ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, log = FALSE)), -ppois( ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, lower.tail=TRUE, log.p=TRUE))
ASM_StaticwardCosine$PoissonProb_original <- ifelse(ASM_StaticwardCosine$Diff_obs_est>=0, ppois(ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, log = FALSE), ppois( ASM_StaticwardCosine$V, ASM_StaticwardCosine$vStar, lower.tail=TRUE, log.p=FALSE))

###the numerator of chi2 is to the power of 2, so will be always positive independent of if V is smaller or greater than vStar
###add the negative signal in the case the observation is lower than the expected - chi2POSNEG
ASM_StaticwardCosine$chi2 <- ((ASM_StaticwardCosine$V-ASM_StaticwardCosine$vStar)^2)/ASM_StaticwardCosine$vStar
ASM_StaticwardCosine[ ASM_StaticwardCosine$vStar==0 ,]$chi2 <- 0
ASM_StaticwardCosine$chi2POSNEG <- ifelse(ASM_StaticwardCosine$Diff_obs_est>=0,ASM_StaticwardCosine$chi2 ,-ASM_StaticwardCosine$chi2 )

head(ASM_StaticwardCosine)



#get the most relevant hastags by party
#mostUsedHashOfClusterwardCosine <- subset(ASM_StaticwardCosine, (ASM_StaticwardCosine$V!=0 ) &( ASM_StaticwardCosine$PoissonProb_original<0.001))
#mostUsedHashOfClusterwardCosine <- subset(ASM_StaticwardCosine, (ASM_StaticwardCosine$V!=0 | ASM_StaticwardCosine$Diff_obs_est< (-50)) &(ASM_StaticwardCosine$Prop_obs_est<0.25 | ASM_StaticwardCosine$Prop_obs_est>4))
##only the interesting ones by being higher than expected
threshold <- quantile(ASM_StaticwardCosine$chi2POSNEG, 0.98)
mostUsedHashOfClusterwardCosine <- subset(ASM_StaticwardCosine, (ASM_StaticwardCosine$V!=0 ) &( ASM_StaticwardCosine$chi2POSNEG>threshold))
mostUsedHashOfClusterwardCosine$wardGroupsCosine <- as.factor(mostUsedHashOfClusterwardCosine$wardGroupsCosine)
mostUsedHashOfClusterwardCosine <- mostUsedHashOfClusterwardCosine[c("wardGroupsCosine", "V","vStar","chi2POSNEG", "Hashtag")]
mostUsedHashOfClusterwardCosine$vStar <- round(mostUsedHashOfClusterwardCosine$vStar, 2)
mostUsedHashOfClusterwardCosine$chi2POSNEG <- round(mostUsedHashOfClusterwardCosine$chi2POSNEG, 2)


remove(threshold)

plot_ly(mostUsedHashOfClusterwardCosine, x = ~V, y = ~vStar,text= ~Hashtag, type="scatter" , mode = "markers", color = ~wardGroupsCosine, size = ~chi2POSNEG)
mostUsedHashOfClusterwardCosine[,c("wardGroupsCosine","Hashtag", "V", "vStar", "chi2POSNEG")]

setwd("C:\\Users\\Mariana\\Desktop\\images_report")
fwrite(mostUsedHashOfClusterwardCosine, file="mostUsedHashOfClusterwardCosine.csv")


####still not a poisson distribution
int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}
int.hist(subset(ASM_StaticwardCosine$V, ASM_StaticwardCosine$V<50))

##################################################################################################################################################
##for louvain_weights
################################### ASM MODEL ################################################################################
###DO the joins to calculate v* - STATIC APPROACH

#calculate for all cobinations of user and hashtag
ASM_Staticlouvain_weights <- merge(x= hash_louvain_weights_._., y = hash_._._h,  by= NULL)
names(ASM_Staticlouvain_weights)[names(ASM_Staticlouvain_weights)=="count.x"] <- "count_louvain_weights"
names(ASM_Staticlouvain_weights)[names(ASM_Staticlouvain_weights)=="count.y"] <- "count_h"
ASM_Staticlouvain_weights <- merge(x= ASM_Staticlouvain_weights, y = hash_louvain_weights_._h, by = c("louvain_weights", "Hashtag"), all.x = TRUE)
names(ASM_Staticlouvain_weights)[names(ASM_Staticlouvain_weights)=="count"] <- "V"

##if it is NA means the observation is zero
ASM_Staticlouvain_weights$V[is.na(ASM_Staticlouvain_weights$V)] <- 0
##how many combinations user x hashtag have no observations
nrow(ASM_Staticlouvain_weights[ASM_Staticlouvain_weights$V==0,])



ASM_Staticlouvain_weights$div <- (ASM_Staticlouvain_weights$count_h/hash_._._.) 
ASM_Staticlouvain_weights$vStar <- ASM_Staticlouvain_weights$div * ASM_Staticlouvain_weights$count_louvain_weights
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

ASM_Staticlouvain_weights$PoissonProbLOG_size <- ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0, -log(ppois(ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, log = FALSE)), -ppois( ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=TRUE, log.p=TRUE))
ASM_Staticlouvain_weights$PoissonProb_original <- ifelse(ASM_Staticlouvain_weights$Diff_obs_est>=0, ppois(ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=FALSE, log.p=FALSE) + dpois (ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, log = FALSE), ppois( ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$vStar, lower.tail=TRUE, log.p=FALSE))


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
threshold <- quantile(ASM_Staticlouvain_weights$chi2POSNEG_norm, 0.95)
mostUsedHashOfClusterlouvain_weights <- subset(ASM_Staticlouvain_weights, (ASM_Staticlouvain_weights$V!=0 ) &( ASM_Staticlouvain_weights$chi2POSNEG_norm>threshold))
mostUsedHashOfClusterlouvain_weights$louvain_weights <- as.factor(mostUsedHashOfClusterlouvain_weights$louvain_weights)
remove(threshold)

plot_ly(mostUsedHashOfClusterlouvain_weights, x = ~V, y = ~vStar,text= ~Hashtag, type="scatter" , mode = "markers", color = ~louvain_weights, size = ~PoissonProbLOG_size)
mostUsedHashOfClusterlouvain_weights[,c("louvain_weights","Hashtag", "V", "vStar", "chi2POSNEG")]



####still not a poisson distribution
int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}
int.hist(subset(ASM_Staticlouvain_weights$V, ASM_Staticlouvain_weights$V<50))









##################################################################################################################################################
##for louvain_NOweights
################################### ASM MODEL ################################################################################
###DO the joins to calculate v* - STATIC APPROACH
#calculate for all cobinations of user and hashtag
ASM_Staticlouvain_NOweights <- merge(x= hash_louvain_NOweights_._., y = hash_._._h,  by= NULL)
names(ASM_Staticlouvain_NOweights)[names(ASM_Staticlouvain_NOweights)=="count.x"] <- "count_louvain_NOweights"
names(ASM_Staticlouvain_NOweights)[names(ASM_Staticlouvain_NOweights)=="count.y"] <- "count_h"
ASM_Staticlouvain_NOweights <- merge(x= ASM_Staticlouvain_NOweights, y = hash_louvain_NOweights_._h, by = c("louvain_NOweights", "Hashtag"), all.x = TRUE)
names(ASM_Staticlouvain_NOweights)[names(ASM_Staticlouvain_NOweights)=="count"] <- "V"

##if it is NA means the observation is zero
ASM_Staticlouvain_NOweights$V[is.na(ASM_Staticlouvain_NOweights$V)] <- 0
##how many combinations user x hashtag have no observations
nrow(ASM_Staticlouvain_NOweights[ASM_Staticlouvain_NOweights$V==0,])



ASM_Staticlouvain_NOweights$div <- (ASM_Staticlouvain_NOweights$count_h/hash_._._.) 
ASM_Staticlouvain_NOweights$vStar <- ASM_Staticlouvain_NOweights$div * ASM_Staticlouvain_NOweights$count_louvain_NOweights
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
minChi <- min(ASM_Staticlouvain_NOweights$chi2POSNEG)
maxChi <- max(ASM_Staticlouvain_NOweights$chi2POSNEG)
ASM_Staticlouvain_NOweights$chi2POSNEG_norm <-  ifelse(ASM_Staticlouvain_NOweights$Diff_obs_est>=0, sqrt(ASM_Staticlouvain_NOweights$chi2) ,-sqrt(ASM_Staticlouvain_NOweights$chi2) )	
remove(minChi)
remove(maxChi)


head(ASM_Staticlouvain_NOweights)



#get the most relevant hastags by party
#mostUsedHashOfClusterlouvain_NOweights <- subset(ASM_Staticlouvain_NOweights, (ASM_Staticlouvain_NOweights$V!=0 ) &( ASM_Staticlouvain_NOweights$PoissonProb_original<0.001))
#mostUsedHashOfClusterlouvain_NOweights <- subset(ASM_Staticlouvain_NOweights, (ASM_Staticlouvain_NOweights$V!=0 | ASM_Staticlouvain_NOweights$Diff_obs_est< (-50)) &(ASM_Staticlouvain_NOweights$Prop_obs_est<0.25 | ASM_Staticlouvain_NOweights$Prop_obs_est>4))
##only the interesting ones by being higher than expected

threshold <- quantile(ASM_Staticlouvain_NOweights$chi2POSNEG_norm, 0.95)
mostUsedHashOfClusterlouvain_NOweights <- subset(ASM_Staticlouvain_NOweights, (ASM_Staticlouvain_NOweights$V!=0 ) &( ASM_Staticlouvain_NOweights$chi2POSNEG_norm>threshold))
mostUsedHashOfClusterlouvain_NOweights$louvain_NOweights <- as.factor(mostUsedHashOfClusterlouvain_NOweights$louvain_NOweights)
remove(threshold)

plot_ly(mostUsedHashOfClusterlouvain_NOweights, x = ~V, y = ~vStar,text= ~Hashtag, type="scatter" , mode = "markers", color = ~louvain_NOweights, size = ~chi2POSNEG_norm)
mostUsedHashOfClusterlouvain_NOweights[order(mostUsedHashOfClusterlouvain_NOweights$louvain_NOweights),c("louvain_NOweights","Hashtag", "V", "vStar", "chi2POSNEG")]



####still not a poisson distribution
int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}
int.hist(subset(ASM_Staticlouvain_NOweights$V, ASM_Staticlouvain_NOweights$V<500))




#############################################
############################################## save full list of clusters
groups_usersSAVE <- groups_users
groups_usersSAVE$wardGroups <- NULL
groups_usersSAVE$wardGroupsCosine <- as.numeric(groups_usersSAVE$wardGroupsCosine)
groups_usersSAVE <- groups_usersSAVE[order(groups_usersSAVE$wardGroupsCosine),]
setwd("C:\\Users\\Mariana\\Desktop\\images_report")
write.csv(groups_usersSAVE, 'groups_usersSAVE.csv', fileEncoding="utf-8")

remove(groups_usersSAVE)



#####clean the workspace
remove(hash_ward_._h)
remove(hash_ward_._.)
remove(ASM_StaticWard)

remove(hash_wardCorr_._h)
remove(hash_wardCorr_._.)
remove(ASM_StaticwardCorr)

remove(hash_louvain_weights_._h)
remove(hash_louvain_weights_._.)
remove(ASM_Staticlouvain_weights)

remove(hash_louvain_NOweights_._h)
remove(hash_louvain_NOweights_._.)
remove(ASM_Staticlouvain_NOweights)