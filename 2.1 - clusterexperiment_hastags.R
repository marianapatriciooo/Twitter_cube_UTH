library(plotly)
library(reshape)
library(maptools)
library(rCarto)
library(RColorBrewer)
library(statnet)
library(reshape)
library(plotly)
library(rgexf)
library(NMI)
library(Hmisc)
library(dbscan)
library(lsa)



##pivot table
head(ASM_Static)
ASM_Static <- ASM_Static_hashtags
pivot_hashtags <- cast(ASM_Static, Hashtag~UserID, value = 'chi2POSNEG_normBYUsers')

#####wards method for clustering
# Ward Hierarchical Clustering
d <- dist(pivot_hashtags[2:dim(pivot_hashtags)[2]], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
wardGroups <- cutree(fit, k=20) # cut tree into X clusters
# draw dendogram with red borders around the X clusters
rect.hclust(fit, k=20, border="red")




groups_hashtags <- pivot_hashtags$Hashtag
groups_hashtags <- data.frame(groups_hashtags)

##pivot table
head(ASM_Static)
pivot_hashtags <- cast(ASM_Static, UserID~Hashtag, value = 'chi2POSNEG_normBYUsers')

groups_hashtags$wardGroups <-  wardGroups
names(groups_hashtags)[names(groups_hashtags)=="groups_hashtags"] <- "Hashtag"
remove(d)
remove(fit)
remove(wardGroups)

groups_hashtags[order(groups_hashtags$wardGroups),][,c("Hashtag","wardGroups")]



##cluster 1 - mostly political and non political hastags
##cluster 2 - mostly sport
##cluster 3 - burkini; conseildetat; sainterita; sisco
##cluster 4 - summer/ paris2024
##cluster 5 - Sarkozy related - ToutPourLaFrance - blog de campagne supervisé par l'équipe de Nicolas Sarkozy #2017
##cluster 6 - president, laws and presidentials
##cluster 7 - primaires
##cluster 8 - rio2016


###########################################correlations####################################################


correlations_hashtags_input <- data.frame(pivot_hashtags[2:length(pivot_hashtags)])
correlations_hashtags <- cor(correlations_hashtags_input, use="pairwise.complete.obs", method="pearson") 
#correlations_hashtags <- data.frame(rcorr(as.matrix(correlations_hashtags_input), type="spearman")[1]) # type can be pearson or spearman
correlations_hashtags_significance <- data.frame(rcorr(as.matrix(correlations_hashtags_input), type="pearson")[3])
remove(correlations_hashtags_input)

for(i in 1:dim(correlations_hashtags)[1]) {
  for(j in 1:dim(correlations_hashtags)[2]) {
   corr <- ifelse(correlations_hashtags_significance[i,j]<=0.05 | is.na(correlations_hashtags_significance[i,j]) ,correlations_hashtags[i,j],0 )
   correlations_hashtags[i,j] <- corr
  }
  remove(corr)
}
remove(correlations_hashtags_significance)

##try clustering on the correlations - hashatgs

# Ward Hierarchical Clustering

###transform into distance matrix
round(correlations_hashtags[1:8,1:8],2)
dis <- (1-correlations_hashtags)/2
round(dis[1:8,1:8],2)
hist(dis)
##d <- dist(correlations_hashtags, method = "euclidean") # distance matrix
fit <- hclust(as.dist(dis), method="ward.D")
plot(fit, cex=0.5,hang=-1) # display dendogram
wardGroupsCorr <- cutree(fit, k=20) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=20, border="red")


wardCorrH <- data.frame(colnames(pivot_hashtags[2:length(pivot_hashtags)]), wardGroupsCorr)
names(wardCorrH)[names(wardCorrH)=="colnames.pivot_hashtags.2.length.pivot_hashtags..."] <- "Hashtag"
wardCorrH[order(wardCorrH$wardGroups),]

groups_hashtags <- merge(x= groups_hashtags, y= wardCorrH, by = "Hashtag", all.x = TRUE) 

groups_hashtags[order(groups_hashtags$wardGroupsCorr),]


####see the most correlated hashtags
aux <- melt(correlations_hashtags)
aux <- aux[aux$value>0.7 & aux$X1!=aux$X2 , 1:3]
aux
remove(aux)


remove(dis)
remove(fit)
remove(wardGroupsCorr)


##chateaurenard - city in france. sarkozy made a speach there in august - a officialisé lundi sa candidature à la primaire de la droite -nicolas-sarkozy-promet-de---retablir-l-autorite---et-condamne-le-burkini
##le79inter - show from 7pm to 9pm on channel inter
##canicule - heat wave
##SainteRita - church on the 15th that were occupied for months and were evacuated last august
##AFP - agence france press
##Sisco - region in france -  La région de Bastia (Corse) a vécu un week-end tendu après une rixe qui a fait cinq blessés, samedi, sur la plage de Sisco. Environ 500 personnes se sont rassemblées, dimanche, devant la préfecture corse aux cris de « On est chez nous » après cette altercation qui pourrait avoir été causée par une affaire de burkini. 
##CotedAzurNow - has a lot of stuff just related to cote d'azure itself
##Toulouse -  has a lot of stuff just related to toulouse itself
#3moispourgagner - Alain Juppé

############################################################# cosine similarity #########################

correlations_hashtags <-  cosine(as.matrix(pivot_hashtags))


round(correlations_hashtags[1:8,1:8],2)
dis <- (1-correlations_hashtags)/2
round(dis[1:8,1:8],2)
hist(dis)
##d <- dist(correlations_hashtags, method = "euclidean") # distance matrix
fit <- hclust(as.dist(dis), method="ward.D")
plot(fit, cex=0.5,hang=-1) # display dendogram
wardGroupsCosine <- cutree(fit, k=20) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=20, border="red")


wardCosineH <- data.frame(colnames(pivot_hashtags[2:length(pivot_hashtags)]), wardGroupsCosine)
names(wardCosineH)[names(wardCosineH)=="colnames.pivot_hashtags.2.length.pivot_hashtags..."] <- "Hashtag"
wardCosineH[order(wardCosineH$wardGroupsCosine),]



groups_hashtags <- merge(x= groups_hashtags, y= wardCosineH, by = "Hashtag", all.x = TRUE) 

groups_hashtags[order(groups_hashtags$wardGroupsCosine),]


####see the most correlated hashtags
aux <- melt(correlations_hashtags)
aux <- aux[aux$value>0.7 & aux$X1!=aux$X2 , 1:3]
aux
remove(aux)


remove(dis)
remove(fit)
remove(wardGroupsCosine)
remove(wardCosineH)










############# test - check if the very correlated ones  portray a similar behaviour on the ASM_Static table

subset(ASM_Static[,c("UserID","Hashtag","V", "vStar", "chi2POSNEG", "chi2POSNEG_normBYUsers" )], ASM_Static$Hashtag =="AJ2017" & ASM_Static$V>0)
subset(ASM_Static[,c("UserID","Hashtag","V", "vStar", "chi2POSNEG", "chi2POSNEG_normBYUsers" )], ASM_Static$Hashtag =="BourdinDirect" & ASM_Static$V>0)


aux <- melt(correlations_hashtags)
aux[aux$X1=="BourdinDirect" & aux$X2=="AFP",]
remove(aux)

####################################### NMI #############################################"
#X and Y - a data frame or matrix whose first column is the node id and the second column is module
X<-data.frame(groups_hashtags$Hashtag, groups_hashtags$wardGroups)
Y<-data.frame(groups_hashtags$Hashtag, groups_hashtags$wardGroupsCorr)
Z<-data.frame(groups_hashtags$Hashtag, groups_hashtags$wardGroupsCosine)
NMI(X,Y)
NMI(X,Z)
NMI(Y,Z)
remove(X)
remove(Y)
remove(Z)
