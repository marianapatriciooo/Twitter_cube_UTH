library(NMI)
# Prepare Data
library(reshape)
library(dplyr)
library(lubridate)
library(dplyr)
library(data.table)
library(plyr)
library(igraph)
library(modMax)
library(flexclust)
library(lsa)

ASM_Static <- ASM_Static_users

##pivot table
head(ASM_Static)
pivot_users <- cast(ASM_Static, UserID~Hashtag, value = 'chi2POSNEG_normBYhashtags')



#####wards method for hierarchical clustering################################################################################
# Ward Hierarchical Clustering
d <- dist(pivot_users[2:dim(pivot_users)[2]], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
wardGroups <- cutree(fit, k=15) # cut tree into X clusters
# draw dendogram with red borders around the X clusters
rect.hclust(fit, k=15, border="red")


groups_users <- pivot_users$UserID
groups_users <- data.frame(groups_users)

##pivot table
head(ASM_Static)
pivot_users <- cast(ASM_Static, Hashtag~UserID, value = 'chi2POSNEG_normBYhashtags')


##get user id
names(groups_users)[names(groups_users)=="groups_users"] <- "UserID"
groups_users <-  merge(x = groups_users, y = import_tracked_userID, by = "UserID" , all.x = TRUE)


groups_users$wardGroups <-  wardGroups




##aggreagted value on clusters and parties
ward_clusterVSParties <- aggregate(groups_users$UserID, by=list(groups_users$wardGroups, groups_users$Party) ,FUN=length)
ward_clusterVSParties[order(ward_clusterVSParties$Group.1),]
names(ward_clusterVSParties)[names(ward_clusterVSParties)=="Group.1"] <- "wardGroups"
names(ward_clusterVSParties)[names(ward_clusterVSParties)=="Group.2"] <- "Party"

ward_clusterVSParties$wardGroups <- as.character(ward_clusterVSParties$wardGroups)

##bar charts
library(plotly)
library(dplyr)
##parties by cluster
ward_clusterVSParties%>%plot_ly(x = ~wardGroups, y = ~x, color = ~Party, type = 'bar')
##clusters by party
ward_clusterVSParties%>%plot_ly(x = ~Party, y = ~x, color = ~wardGroups, type = 'bar')

###next try directly with the observations rather than the poisson prob log
##dont think in the geomedia article the correlations are being used to buold the graph, not sure

remove(d)
remove(fit)
remove(wardGroups)

##plot the percentace of clusters per party - EXAMPLE
plot_ly(subset(ward_clusterVSParties, ward_clusterVSParties$wardGroups=="2"), labels = ~Party, values = ~x, type = 'pie',textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste( x, 'users'))%>%
  layout(title = 'Percentage of users by cluster in 1',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))




remove(ward_clusterVSParties)

#########################correlations##################################################################################
#################################################check correlations

correlations_users_input <- data.frame(pivot_users[2:length(pivot_users)])
correlations_users <- cor(correlations_users_input, use="pairwise.complete.obs", method="pearson") 
#correlations_users <- data.frame(rcorr(as.matrix(correlations_users_input), type="spearman")[1]) # type can be pearson or spearman
correlations_users_significance <- data.frame(rcorr(as.matrix(correlations_users_input), type="pearson")[3])
remove(correlations_users_input)

for(i in 1:dim(correlations_users)[1]) {
  for(j in 1:dim(correlations_users)[2]) {
    corr <- ifelse(correlations_users_significance[i,j]<=0.05 | is.na(correlations_users_significance[i,j]) , correlations_users[i,j],0 )
    correlations_users[i,j] <- corr
  }
  remove(corr)
}

remove(correlations_users_significance)

##try clustering on the correlations - users####################################################################
####ward method on the correlations

###transform into distance matrix
round(correlations_users[1:8,1:8],2)
dis <- (1-correlations_users)/2
round(dis[1:8,1:8],2)
hist(dis)




fit <- hclust(as.dist(dis), method="ward.D")
plot(fit, cex=0.5,hang=-1) # display dendogram
wardGroupsCorr <- cutree(fit, k=15) # cut tree into 11 clusters
# draw dendogram with red borders around the 11 clusters
rect.hclust(fit, k=15, border="red")




colnames(pivot_users[2:dim(pivot_users)[2]])
wardCorr <- data.frame(colnames(pivot_users[2:dim(pivot_users)[2]]), wardGroupsCorr)
names(wardCorr) <- c("UserID", "wardGroupsCorr")
wardCorr[order(wardCorr$wardGroups),]

groups_users <- merge(x= groups_users, y= wardCorr, by = "UserID", all.x = TRUE) 

groups_users[order(groups_users$wardGroupsCorr),]
remove(wardCorr)


wardCorr_clusterVSParties <- aggregate(groups_users$User, by=list(groups_users$wardGroupsCorr, groups_users$Party) ,FUN=length)
names(wardCorr_clusterVSParties)[names(wardCorr_clusterVSParties)=="Group.1"] <- "wardGroupsCorr"
names(wardCorr_clusterVSParties)[names(wardCorr_clusterVSParties)=="Group.2"] <- "Party"
wardCorr_clusterVSParties[order(wardCorr_clusterVSParties$wardGroupsCorr),]


remove(dis)
remove(fit)
remove(wardGroupsCorr)

#remove(wardCorr)
#remove(wardCorr_clusterVSParties)


wardCorr_clusterVSParties$wardGroupsCorr <- as.character(wardCorr_clusterVSParties$wardGroupsCorr)
##bar charts
library(plotly)
library(dplyr)
##parties by cluster
wardCorr_clusterVSParties%>%plot_ly(x = ~wardGroupsCorr, y = ~x, color = ~Party, type = 'bar')
##clusters by party
wardCorr_clusterVSParties%>%plot_ly(x = ~Party, y = ~x, color = ~wardGroupsCorr, type = 'bar')



##plot the percentace of clusters per party - EXAMPLE
plot_ly(subset(wardCorr_clusterVSParties, wardCorr_clusterVSParties$wardGroupsCorr=="2"), labels = ~Party, values = ~x, type = 'pie',textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste( x, 'users'))%>%
  layout(title = 'Percentage of users by cluster in 1',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



remove(wardCorr_clusterVSParties)


##################################COSINE SIMILARITY###########


correlations_users <-  cosine(as.matrix(pivot_users))


round(correlations_users[1:8,1:8],2)
dis <- (1-correlations_users)/2
round(dis[1:8,1:8],2)
hist(dis)
##d <- dist(correlations_users, method = "euclidean") # distance matrix
fit <- hclust(as.dist(dis), method="ward.D")
plot(fit, cex=0.5,hang=-1) # display dendogram
wardGroupsCosine <- cutree(fit, k=15) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=15, border="red")


wardCosineU <- data.frame(colnames(pivot_users[2:length(pivot_users)]), wardGroupsCosine)
names(wardCosineU)[names(wardCosineU)=="colnames.pivot_users.2.length.pivot_users..."] <- "UserID"
wardCosineU[order(wardCosineU$wardGroupsCosine),]



groups_users <- merge(x= groups_users, y= wardCosineU, by = "UserID", all.x = TRUE) 

groups_users[order(groups_users$wardGroupsCosine),]


####see the most correlated hashtags
aux <- melt(correlations_users)
aux <- aux[aux$value>0.7 & aux$X1!=aux$X2 , 1:3]
aux
remove(aux)


remove(dis)
remove(fit)
remove(wardGroupsCosine)
remove(wardCosineU)

##aggreagted value on clusters and parties
ward_clusterVSParties <- aggregate(groups_users$UserID, by=list(groups_users$wardGroupsCosine, groups_users$Party) ,FUN=length)
ward_clusterVSParties[order(ward_clusterVSParties$Group.1),]
names(ward_clusterVSParties)[names(ward_clusterVSParties)=="Group.1"] <- "wardGroups"
names(ward_clusterVSParties)[names(ward_clusterVSParties)=="Group.2"] <- "Party"

ward_clusterVSParties$wardGroups <- as.character(ward_clusterVSParties$wardGroups)

##bar charts
library(plotly)
library(dplyr)
##parties by cluster

ward_clusterVSParties$wardGroups <- as.factor(ward_clusterVSParties$wardGroups)
ward_clusterVSParties <- ward_clusterVSParties[order(ward_clusterVSParties$wardGroups),]


ward_clusterVSParties%>%plot_ly(x = ~wardGroups, y = ~x, color = ~Party, type = 'bar')


remove(ward_clusterVSParties)



############# test - check if the very correlated ones  portray a similar behaviour on the ASM_Static table

##one ump and one gauche put in the same cluster by ward on the cosine
subset(ASM_Static[,c("UserID","Hashtag","V", "vStar", "chi2POSNEG", "chi2POSNEG_normBYhashtags" )], ASM_Static$UserID =="bertrandserp" & ASM_Static$V>0)
subset(ASM_Static[,c("UserID","Hashtag","V", "vStar", "chi2POSNEG", "chi2POSNEG_normBYhashtags" )], ASM_Static$UserID =="CaroleDelga" & ASM_Static$V>0)

##one media one ps put in the same cluster by ward on the cosine
subset(ASM_Static[,c("UserID","Hashtag","V", "vStar", "chi2POSNEG", "chi2POSNEG_normBYhashtags" )], ASM_Static$UserID =="20HPolitique" & ASM_Static$V>0)
subset(ASM_Static[,c("UserID","Hashtag","V", "vStar", "chi2POSNEG", "chi2POSNEG_normBYhashtags" )], ASM_Static$UserID =="BEDINISebastien" & ASM_Static$V>0)


##one ump one ps put in the same cluster by ward on the cosine
subset(ASM_Static[,c("UserID","Hashtag","V", "vStar", "chi2POSNEG", "chi2POSNEG_normBYhashtags" )], ASM_Static$UserID =="karampatrick" & ASM_Static$V>0)
subset(ASM_Static[,c("UserID","Hashtag","V", "vStar", "chi2POSNEG", "chi2POSNEG_normBYhashtags" )], ASM_Static$UserID =="Juanico" & ASM_Static$V>0)

aux <- melt(correlations_users)
aux[aux$X1=="CaroleDelga" & aux$X2=="bertrandserp",]
remove(aux)




####################################### NMI #############################################"
#X and Y - a data frame or matrix whose first column is the node id and the second column is module
X<-data.frame(groups_users$UserID, groups_users$wardGroups)
Y<-data.frame(groups_users$UserID, groups_users$wardGroupsCorr)
Z<-data.frame(groups_users$UserID, groups_users$wardGroupsCosine)
NMI(X,Y)
NMI(X,Z)
NMI(Y,Z)
remove(X)
remove(Y)
remove(Z)



####put usersname in groups
a <- notFiltered_hash_u_._.[,1:2]
groups_users <-  merge(x = groups_users, y = a, by = "UserID" , all.x = TRUE)

remove(a)
