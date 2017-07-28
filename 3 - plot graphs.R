###libraries
library(maptools)
library(rCarto)
library(RColorBrewer)
library(statnet)
library(reshape)
library(plotly)
library(rgexf)
library(data.table)
library(igraph)
library(NMI)


##path
setwd("C:\\Users\\Mariana\\Desktop\\twittercube_hashtag\\graphs")

#############################################################################################################
###HASHTAGS
ASM_Static <- ASM_Static_hashtags

#correlations_hashtags <- data.frame(correlations_hashtags)

######################################################################################################################
##other alternatives to plot the hashtags graph:
##igraph of the distance matrix found using the quantile for the distance
##export to gephi

require(igraph)
dis <- (1-correlations_hashtags)/2
##remove correlations to self and the simetric ones
linkh <- melt(dis)
names(linkh) <- c("i","j","DISij")
threshold <- quantile(linkh$DISij, 0.03)

linkh <- linkh[linkh$DISij < threshold,]
linkh <- linkh[linkh$i != linkh$j,]
linkh <- data.frame(t(apply(linkh,1,sort)))
linkh <- linkh[!duplicated(linkh),]
linkh$node1 <- linkh$X2
linkh$node2 <- linkh$X3
linkh$corr <- linkh$X1
linkh$X1<- NULL
linkh$X2<- NULL
linkh$X3<- NULL
igraphNetworkh<-graph.data.frame(linkh, directed=F)


V(igraphNetworkh) #prints the list of vertices 
E(igraphNetworkh) #prints the list of edges)
centralization.degree(igraphNetworkh)
sum(centralization.degree(igraphNetworkh)$res) #total nb of edges (considering both directions since we are counting the degree of each node)
#plot(igraphNetworkh) 


g1.gexf <- igraph.to.gexf(igraphNetworkh)

# You have to create a file connection.
f <- file("edgesHashtag1.gexf")
writeLines(g1.gexf$graph, con = f)
close(f)

remove(f)
remove(g1.gexf)
remove(igraphNetworkh)
remove(threshold)
remove(linkh)

######################################################################################################################
##other alternatives to plot the hashtags graph:
##igraph of the distance matrix found using the quantile for the distance
##do the louvain clustering
##export to gephi the graph and the clustering
####try to plot louvain

require(igraph)

##remove correlations to self and the simetric ones
dis <- correlations_hashtags
linkh <- melt(dis)
names(linkh) <- c("i","j","DISij")
threshold <- 0.45

##keep just the closest ones; ones between different nodes (no edges to); remove duplicates (both ways of the correlation)
linkh <- linkh[linkh$DISij > threshold,]
linkh <- linkh[linkh$i != linkh$j,]
linkh <- data.frame(t(apply(linkh,1,sort)))
linkh <- linkh[!duplicated(linkh),]
linkh$node1 <- linkh$X2
linkh$node2 <- linkh$X3
linkh$corr <- linkh$X1
linkh$X1<- NULL
linkh$X2<- NULL
linkh$X3<- NULL
igraphNetworkh<-graph.data.frame(linkh, directed=F)

imc <- cluster_louvain(igraphNetworkh, weights = NA)
#membership(imc)
communities(imc)



plot(imc, igraphNetworkh, col = membership(imc),
     mark.groups = communities(imc))


a <- data.table(cbind(V(igraphNetworkh)$name,imc$membership))
names(a)[names(a)=="V1"] <- "Hashtag"
names(a)[names(a)=="V2"] <- "louvain_NoWeights"
###correct names that were added an X in the correlation table for starting with a number
#a[Hashtag=="3moispourgagner"]$Hashtag <- "3moispourgagner"


##add the louvain clustering to the clusterings done
##RUN ONCE
groups_hashtags <- merge(groups_hashtags, a, by = c("Hashtag"), all.x = TRUE) 
groups_hashtags[order(groups_hashtags$louvain),]
remove(a)

V(igraphNetworkh)$cluster <- membership(imc)
V(igraphNetworkh)$cluster <- as.character(V(igraphNetworkh)$cluster)

# You have to create a file connection.
g1.gexf <- igraph.to.gexf(igraphNetworkh)
f <- file("edgesHashtag2_clusterplot.gexf")
writeLines(g1.gexf$graph, con = f)
close(f)


remove(f)
remove(g1.gexf)
remove(igraphNetworkh)
remove(linkh)
remove(imc)
remove(threshold)
remove(dis)

######################################################################################################################
##other alternatives to plot the hashtags graph:
##igraph using the correlation matrix - everything that is more than 0.5
##try keeping the weights on the edges
##export to gephi the graph and edge weights
####try to plot graph with weights


igraphNetworkh<- graph.adjacency(correlations_hashtags, mode="undirected", weighted=TRUE)
igraphNetworkh<- simplify(igraphNetworkh) ##remove repeated edges
##edges to delete - correlation lower than 0,5
toRemove <- subset(E(igraphNetworkh),E(igraphNetworkh)$weight<0.3)
igraphNetworkh <- delete_edges(igraphNetworkh,toRemove)

##weights - Optional positive weight vector.
imc <- cluster_louvain(igraphNetworkh, weights = E(igraphNetworkh)$weight)


a <- data.table(cbind(V(igraphNetworkh)$name,imc$membership))
names(a)[names(a)=="V1"] <- "Hashtag"
names(a)[names(a)=="V2"] <- "louvain_weights"
###correct names that were added an X in the correlation table for starting with a number
a[Hashtag=="X3moispourgagner"]$Hashtag <- "3moispourgagner"


membership(imc)
communities(imc)



##add the louvain clustering to the clusterings done
##RUN ONCE
groups_hashtags <- merge(groups_hashtags, a, by = c("Hashtag"), all.x = TRUE) 
groups_hashtags[order(groups_hashtags$louvain),]
remove(a)


aux1 <- data.frame((V(igraphNetworkh)$name))
aux2 <- groups_hashtags
names(aux1)<- c("Hashtag")
aux1[[1]] <- as.character(aux1[[1]])
###correct names that were added an X in the correlation table for starting with a number
#aux1[aux1=="X3moispourgagner"] <- "3moispourgagner"

####just to make sure the order is the same as the igraph to merge the clusters there well
a <- merge(aux1 ,aux2, by = c("Hashtag"), all.x = TRUE )
###we are using wardGroups to plot in gephi, can chose another one HERE
a <- as.vector(a$wardGroups)%>% as_membership
remove(aux1)
remove(aux2)

plot(imc, igraphNetworkh, col = membership(imc),
     mark.groups = communities(imc))

######add the cluster to the igraph
V(igraphNetworkh)$cluster <- a


# You have to create a file connection.
g1.gexf <- igraph.to.gexf(igraphNetworkh)
f <- file("edgesHashtag3_weights.gexf")
writeLines(g1.gexf$graph, con = f)
close(f)


remove(f)
remove(g1.gexf)
remove(igraphNetworkh)
remove(toRemove)
remove(imc)
remove(a)







#############################################################################################################
##USERS

#correlations_users <- data.frame(correlations_users)
ASM_Static <- ASM_Static_users

######################################################################################################################
##other alternatives to plot the hashtags graph:
##igraph of the distance matrix found using the quantile for the distance
##export to gephi

require(igraph)
dis <- (1-correlations_users)/2
##remove correlations to self and the simetric ones
linkh <- melt(dis)
names(linkh) <- c("i","j","DISij")
threshold <- quantile(linkh$DISij, 0.03)

linkh <- linkh[linkh$DISij < threshold,]
linkh <- linkh[linkh$i != linkh$j,]
linkh <- data.frame(t(apply(linkh,1,sort)))
linkh <- linkh[!duplicated(linkh),]
linkh$node1 <- linkh$X2
linkh$node2 <- linkh$X3
linkh$corr <- linkh$X1
linkh$X1<- NULL
linkh$X2<- NULL
linkh$X3<- NULL
igraphNetworkh<-graph.data.frame(linkh, directed=F)


V(igraphNetworkh) #prints the list of vertices 
E(igraphNetworkh) #prints the list of edges)
centralization.degree(igraphNetworkh)
sum(centralization.degree(igraphNetworkh)$res) #total nb of edges (considering both directions since we are counting the degree of each node)
#plot(igraphNetworkh) 


g1.gexf <- igraph.to.gexf(igraphNetworkh)

# You have to create a file connection.
f <- file("edgesUsers1.gexf")
writeLines(g1.gexf$graph, con = f)
close(f)

remove(f)
remove(g1.gexf)
remove(igraphNetworkh)
remove(threshold)
remove(linkh)


######################################################################################################################
##other alternatives to plot the hashtags graph:
##igraph of the distance matrix found using the quantile for the distance
##do the louvain clustering
##export to gephi the graph and the clustering
####try to plot louvain

require(igraph)

##remove correlations to self and the simetric ones
dis <- correlations_users
linkh <- melt(dis)
names(linkh) <- c("i","j","DISij")
threshold <- 0.5

linkh <- linkh[linkh$DISij > threshold,]
linkh <- linkh[linkh$i != linkh$j,]
linkh <- data.frame(t(apply(linkh,1,sort)))
linkh <- linkh[!duplicated(linkh),]
linkh$node1 <- linkh$X2
linkh$node2 <- linkh$X3
linkh$corr <- linkh$X1
linkh$X1<- NULL
linkh$X2<- NULL
linkh$X3<- NULL
igraphNetworkh<-graph.data.frame(linkh, directed=F)


imc <- cluster_louvain(igraphNetworkh, weights = NA)
#membership(imc)
communities(imc)

# TOO heavy for the whole data
# plot(imc, igraphNetworkh, col = membership(imc),
#      mark.groups = communities(imc))


a <- data.table(cbind(V(igraphNetworkh)$name,imc$membership))
names(a)[names(a)=="V1"] <- "UserID"
names(a)[names(a)=="V2"] <- "louvain_NoWeights"


###RUN ONCE
##add the louvain clustering to the clusterings done
groups_users <- merge(groups_users, a, by = c("UserID"), all.x = TRUE) 
remove(a)

V(igraphNetworkh)$cluster <- membership(imc)


# You have to create a file connection.
g1.gexf <- igraph.to.gexf(igraphNetworkh)
f <- file("edgesUser2_clusterplot.gexf")
writeLines(g1.gexf$graph, con = f)
close(f)


remove(f)
remove(g1.gexf)
remove(igraphNetworkh)
remove(linkh)
remove(imc)
remove(threshold)
remove(dis)

######################################################################################################################
##other alternatives to plot the hashtags graph:
##igraph using the correlation matrix - everything that is more than 0.5
##try keeping the weights on the edges
##export to gephi the graph and edge weights
####try to plot graph with weights


igraphNetworkh<- graph.adjacency(correlations_users, mode="undirected", weighted=TRUE)
igraphNetworkh<- simplify(igraphNetworkh) ##remove repeated edges
##edges to delete - correlation lower than 0,5
toRemove <- subset(E(igraphNetworkh),E(igraphNetworkh)$weight<0.4)
igraphNetworkh <- delete_edges(igraphNetworkh,toRemove)

imc <- cluster_louvain(igraphNetworkh, weights = E(igraphNetworkh)$weight)

a <- data.table(cbind(V(igraphNetworkh)$name,imc$membership))
names(a)[names(a)=="V1"] <- "UserID"
names(a)[names(a)=="V2"] <- "louvain_weights"


membership(imc)
communities(imc)



###RUN ONCE
##add the louvain clustering to the clusterings done
groups_users <- merge(groups_users, a, by = c("UserID"), all.x = TRUE) 
remove(a)



###for the users

aux1 <- data.frame((V(igraphNetworkh)$name))
aux2 <- groups_users
names(aux1)<- c("UserID")
aux1[[1]] <- as.character(aux1[[1]])


####just to make sure the order is the same as the igraph to merge the clusters there well
a <- merge(aux1 ,aux2, by = c("UserID"), all.x = TRUE )
###we are using wardGroups to plot in gephi, can chose another one HERE
a <- as.vector(a$wardGroups) %>% as_membership
remove(aux1)
remove(aux2)



# TOO heavy for the whole data
# 
# plot(imc, igraphNetworkh, col = membership(imc),
#      mark.groups = communities(imc))

######add the cluster to the igraph
V(igraphNetworkh)$cluster <- a


# You have to create a file connection.
g1.gexf <- igraph.to.gexf(igraphNetworkh)
f <- file("edgesUser3_weights.gexf")
writeLines(g1.gexf$graph, con = f)
close(f)


remove(f)
remove(g1.gexf)
remove(igraphNetworkh)
remove(imc)
remove(toRemove)







############################################################
#### NMI - hashtag clusters
####################################### NMI #############################################"
#X and Y - a data frame or matrix whose first column is the node id and the second column is module
X<-data.frame(groups_hashtags$Hashtag, groups_hashtags$wardGroups)
Y<-data.frame(groups_hashtags$Hashtag, groups_hashtags$wardGroupsCorr)
Z<-data.frame(groups_hashtags$Hashtag, groups_hashtags$wardGroupsCosine)
Q<-data.frame(groups_hashtags$Hashtag, groups_hashtags$louvain_NoWeights)
W<-data.frame(groups_hashtags$Hashtag, groups_hashtags$louvain_weights)

NMI(Y,Z)
NMI(Y,Q)
NMI(Y,W)
NMI(Z,Q)
NMI(Z,W)
NMI(W,Q)

remove(X)
remove(Y)
remove(Z)
remove(Q)
remove(W)

############################################################
#### NMI - user clusters
#X<-data.frame(groups_users$UserID, groups_users$wardGroups)
Y<-data.frame(groups_users$UserID, groups_users$wardGroupsCorr)
Z<-data.frame(groups_users$UserID, groups_users$wardGroupsCosine)
Q<-data.frame(groups_users$UserID, groups_users$louvain_NoWeights)
W<-data.frame(groups_users$UserID, groups_users$louvain_weights)
NMI(Y,Z)
NMI(Y,Q)
NMI(Y,W)
NMI(Z,Q)
NMI(Z,W)
NMI(W,Q)
remove(X)
remove(Y)
remove(Z)
remove(Q)
remove(W)

groups_hashtags[order(groups_hashtags$wardGroups),]
groups_hashtags[order(groups_hashtags$wardGroupsCosine),]
groups_hashtags[order(groups_hashtags$louvain_weights),]
groups_hashtags[order(groups_hashtags$louvain_NoWeights),]


groups_users[order(groups_users$louvain_weights),]
groups_users[order(groups_users$louvain_NoWeights),]
groups_users[order(groups_users$wardGroupsCosine),]

