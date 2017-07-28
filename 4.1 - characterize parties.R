library(plotly)
###groupings by party and by party and time

hash_p_._. <- aggregate(hashtagsAnnotated$Party, by=list(hashtagsAnnotated$Party), FUN=length)
names(hash_p_._.)[names(hash_p_._.)=="Group.1"] <- "Party"
names(hash_p_._.)[names(hash_p_._.)=="x"] <- "count"


hash_p_._h <- aggregate(hashtagsAnnotated$Party, by=list(hashtagsAnnotated$Party, hashtagsAnnotated$Hashtag), FUN=length)
names(hash_p_._h)[names(hash_p_._h)=="Group.1"] <- "Party"
names(hash_p_._h)[names(hash_p_._h)=="Group.2"] <- "Hashtag"
names(hash_p_._h)[names(hash_p_._h)=="x"] <- "count"

###keep just the hashtags in study
hash_p_._h <- hash_p_._h[(hash_p_._h $Hashtag %in% filter_hashtags$Hashtag),]


################################### ASM MODEL ################################################################################
###DO the joins to calculate v* - STATIC APPROACH




#calculate for all cobinations of user and hashtag
ASM_StaticPARTY <- merge(x= hash_p_._., y = hash_._._h,  by= NULL)
names(ASM_StaticPARTY)[names(ASM_StaticPARTY)=="count.x"] <- "count_p"
names(ASM_StaticPARTY)[names(ASM_StaticPARTY)=="count.y"] <- "count_h"
ASM_StaticPARTY <- merge(x= ASM_StaticPARTY, y = hash_p_._h, by = c("Party", "Hashtag"), all.x = TRUE)
names(ASM_StaticPARTY)[names(ASM_StaticPARTY)=="count"] <- "V"

##if it is NA means the observation is zero
ASM_StaticPARTY$V[is.na(ASM_StaticPARTY$V)] <- 0
##how many combinations user x hashtag have no observations
nrow(ASM_StaticPARTY[ASM_StaticPARTY$V==0,])





ASM_StaticPARTY$div <- (ASM_StaticPARTY$count_h/hash_._._.) 
ASM_StaticPARTY$vStar <- ASM_StaticPARTY$div * ASM_StaticPARTY$count_p
head(ASM_StaticPARTY)


##Calculate observations-estimatives and observations/estimatives and poisson probabilities
ASM_StaticPARTY$Diff_obs_est <- ASM_StaticPARTY$V-ASM_StaticPARTY$vStar
ASM_StaticPARTY$Prop_obs_est <- ASM_StaticPARTY$V/ASM_StaticPARTY$vStar

ASM_StaticPARTY$Positive_obs_est <- ifelse(ASM_StaticPARTY$Diff_obs_est>=0,"Positive","Negative")

ASM_StaticPARTY$chi2 <- ((ASM_StaticPARTY$V-ASM_StaticPARTY$vStar)^2)/ASM_StaticPARTY$vStar
ASM_StaticPARTY$chi2POSNEG <- ifelse(ASM_StaticPARTY$Diff_obs_est>=0,ASM_StaticPARTY$chi2 ,-ASM_StaticPARTY$chi2 )
ASM_StaticPARTY$sqRootchi2POSNEG <- ifelse(ASM_StaticPARTY$chi2POSNEG>=0, sqrt(ASM_StaticPARTY$chi2) , -sqrt(ASM_StaticPARTY$chi2)  )


head(ASM_StaticPARTY)



#get the most relevant hastags by party

threshold <- quantile(ASM_StaticPARTY$sqRootchi2POSNEG, 0.95)

##only the interesting ones by being higher than expected
mostUsedHashOfParties <- subset(ASM_StaticPARTY, (ASM_StaticPARTY$V!=0 ) &( ASM_StaticPARTY$sqRootchi2POSNEG>=threshold))
remove(threshold)

###SEEE THE MAIN ONES
mostUsedHashOfParties[,c("Party","Hashtag", "V", "vStar")]

#####save in file 
mostUsedHashOfPartiesSAVE <- mostUsedHashOfParties[,c("Party","Hashtag", "V", "vStar","chi2")]
mostUsedHashOfPartiesSAVE$vStar <- round(mostUsedHashOfPartiesSAVE$vStar,2)
mostUsedHashOfPartiesSAVE$chi2 <- round(mostUsedHashOfPartiesSAVE$chi2,2)
mostUsedHashOfPartiesSAVE <- mostUsedHashOfPartiesSAVE[order(-mostUsedHashOfPartiesSAVE$chi2),]
setwd("C:\\Users\\Mariana\\Desktop\\images_report")
fwrite(mostUsedHashOfPartiesSAVE, file="mostUsedHashOfPartiesSAVE.csv")
remove(mostUsedHashOfPartiesSAVE)


parties <- aggregate(mostUsedHashOfParties$Party,by = list(mostUsedHashOfParties$Party), FUN=length)
# Group.1  x
# 1         centre 22
# 2         Droite 27
# 3  ExtremeDroite 26
# 4  ExtremeGauche  7
# 5             FN 52
# 6         Gauche 31
# 7         Mairie 45
# 8          Media 49
# 9            PCF 13
# 10           PRG  7
# 11            PS 57
# 12           UMP 58

##SET THE PARTY TO PLOT THE GRAPH
  p <- "FN"
  data <- subset(mostUsedHashOfParties[,c("Party","Hashtag", "V","vStar","count_h", "sqRootchi2POSNEG")], mostUsedHashOfParties$Party==p)
  data[order(-data$sqRootchi2POSNEG),]
  plot_ly(data, x = ~V, y = ~vStar,text= ~Hashtag , type="scatter" , mode = "markers", color = ~sqRootchi2POSNEG, size = ~sqRootchi2POSNEG)
 
remove(parties)
remove(data)
remove(p)

####still not a poisson distribution
int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}
int.hist(subset(ASM_StaticPARTY$V, ASM_StaticPARTY$V<50))

#remove(hash_p_._h)
#remove(hash_p_._.)
remove(ASM_StaticPARTY)
remove(mostUsedHashOfParties)