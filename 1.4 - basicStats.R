library("lubridate", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library(data.table)
library(ggplot2)

############################################## Not filtered data, from october on

###############################Users

a0 <- aggregate(import_tracked_userID$UserID, by=list(import_tracked_userID$Party), FUN=length)
names(a0)  <- c("Party","Number_tracked_Users")

###se how many users by party
a <- aggregate(notFiltered_hash_u_._.$Party , by=list(notFiltered_hash_u_._.$Party) ,FUN=length)
names(a)  <- c("Party","Number_Users_in_data")

##how many different hashtags
b <- aggregate(notFiltered_hash_u_._h$Party , by=list(notFiltered_hash_u_._h$Party, notFiltered_hash_u_._h$Hashtag) ,FUN=length)
names(b)<- c("Party","Hashtag","count")
b <- aggregate(b$Party, by = list(b$Party),FUN=length)
names(b)  <- c("Party","Number_Different_Hashtags")

###se how many records by party
c <- aggregate(notFiltered_hash_u_._h$count , by=list(notFiltered_hash_u_._h$Party) ,FUN=sum)
names(c)  <- c("Party","Number_Hashtags_Tweeted")

stats_partytable <- merge(x=a0, y=a, by = c("Party"), all.x = TRUE)
stats_partytable <- merge(x=stats_partytable, y=b, by = c("Party"), all.x = TRUE)
stats_partytable <- merge(x=stats_partytable, y=c, by = c("Party"), all.x = TRUE)
remove(a0)
remove(a)
remove(b)
remove(c)

######## PRINT

###How many users in data
length(notFiltered_hash_u_._.$User)
##Stats on parties
stats_partytable


###average posting per user
mean(notFiltered_hash_u_._.$count)
###Main users and how many hashtags
head(notFiltered_hash_u_._.[order(-notFiltered_hash_u_._.$count),])
####Less active users
count(subset(notFiltered_hash_u_._., notFiltered_hash_u_._.$count==1))
count(subset(notFiltered_hash_u_._., notFiltered_hash_u_._.$count<=10))

#####################################Hashtags



#######PRINT
###average posting per hashtag
mean(notFiltered_hash_._._h$count)
###Main hashtags and how many users
head(notFiltered_hash_._._h[order(-notFiltered_hash_._._h$count),], n=20L)
####Less active hashtags
count(subset(notFiltered_hash_._._h, notFiltered_hash_._._h$count==1))
count(subset(notFiltered_hash_._._h, notFiltered_hash_._._h$count<=10))






#####################################################
#plot distributions of Aggregations- BEFORE FILTERS

hist(notFiltered_hash_u_._.$count, breaks = 1000 , main = "Distribution of the Users activity v(u,-,-)",  xlab = "v(u,-,-)")
#hist(subset(notFiltered_hash_u_._., notFiltered_hash_u_._.$count <200)$count, breaks = 200)

hist(notFiltered_hash_._._h$count, breaks = 1000, main = "Distribution of the Hashtag usage v(u,-,-)",  xlab = "v(-,-,h)")
#hist(subset(notFiltered_hash_._._h, notFiltered_hash_._._h$count <50)$count, breaks = 50)


#hist(notFiltered_hash_u_._h$count, breaks = 1000)
#hist(subset(notFiltered_hash_u_._h, notFiltered_hash_u_._h$count <20)$count, breaks = 20)


#########distributions on log scale

hist(log(notFiltered_hash_u_._.$count), breaks = 100,   main = "Logarithm of the distribution of the Users activity - v(u,-,-)",  xlab = "log(v(u,-,-))")

hist(log(notFiltered_hash_._._h$count), breaks = 100,   main = "Logarithm of the distribution of the Hashtags usage - v(-,-,h)",  xlab = "log(v(-,-,h))")
















#######################################################AFTER FILTERS AND ASM
##############################################################################
##plot distributions - AFTER FILTERS
hist(hash_u_._.$count, breaks = 1000)
hist(subset(hash_u_._., hash_u_._.$count <1000)$count, breaks = 1000)

hist(hash_._._h$count, breaks = 1000)
hist(subset(hash_._._h, hash_._._h$count <1000)$count, breaks = 1000)


hist(hash_u_._h$count, breaks = 1000)
hist(subset(hash_u_._h, hash_u_._h$count <20)$count, breaks = 20)
a <- aggregate(subset(hash_u_._h, hash_u_._h$count <=20)$Hashtag, by =list(subset(hash_u_._h, hash_u_._h$count <=20)$count), FUN=length)
names(a)<- c("Nb_TweetedHashtags_Per_User","Frequency")
a
remove(a)



####################### On the ASM - Sparsity
a <- aggregate(subset(ASM_Static_hashtags, ASM_Static_hashtags$V <=20)$Hashtag, by =list(subset(ASM_Static_hashtags, ASM_Static_hashtags$V <=20)$V), FUN=length)
names(a)<- c("Nb_TweetedHashtags_Per_User","Frequency")
a
remove(a)