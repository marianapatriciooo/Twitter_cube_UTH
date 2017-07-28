# Charge the plotly library
library(plotly)



subset(hash_U_._H, Hashtag == "France")
subset(hash_U_._H[order(-hash_U_._H$count),], Hashtag == "France")
hash_u_._.[order(-hash_u_._.$count),]

###view some examples
##ANALISYS EXAMPLE
View(subset(ASM, User == "Partiradical28" & Month =="8" & Day == "5"))
#to subset and order
subset(hash_U_TDay_.[order(-hash_U_TDay_.$count),], Month =="8" & Day == "31")


##plot distributions
plot(density(hash_u_._h$count))
plot(density(subset(hash_u_._h, count <50)$count))




ASM_Static <- ASM_Static_users

####see unxepected values
unexpected <- subset(ASM_Static, ASM_Static$chi2>5000 & ASM_Static$Diff_obs_est>2)
View(unexpected)
remove(unexpected)

p1 <- subset(ASM_Static, UserID == "108359730" & V>0)
names(p1)[names(p1)=="V"] <- "observed"
names(p1)[names(p1)=="vStar"] <- "expected"
names(p1)[names(p1)=="chi2POSNEG_normBYhashtags"] <- "divergenceFromExpected"
head(p1)

plot_ly(p1, x = ~observed, y = ~expected,text= ~Hashtag, type="scatter" , mode = "markers", color = ~divergenceFromExpected, size = ~divergenceFromExpected)

remove(p1)


ASM_Static <- ASM_Static_hashtags

####see unxepected values
unexpected <- subset(ASM_Static, ASM_Static$chi2>5000 & ASM_Static$Diff_obs_est>2)
View(unexpected)
remove(unexpected)


p2 <- subset(ASM_Static, Hashtag == "PrendsGarde" & V>0)
names(p2)[names(p2)=="V"] <- "observed"
names(p2)[names(p2)=="vStar"] <- "expected"
names(p2)[names(p2)=="chi2POSNEG_normBYhashtags"] <- "divergenceFromExpected"
head(p2)

plot_ly(p2, x = ~observed, y = ~expected,text= ~UserName, type="scatter" , mode = "markers", color = ~divergenceFromExpected, size = ~divergenceFromExpected)

remove(p2)