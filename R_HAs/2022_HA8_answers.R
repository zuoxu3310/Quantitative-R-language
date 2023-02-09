# HA 8

#1. Read data from the 6th wave of WVS.
library("haven")
wvs6 <- read_sav("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav")
dim(wvs6) 
# 89565 respondents,   442 variables

#2. Select Russian and Sweden sample in a new data set
wvs6_rusw<-wvs6[as_factor(wvs6$V2)=="Russia" | as_factor(wvs6$V2)=="Sweden", ]
dim(wvs6_rusw)
#3706 respondents,  442 variables (still)

#3. Select variables: V102 (trust in family), V103 (trust in neighbors), V104 (trust in known
#people), V105 (trust in unknown people), V106 (trust in people of another religion),
#V107(trust in people of another nationality), V23 (life satisfaction), V10 (happiness).
wvs6_rusw <- wvs6_rusw[c("V2", "V102", "V103", "V104", "V105", "V106", "V107", "V23", "V10")]
dim(wvs6_rusw)
#3706 respondents,  9 variables

#4. Recode variables with reversed scales to get a correct order of numeric codes. Don`t forget to check recoding.
attributes(wvs6_rusw$V102)$labels #ordinal 4 points, reversed
attributes(wvs6_rusw$V103)$labels #ordinal 4 points, reversed
attributes(wvs6_rusw$V104)$labels #ordinal 4 points, reversed
attributes(wvs6_rusw$V105)$labels #ordinal 4 points, reversed
attributes(wvs6_rusw$V106)$labels #ordinal 4 points, reversed
attributes(wvs6_rusw$V107)$labels #ordinal 4 points, reversed
attributes(wvs6_rusw$V23)$labels  #ordinal 10 points, straight
attributes(wvs6_rusw$V10)$labels #ordinal 4 points, reversed
#So, all variables except V23 (life satisfaction) should be reversed.
#Now I will just reverse scales and I will take a look later if I should combine some categories.

##V102 (trust in family)
wvs6_rusw$trfam<-5-as.numeric(wvs6_rusw$V102)
wvs6_rusw$trfam<-labelled(wvs6_rusw$trfam,c("Do not trust at all"=1, "Do not trust very much"=2, 
                                            "Trust somewhat"=3, "Trust completely"=4))
table(wvs6_rusw$trfam, wvs6_rusw$V102) #checking

##V103 (trust in neighbors)
wvs6_rusw$trneighbors<-5-as.numeric(wvs6_rusw$V103)
wvs6_rusw$trneighbors<-labelled(wvs6_rusw$trneighbors,c("Do not trust at all"=1, "Do not trust very much"=2, 
                                                        "Trust somewhat"=3, "Trust completely"=4 ))
table(wvs6_rusw$trneighbors, wvs6_rusw$V103) #checking

##V104 (trust in known people)
wvs6_rusw$trknown<-5-as.numeric(wvs6_rusw$V104)
wvs6_rusw$trknown<-labelled(wvs6_rusw$trknown,c("Do not trust at all"=1, "Do not trust very much"=2, 
                                                "Trust somewhat"=3, "Trust completely"=4 ))
table(wvs6_rusw$trknown, wvs6_rusw$V104) #checking

##V105 (trust in unknown people)
wvs6_rusw$trunknown<-5-as.numeric(wvs6_rusw$V105)
wvs6_rusw$trunknown<-labelled(wvs6_rusw$trunknown,c("Do not trust at all"=1, "Do not trust very much"=2, 
                                                    "Trust somewhat"=3, "Trust completely"=4 ))
table(wvs6_rusw$trunknown, wvs6_rusw$V105) #checking

##V106 (trust in people of another religion)
wvs6_rusw$trrel<-5-as.numeric(wvs6_rusw$V106)
wvs6_rusw$trrel<-labelled(wvs6_rusw$trrel,c("Do not trust at all"=1, "Do not trust very much"=2, 
                                            "Trust somewhat"=3, "Trust completely"=4 ))
table(wvs6_rusw$trrel, wvs6_rusw$V106) #checking

##V107 (trust in people of another nationality)
wvs6_rusw$trnat<-5-as.numeric(wvs6_rusw$V107)
wvs6_rusw$trnat<-labelled(wvs6_rusw$trnat,c("Do not trust at all"=1, "Do not trust very much"=2, 
                                            "Trust somewhat"=3, "Trust completely"=4 ))
table(wvs6_rusw$trnat, wvs6_rusw$V107) #checking

##V10 (happiness)
wvs6_rusw$happy<-5-as.numeric(wvs6_rusw$V10)
wvs6_rusw$happy<-labelled(wvs6_rusw$happy,c("Not at all happy"=1, "Not very happy"=2, 
                                            "Rather happy"=3, "Very happy"=4 ))
table(wvs6_rusw$happy, wvs6_rusw$V10) #checking

#5. Using raw counts check the distributions of all trust variables in Russia and Sweden including missing values.
addmargins(table(as_factor(wvs6_rusw$trfam), useNA="ifany"))
addmargins(table(as_factor(wvs6_rusw$trneighbors), useNA="ifany"))
addmargins(table(as_factor(wvs6_rusw$trknown), useNA="ifany"))
addmargins(table(as_factor(wvs6_rusw$trunknown), useNA="ifany"))
addmargins(table(as_factor(wvs6_rusw$trrel), useNA="ifany"))
addmargins(table(as_factor(wvs6_rusw$trnat), useNA="ifany"))

#As we can see, there is missing data in several trust variables. There are 43 NAs for trust in family, 
#81 for trust in neighbors, 32 for trust in known people, 137 for trust in unknown ones, 
#425 for trust in people of different religion, and 411 for trust in people of other nationalities.

#As for size of the groups, people who Do not trust at all in family and who Do not trust very much in family are rather small, 
#16 and 51 (N=3706) respectively. The same situation is among groups of trust in known people, 
#56 respondents Do not trust at all. It would be better to combine the groups to get fuller ones. 
#There are 104 respondents who trust completely in unknown people, which is almost 3%, it is a small number but seems more or less ok.

#6. Recode some trust variables to solve the problem of small number of observations in cross-tables. Check the recoding.
library(car)
wvs6_rusw$trfam_rec<-car::recode(as.numeric(wvs6_rusw$trfam), "1:2=1; 3=2; 4=3") #combine 1 and 2 groups
wvs6_rusw$trfam_rec<-labelled(wvs6_rusw$trfam_rec, c("Do not trust"=1, "Trust somewhat"=2, "Trust completely"=3))
table(as_factor(wvs6_rusw$trfam), as_factor(wvs6_rusw$trfam_rec), useNA="ifany") #check

wvs6_rusw$trknown_rec<-car::recode(as.numeric(wvs6_rusw$trknown), "1:2=1; 3=2; 4=3") #combine 1 and 2 groups
wvs6_rusw$trknown_rec<-labelled(wvs6_rusw$trknown_rec, c("Do not trust"=1, "Trust somewhat"=2, "Trust completely"=3))
table(as_factor(wvs6_rusw$trknown), as_factor(wvs6_rusw$trknown_rec), useNA="ifany") #check

#7. Compare levels of all trust indicators in Russia and Sweden. Draw conclusions and illustrate your conclusions 
#using measures of basic statistics. Choose the correct ones.
addmargins(prop.table(xtabs(~as_factor(trfam_rec)+as_factor(V2),
                            data=wvs6_rusw, drop.unused.levels = T),2),1)*100
#The general trend is almost the same for trust in family in both countries. 
#There are very small shares of those who do not trust in their family, 2% in Russia and 1% in Sweden. 
#The groups of respondents who trust somewhat and trust completely are almost identical: 
#there are 9% each who trust somewhat and 89% each who trust completely in both countries.

addmargins(prop.table(xtabs(~as_factor(trneighbors)+as_factor(V2),
                            data=wvs6_rusw, drop.unused.levels = T),2),1)*100
#The groups who do not trust at all in neighbors and who trust somewhat to them are similar in Russia and Sweden. 
#Thus, there are 6% each who do not trust at all and 19% and 31% of those who trust completely in neighbors in Russia and Sweden, 
#respectively. Those who do not trust very much in neighbors are 7% more in Russia than in Sweden, 20% and 13%  correspondingly. 
#Also, those who trust completely in neighbors are 12% less in Russia than in Sweden, 19% and 31% respectively. 
#Overall, trust in neighbors level is slightly higher in Sweden.

addmargins(prop.table(xtabs(~as_factor(trknown_rec)+as_factor(V2),
                            data=wvs6_rusw, drop.unused.levels = T),2),1)*100
# In Russian sample 17% of respondents do not trust in known people while in Sweden one this value is just 3%.
# More than a half of respondents in both samples trust somewhat in close circle, 62% in Russia and 54% in Sweden. 
# In Russia there are twice less respondents who trust completely in known people, 20% in Russia and 43% in Sweden.
#Overall, respondents from both samples tend to trust in knowns but this trust level in Sweden is slightly higher.

addmargins(prop.table(xtabs(~as_factor(trunknown)+as_factor(V2),
                            data=wvs6_rusw, drop.unused.levels = T),2),1)*100
#Here we can see the opposite trends. In Russia there are 3 times more people who do not trust at all in unknown people than in Sweden,
#34% and 11%, respectively. Also, there are more Russians (45%) than Sweds (30%) who do not trust very much in unknown people. 
#By contrast, the share of Sweds who trust somewhat in unknown people is higher - there are 55% Sweds and 20% Russians in this group.
#Though, the groups of those who trust completely in unknown people are rather small in both countries but it's slightly larger
#in Sweden (5%) than in Russia (2%).

addmargins(prop.table(xtabs(~as_factor(trrel)+as_factor(V2),
                            data=wvs6_rusw, drop.unused.levels = T),2),1)*100
#Again, we can see that Russians are comparably less trustful in people of other religions. 
#The majority of Sweds trust somewhat in people of other religions (64%). 
#In Russia there are similar shares of those who trust somewhat (37%) and who do not trust very much (36%) 
#in people of another religion. Also, there are similar shares for 'trust very much' group in Russia (5%) and do not trust at all 
#in Sweden (4%). 

addmargins(prop.table(xtabs(~as_factor(trnat)+as_factor(V2),
                            data=wvs6_rusw, drop.unused.levels = T),2),1)*100
#In general, there are opposite trends of trust in people of another nationality in both countries. 
#The majority of Swiss (64%) trust somewhat in people of another nationality. 
#In Russia there are 38% of those who trust somewhat and 35% of those who do not trust very much in people on another nationality.
#As for those who do not trust at all in other nationalities, there are 22% in Russia and 3% in Sweden. 
#By contrast, there are 21% of Sweds who trust completely in people of another nationality and only 5% of such people in Russia.

# DO NOT FORGET TO MIND OVERALL TRENDS AND SPESIFIC DIFFERENCES BETWEEN GROUPS WHILE INTERPRETATING

# 8. Using raw counts check the distributions of life satisfaction in Russia and Sweden including missing values.
addmargins(table(as_factor(wvs6_rusw$V23), useNA="ifany"))
# there are 37 missing values

#9. Compare levels of life satisfaction in Russia and Sweden. Draw conclusions and illustrate
#these conclusions using measures of basic statistics. Choose the correct ones. If you need
#to recode this variable, don`t forget to do it. Don`t forget to check recoding.

#To compare the levels of life satisfaction, I will, first, record V23 in 5-points scale to make less categories for clearer 
#interpretation. Also, groups of 'Completely dissatisfied ' (89, N= 3706) and those who estimated life satisfaction 
#as '2' (86, N=3706) are very small and I prefer to combine them.
wvs6_rusw$lifesat <- car::recode(as.numeric(wvs6_rusw$V23), "1:2=1; 3:4=2; 5:6=3; 7:8=4; 9:10=5")
wvs6_rusw$lifesat<-haven::labelled(wvs6_rusw$lifesat, c("Dissatisfied"=1, "Rather dissatisfied"=2, "Not satisfied, not dissatisfied"=3,
                                                 "Rather satisfied"=4, "Satisfied"=5))
table(as_factor(wvs6_rusw$V23), wvs6_rusw$lifesat, useNA="ifany") #check is ok

#Now we can compare the levels of life satisfaction in two countries.
addmargins(prop.table(xtabs(~as_factor(lifesat)+as_factor(V2),
                            data=wvs6_rusw, drop.unused.levels = T),2),1)*100
#The overall level of life satisfaction is higher in Sweden than in Russia. More than a half (52%) of Sweds are rather satisfied 
#with their life while there are 33% of Russians in this group. Also, 34% of Russians are not satisfied, 
#not dissatisfied with their life (3 out of 5 points scale) while there are 13% of Sweds in that group. 
#Almost the third (29%) of respondents in Sweden are satisfied with their life, there are only 13% of satisfied people in Russia. 
#There is a difference in approximately 3 times between Russians and Sweds who are rather dissatisfied with their life, 
#14% and 4% respectively. Also, there are more Russians (7%) who are dissatisfied with their life than Sweds (less than 1%).

#10. Using raw counts check the distributions of happiness in Russia and Sweden including missing values.
addmargins(table(as_factor(wvs6_rusw$happy), useNA="ifany"))
#As we can see, the group of those who are not at all happy is small (48, N=3706). 
#So, I suggest to combine it with 'Not very happy'. Hence, we will get a 3-points scale as Not happy - Rather happy - Very happy. 
#Also, there are 108 NAs.
wvs6_rusw$happy_rec <- car::recode(as.numeric(wvs6_rusw$happy), "1:2=1; 3=2;4=3")
wvs6_rusw$happy_rec<-labelled(wvs6_rusw$happy_rec, c("Not happy"=1, "Rather happy"=2, "Very happy"=3))
table(as_factor(wvs6_rusw$happy), as_factor(wvs6_rusw$happy_rec), useNA="ifany") #check is ok

#11. Compare levels of happiness in Russia and Sweden. Draw conclusions and illustrate these conclusions using numbers. 
#If you need to recode this variable, don`t forget to do it. Don`t forget to check recoding.
addmargins(prop.table(xtabs(~as_factor(happy_rec)+as_factor(V2),
                            data=wvs6_rusw, drop.unused.levels = T),2),1)*100
#The general level of happiness is higher in Sweden than in Russia. The majorities of people are rather happy in both countries, 
# 60% in Russia and 53% in Sweden. Though, there are more than 2 times more very happy people in Sweden (42%) than in Russia (16%). 
#Hence, there are less unhappy people in Sweden (5%) than in Russia (24%).

# 12. Create accurate visualization for distributions of V102 (trust in family), V103 (trust in neighbors),
#V104 (trust in known people), V105 (trust in unknown people), V106 (trust in people of another religion), 
#V107(trust in people of another nationality), V23 (life satisfaction), V10 (happiness). 
# Do not forget to include meaningful names and labels on your graphs.

#V102 (trust in family)
addmargins(table(wvs6_rusw$trfam_rec))
g1 <- ggplot(wvs6_rusw, aes(x=trfam_rec, fill=as_factor(V2)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+ # to show shares rather than absolute numbers
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#eba2a2","Sweden"="#a3a2eb")) +
  theme_minimal() +
  labs( caption = 'N=3663',
       x = 'trust in family'); g1

#V103 (trust in neighbors)
addmargins(table(wvs6_rusw$trneighbors))
g2 <- ggplot(wvs6_rusw, aes(x=trneighbors, fill=as_factor(V2)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+ 
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#eba2a2","Sweden"="#a3a2eb")) +
  theme_minimal() +
  labs( caption = 'N=3625 ',
        x = 'trust in neighbors'); g2

#V104 (trust in known people)
addmargins(table(wvs6_rusw$trknown_rec))
g3 <- ggplot(wvs6_rusw, aes(x=trknown_rec, fill=as_factor(V2)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+ 
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#eba2a2","Sweden"="#a3a2eb")) +
  theme_minimal() +
  labs( caption = 'N=3674',
        x = 'trust in known people'); g3

#V104 (trust in known people)
addmargins(table(wvs6_rusw$trunknown))
g4 <- ggplot(wvs6_rusw, aes(x=trunknown, fill=as_factor(V2)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+ 
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#eba2a2","Sweden"="#a3a2eb")) +
  theme_minimal() +
  labs( caption = 'N=3674',
        x = 'trust in unknown people'); g4

#V106 (trust in people of another religion)
addmargins(table(wvs6_rusw$trrel))
g5 <- ggplot(wvs6_rusw, aes(x=trrel, fill=as_factor(V2)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+ 
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#eba2a2","Sweden"="#a3a2eb")) +
  theme_minimal() +
  labs( caption = 'N=3281 ',
        x = 'trust in people of another religion'); g5

#V107(trust in people of another nationality)
addmargins(table(wvs6_rusw$trnat))
g6 <- ggplot(wvs6_rusw, aes(x=trnat, fill=as_factor(V2)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+ 
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#eba2a2","Sweden"="#a3a2eb")) +
  theme_minimal() +
  labs( caption = 'N=3295 ',
        x = 'trust in people of another nationality'); g6

#V23 (life satisfaction)
addmargins(table(wvs6_rusw$lifesat))
g_lifesat <- ggplot(wvs6_rusw, aes(x=lifesat, fill=as_factor(V2)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+ 
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#eba2a2","Sweden"="#a3a2eb")) +
  theme_minimal() +
  labs(caption = 'N=3669 ',
        x = 'life satisfaction, 1=do not trust'); g_lifesat

#V10 (happiness)
addmargins(table(wvs6_rusw$happy_rec))
g_happy <- ggplot(wvs6_rusw, aes(x=happy_rec, fill=as_factor(V2)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+ 
  scale_fill_manual(name="Country", 
                    values = c("Russia"="#eba2a2","Sweden"="#a3a2eb")) +
  theme_minimal() +
  labs(caption = 'N=3598  ',
       x = 'happiness') +
  scale_x_continuous(breaks = c(1,2,3), labels=c("not happy", "neither", "happy")); g_happy

#13. Create a picture which includes graphs of trust variables.
library(ggpubr)
ggarrange(g1, g2, g3, g4, g5, g6, nrow = 3, ncol=3)
#to save it use ggsave()


