#  HA 9

# Calculate confidence intervals for mean = 5500. Ignore population size. Keep in mind that now we have sample sd and have to use correction n-1
# 1.	sample sd=3000, alpha=0.05, sample=100
# 2.	sample sd=3000, alpha=0.05, sample=500
# 3.	sample sd=3000, alpha=0.05, sample=1000
# 4.	sample sd=3000, alpha=0.05, sample=10000
# 5.	sample sd=3000, alpha=0.05, sample=20000
# 6.	sample sd=3000, alpha=0.05, sample=50000
# 7.	Create a data.frame which includes all results. You may create objects and use vectors 
# 8.	Repeat 1-7 for sd=100
# 9.	Repeat 1-7 for alpha=0.1, sd = 100
# 10.	Draw conclusions of how the width of confidence intervals depends of sample size, confidence level and diversity of the population. 

#manually
ci.l<-5500-qnorm(0.95+0.025)*3000/sqrt(100-1); ci.u<-5500+qnorm(0.95+0.025)*3000/sqrt(100-1); ci.l; ci.u

#or to make it for all tasks with vectors
mean<-5500; mean
sdsample<-3000; sdsample
z<-qnorm(0.95+0.025); z
ssize<-c(100, 500, 1000, 10000, 20000, 50000); ssize
ci.l<-mean-z*sdsample/sqrt(ssize-1); ci.u<-mean+z*sdsample/sqrt(ssize-1); ci.l; ci.u
ci_95_3000<-data.frame(Lower = ci.l, Mean = 5500, Upper = ci.u)
rownames(ci_95_3000)<-c("ss100", "ss500", "ss1000", "ss10000", "ss20000", "ss50000")
ci_95_3000

# 8.	Repeat 1-7 for sd=100

sdsample<-100; sdsample

ci.l<-mean-z*sdsample/sqrt(ssize-1); ci.u<-mean+z*sdsample/sqrt(ssize-1); ci.l; ci.u
ci_95_100<-data.frame(Lower = ci.l, Mean = 5500, Upper = ci.u)
rownames(ci_95_100)<-c("ss100", "ss500", "ss1000", "ss10000", "ss20000", "ss50000")
ci_95_100

# 9.	Repeat 1-7 for alpha=0.1, sd = 100
z<-qnorm(0.90+0.05); z
ci.l<-mean-z*sdsample/sqrt(ssize-1); ci.u<-mean+z*sdsample/sqrt(ssize-1); ci.l; ci.u
ci_90_100<-data.frame(Lower = ci.l, Mean = 5500, Upper = ci.u)
rownames(ci_90_100)<-c("ss100", "ss500", "ss1000", "ss10000", "ss20000", "ss50000")
ci_90_100
all_ci<-cbind(ci_95_3000, ci_95_100, ci_90_100); all_ci

#Confidence intervals become less if:
# our sample size is larger, sd is smaller, 
# and we allow the higher probability of a mistake


#  Part 2 
#1.	Select Russia and Sweden using 6 wave of WVS
library("haven")
wvs6 <- read_sav("/Users/Natasha/Downloads/WV6_Data_Spss_v20180912.sav")
wvs6_rusw<-wvs6[as_factor(wvs6$V2)=="Russia" | as_factor(wvs6$V2)=="Sweden", ]
dim(wvs6_rusw)

#2.	Create an index of generalized trust (strangers, another nationality, another religion). 
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
wvs6_rusw$happy_rec <- car::recode(as.numeric(wvs6_rusw$happy), "1:2=1; 3=2;4=3")
wvs6_rusw$happy_rec<-labelled(wvs6_rusw$happy_rec, c("Not happy"=1, "Rather happy"=2, "Very happy"=3))
table(as_factor(wvs6_rusw$happy), as_factor(wvs6_rusw$happy_rec), useNA="ifany") #check is ok

#index of generalized trust
wvs6_rusw$trust_gen_cnt<-round(rowMeans(wvs6_rusw[c("trunknown","trnat", "trrel")], na.rm=T), 2)
#View(wvs6_rusw[,c("trunknown","trnat", "trrel")])
summary(wvs6_rusw$trust_gen_cnt) #scale is from 1 to 4
table(wvs6_rusw$trust_gen_cnt, useNA = "ifany")
plot(wvs6_rusw$trust_gen_cnt, wvs6_rusw$trunknown)
plot(wvs6_rusw$trust_gen_cnt, wvs6_rusw$trnat)
plot(wvs6_rusw$trust_gen_cnt, wvs6_rusw$trrel)


#3.	Create an index of particularized trust (known people, neighbors).
wvs6_rusw$trust_part_cnt<-round(rowMeans(wvs6_rusw[c("trneighbors","trknown")], na.rm=T), 2)
#View(wvs6_rusw[,c("trneighbors","trknown")])
summary(wvs6_rusw$trust_part_cnt) #the scale is from 1 to 4
table(wvs6_rusw$trust_part_cnt, useNA = "ifany")
plot(wvs6_rusw$trust_part_cnt, wvs6_rusw$trneighbors)
plot(wvs6_rusw$trust_part_cnt, wvs6_rusw$trknown)

#4
# Particularized trust in Russia and Sweden
# to calculate CI we need
# sample size (valid)
# SD
# Mean

ptval<-table(complete.cases(wvs6_rusw$trust_part_cnt),droplevels(as_factor(wvs6_rusw$V2)))

ptval[2,1] # Russia 
ptval[2,2] # Sweden 

pt_mean<-tapply(wvs6_rusw$trust_part_cnt,droplevels(as_factor(wvs6_rusw$V2)), mean, na.rm=T)
pt_mean[1]
pt_mean[2]

pt_SD<-tapply(wvs6_rusw$trust_part_cnt,droplevels(as_factor(wvs6_rusw$V2)), sd, na.rm=T)

pt_SD[1]
pt_SD[2]

z<-qnorm(0.95+0.025)

# Particularized trust in Russia 
ci.l<-pt_mean[1]-z*pt_SD[1]/sqrt(ptval[2,1]-1); ci.u<-pt_mean[1]+z*pt_SD[1]/sqrt(ptval[2,1]-1)
pt_rus<-c(round(ci.l,3), round(ci.u,3)); pt_rus

# In Sweden
ci.l<-pt_mean[2]-z*pt_SD[2]/sqrt(ptval[2,2]-1); ci.u<-pt_mean[2]+z*pt_SD[2]/sqrt(ptval[2,2]-1)
pt_sw<-c(round(ci.l,3), round(ci.u,3)); pt_sw

pt_all<-list(pt_rus, pt_sw); pt_all

# In Russia the level of particularized trust is from 2.92 to 2.96 with 95% probability
# In Sweden with 95% probability it is from 3.21 to 3.27
# In Sweden the level of particularized trust is higher 

## CIs for generalized trust 

gtval<-table(complete.cases(wvs6_rusw$trust_gen_cnt),droplevels(as_factor(wvs6_rusw$V2)))

gtval[2,1] # Russia 
gtval[2,2] # Sweden 

gt_mean<-tapply(wvs6_rusw$trust_gen_cnt,droplevels(as_factor(wvs6_rusw$V2)), mean, na.rm=T)
gt_mean[1]
gt_mean[2]

gt_SD<-tapply(wvs6_rusw$trust_gen_cnt,droplevels(as_factor(wvs6_rusw$V2)), sd, na.rm=T)

gt_SD[1]
gt_SD[2]
z<-qnorm(0.95+0.025)

# Generalized trust in Russia
ci.l<-gt_mean[1]-z*gt_SD[1]/sqrt(gtval[2,1]-1); ci.u<-gt_mean[1]+z*gt_SD[1]/sqrt(gtval[2,1]-1)
gt_rus<-c(round(ci.l,3), round(ci.u,3)); gt_rus
# Generalized trust in Sweden
ci.l<-gt_mean[2]-z*gt_SD[2]/sqrt(gtval[2,2]-1); ci.u<-gt_mean[2]+z*gt_SD[2]/sqrt(gtval[2,2]-1)
gt_sw<-c(round(ci.l,3), round(ci.u,3)); gt_sw

gt_all<-list(gt_rus, gt_sw); gt_all

# In Russia the level of generalized trust with 95% probability is from 2.08 to 2.14
# In Sweden with 95% probability it is from 2.79 to 2.86
# In Sweden the level of generalized trust is higher 


# 5.	Use recoded 3 point scales for happiness in Russia and Sweden.  
# Calculate the share of people who are not happy.
# Calculate confidence intervals using alpha = 0.05. 
# Describe interval estimates in Russia and Sweden 
# and draw conclusions about the level of unhappiness 
# in Russia and Sweden using interval estimates. 
table(wvs6_rusw$happy,droplevels(as_factor(wvs6_rusw$V2)))
table(wvs6_rusw$happy_rec,droplevels(as_factor(wvs6_rusw$V2)))

# To calculate we need:
# Share
# Valid sample size 
# https://towardsdatascience.com/five-confidence-intervals-for-proportions-that-you-should-know-about-7ff5484c024f 

hap_val<-addmargins(table(complete.cases(wvs6_rusw$happy_rec),
                          droplevels(as_factor(wvs6_rusw$V2))),1) #raw counts


hap_val[2,1] # Russia
hap_val[2,2] # Sweden

hap_share<-round(prop.table(table(wvs6_rusw$happy_rec,
                 droplevels(as_factor(wvs6_rusw$V2))),2),3) #shares

hap_share[1,1] # unhappy in Russia
hap_share[1,2] # unhappy in Sweden

ci.l<-hap_share[1,1]-z*sqrt(hap_share[1,1]*(1-hap_share[1,1])/(hap_val[2,1]-1))
ci.u<-hap_share[1,1]+z*sqrt(hap_share[1,1]*(1-hap_share[1,1])/(hap_val[2,1]-1))

hap_ci_rus<-c(ci.l, ci.u)

ci.l<-hap_share[1,2]-z*sqrt(hap_share[1,2]*(1-hap_share[1,2])/(hap_val[2,2]-1))
ci.u<-hap_share[1,2]+z*sqrt(hap_share[1,2]*(1-hap_share[1,2])/(hap_val[2,2]-1))

hap_ci_sw<-c(ci.l, ci.u)

hap_ci_all<-list(round(hap_ci_rus,3), round(hap_ci_sw,3))
hap_ci_all

# With 95% probability the share of unhappy people in Russia is from 22.3% to 25.7%
# The share of unhappy people in Sweden is from 3.5% to 5.9% with 95% probability
# The level of unhappiness is higher in Russia than in Sweden 

library(DescTools)
hap_raw<-table(wvs6_rusw$happy_rec,droplevels(as_factor(wvs6_rusw$V2)))
hap_raw[,1] # Rus
hap_raw[,2] # Sw

MultinomCI(hap_raw[,1], conf.level = 0.95, sides = c("two.sided"),
                 method = c("waldcc"))# Russia 
MultinomCI(hap_raw[,2], conf.level = 0.95, sides = c("two.sided"),
                 method = c("waldcc"))# Sweden 
#to compare with the previous way of calculating unhappy group
hap_ci_all

# 3.	Calculate the mean level of 
# particularized trust for three groups of happiness 
# in Russia and Sweden. 
# Calculate confidence intervals for each mean using alpha 0.05. 
# Describes interval estimates and draw conclusions on which groups 
# of happiness have higher level of 
# particularized trust using interval estimates. 

means<-tapply(wvs6_rusw$trust_part_cnt, 
              list(droplevels(as_factor(wvs6_rusw$V2)),
                   as_factor(wvs6_rusw$happy_rec)),
              mean, na.rm=T)


sdgr<-tapply(wvs6_rusw$trust_part_cnt, 
              list(droplevels(as_factor(wvs6_rusw$V2)),
                   as_factor(wvs6_rusw$happy_rec)),
              sd, na.rm=T)


valid<-tapply(wvs6_rusw$trust_part_cnt, 
             list(droplevels(as_factor(wvs6_rusw$V2)),
                  as_factor(wvs6_rusw$happy_rec)),
             function (x) sum(complete.cases(x)))

ci.l <- means-1.96*sdgr/sqrt(valid-1)
ci.u <- means+1.96*sdgr/sqrt(valid-1)

all_gr<-data.frame(unhl=ci.l[,1], unhu=ci.u[,1],
                   rhl=ci.l[,2], rhu=ci.u[,2],
                   hl=ci.l[,3], hu=ci.u[,3])


tr_hap<-summarySE(wvs6_rusw, measurevar=c("trust_part_cnt"),
          groupvars = c("V2", "happy_rec"), na.rm = TRUE)

tr_hap$tr_hapl<-tr_hap[,"trust_part_cnt"]-tr_hap[,"ci"]
tr_hap$tr_hapu<-tr_hap[,"trust_part_cnt"]+tr_hap[,"ci"]

tr_hap
all_gr

# With 95% probability the mean of particularized trust in the group of 
# unhappy people in Russia is from 2.8 to 2.92, 
# rather happy people  is from 2.94 to 3.00
# happy people is from - 2.85 - 2.9
# So unhappy people have less trust than happy and rather happy
# But we cannot say if happy people have higher trust than rather happy 
# In Sweden the level of trust for unhappy people is from 2.9 to 3.2
# Rather happy - 3.1 - 3.2
# Happy - 3.3 - 3.4
# Therefore, happy people have higher level of trust than 
# rather happy and unhappy
# But we cannot say that unhappy people have higher level of trust than rather happy

# Compare with 4-point scale of happiness 
tr_hap_4<-summarySE(wvs6_rusw, measurevar=c("trust_part_cnt"),
                  groupvars = c("V2", "happy"), na.rm = TRUE)

tr_hap_4$tr_hap_4l<-tr_hap_4[,"trust_part_cnt"]-tr_hap_4[,"ci"]
tr_hap_4$tr_hap_4u<-tr_hap_4[,"trust_part_cnt"]+tr_hap_4[,"ci"]
tr_hap_4
