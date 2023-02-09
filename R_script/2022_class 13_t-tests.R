#####Anna Almakaeva, Natalia Mikhailova####
################## t-tests#################

library("haven")
wvs6<-read_sav("/Users/Natasha/Downloads/WV6_Data_Spss_v20180912.sav")
dim(wvs6)
wvs6_rusw<-wvs6[as_factor(wvs6$V2)=="Russia" | as_factor(wvs6$V2)=="Sweden", ]
dim(wvs6_rusw)


#CA to repeat.
# Compare religiousity (V147) in Russia and Sweden with cross-table
# Draw conclusions
# Calculate CIs for means of age (V242) in Sweden and in Russia each, use alpha=0.05
# Calculate CIs for shares of males and females (V240) in each country (Russia, Sweden)
# Draw conclusions


wvs6_rus<-wvs6[wvs6$V2==643, ] 
dim(wvs6_rus)
table(droplevels(as_factor(wvs6_rus$V2)))


#Independent-samples test
#V240 - gender, 1- males, 2 females
#V242 - age
class(wvs6_rus$V240)
library("Hmisc")
describe(as_factor(wvs6_rus$V240))
library(epiDisplay)
tab1(droplevels(as_factor(wvs6_rus$V240)))
tapply(wvs6_rus$V242, droplevels(as_factor(wvs6_rus$V240)), mean, na.rm=T)

# Manual calculation 

addmargins(table(wvs6_rus$V242, droplevels(as_factor(wvs6_rus$V240)))) # valid cases for gender and age
groupsn<-tail(addmargins(table(wvs6_rus$V242, droplevels(as_factor(wvs6_rus$V240)))),1); groupsn
nm<-groupsn[1]; nm # number of valid cases for males
nf<-groupsn[2]; nf # number of valid cases for females

# valid cases in male group  - 1115, female group - 1385
# std in groups 
means<-tapply(wvs6_rus$V242, droplevels(as_factor(wvs6_rus$V240)), mean, na.rm=T);  means
sd<-tapply(wvs6_rus$V242, droplevels(as_factor(wvs6_rus$V240)), sd, na.rm=T);  sd
var<-tapply(wvs6_rus$V242, droplevels(as_factor(wvs6_rus$V240)), var, na.rm=T);  var
# variance - squared sd
means[1] # male average age
means[2] # females average age 
sd[1]^2; var[1]
sd[2]^2; var[2]

# Calculate observed 
t_obs_mf<-(means[1]-means[2])/sqrt(var[1]/(nm)+var[2]/(nf))
t_obs_mf 

qt(0.975, df=2500-2) # two-samples t-test uses 2 degrees of freedom, we estimate two parameters (two means)
pt(t_obs_mf, df=2500-2) # probability to the left 
pt(t_obs_mf, df=2500-2)*2
#critical t < test t => reject H0 (means are equal) => there is difference in means

# t.test 

t.test(wvs6_rus$V242~droplevels(as_factor(wvs6_rus$V240))) #by default CL=95%,variances are assumed to be unequal, var.equal=FALSE, 

# uses Welch correction for unequal variances

t.test(wvs6_rus$V242~droplevels(as_factor(wvs6_rus$V240)),var.equal=T) # regular t-test

var.test(wvs6_rus$V242~droplevels(as_factor(wvs6_rus$V240))) # test for equality of variances, will be explained later

t.test(wvs6_rus$V242~droplevels(as_factor(wvs6_rus$V240)), alt="less",var.equal=F)  # x has a lower mean than y

t.test(wvs6_rus$V242~droplevels(as_factor(wvs6_rus$V240)), alt="greater", var.equal=F) 


means<-tapply(wvs6_rus$V242, droplevels(as_factor(wvs6_rus$V240)), mean, na.rm=T);  means
means[1] # males average age
means[2] # females average age 
sd<-tapply(wvs6_rus$V242, droplevels(as_factor(wvs6_rus$V240)), sd, na.rm=T); sd

# plotting graphs 

#means 
# install.packages("Rmisc")
library("Rmisc")

# creating a data for ggplot2
wvs6_rus$age<-wvs6_rus$V242 # for convenience
wvs6_rus$gnd<-wvs6_rus$V240 # for convenience 

gnd_age<-summarySE(data=wvs6_rus, measurevar = c("age"), 
                  groupvars = c("gnd"), na.rm = T); gnd_age
gnd_age$gnd<-as_factor(gnd_age$gnd) # to correct problem with ggplot graph 


gr_gnd_age<-ggplot(gnd_age,aes(x=gnd,y=age, color=gnd))+
  geom_point(size=4)+scale_y_continuous("mean age")+ 
  geom_errorbar(aes(ymin=age-1.96*se, ymax=age+1.96*se),width=.3)+
  theme(legend.text=element_text(size=14))+theme(legend.title=element_blank())+
  ggtitle("Mean Age Accross Gender Groups")+theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=round(age,2)),hjust=0.5, vjust=-0.5, size=4.5, color="black", check_overlap = TRUE)
gr_gnd_age


# Statistical vs. theoretical importance
# total sample of the WVS

tapply(wvs6$V242, droplevels(as_factor(wvs6$V240)), mean, na.rm=T)
t.test(wvs6$V242~droplevels(as_factor(wvs6$V240)), var.equal=F)
#! difference is less than a year, is it important? 


#Paired-samples test
#Both variables have something in common:
#Same concept measured at different time points (panel data)
#Same concept measured through different methods (Inglehart and Schwartz values)
#Different aspects of the same concepts measured through the same scales

#V23 - satisfaction with life as a whole
#V59 - financial satisfaction

wvs6_rus2<-wvs6_rus[,c("V2", "V23", "V59")]
colnames(wvs6_rus2)<-c("Country", "Lifesat", "Finsat") # this is just an example, do not rename variables in real practice
dim(wvs6_rus2)

#install.packages("expss") # tables https://github.com/gdemin/expss
library(expss)
ls<-fre(wvs6_rus2$Lifesat); ls
expss_output_viewer() # using viewer instead of console
expss_output_default() # using console again

fs<-fre(wvs6_rus2$Finsat); fs
par<-par(mfrow=c(1,2)) # several graphs, doesn't work with ggplot2, use ggpubr 
hist(wvs6_rus2$Finsat, probability = T)
hist(wvs6_rus2$Lifesat, probability = T)
par<-par(mfrow=c(1,1))# 1 row, 1 column
mean(wvs6_rus2$Finsat, na.rm=T)
mean(wvs6_rus2$Lifesat, na.rm=T)

t.test(wvs6_rus2$Finsat,wvs6_rus2$Lifesat, paired=T) 
t.test(wvs6_rus2$Finsat,wvs6_rus2$Lifesat) # incorrect in that case
# Incorrect degrees of freedom, they are twice larger than sample size!!!!!

