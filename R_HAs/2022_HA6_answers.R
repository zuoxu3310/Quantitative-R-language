# Home Assignment 6

#1.	Read data from the 6th wave of World Value Survey.
setwd("/Users/Natasha/Downloads")
getwd()
library("haven")
wvs6<-read_sav("WV6_Data_sav_v20201117.sav")
dim(wvs6)
class(wvs6)

#2.	Create a new dataset which includes respondents from Sweden.
swed <- wvs6[as_factor(wvs6$V2)== "Sweden",]
dim(swed)
table(droplevels(as_factor(swed$V2)))

#or
attributes(wvs6$V2) #to search for the country code
swed1 <- wvs6[wvs6$V2==752,]
dim(swed1)

#3.	Check scales of intolerance towards People of a different race (V37), 
# Immigrants/foreign workers (V39), People of a different religion (V41), 
# People who speak a different language (V44)
print_labels(swed$V37)
print_labels(swed$V39)
print_labels(swed$V41)
print_labels(swed$V44)
# the question in the codebook is "Would not like to have as neighbors:..."
# all the scales are the same 1-Mentioned, 2-Not mentioned 

# 4.	Recode reverse scales if necessary. Recode scales so that all 4 variables are measured from 0 to 1. Check recoding.

# to make an index of tolerance we should have 0 as Mentioned- "Would not like to have as neighbors:..."
# and Not mentioned as 1 because not mentioning intolerance is about tolerance:)
# So, let's reverse the scales and rescale them to 0-1
swed$race<-scales::rescale(as.numeric(swed$V37), to=c(0,1))  
table(as_factor(swed$V37), swed$race) #check
swed$immigr<-scales::rescale(as.numeric(swed$V39), to=c(0,1))  
table(as_factor(swed$V39), swed$immigr)
swed$rel<-scales::rescale(as.numeric(swed$V41), to=c(0,1))  
table(as_factor(swed$V41), swed$rel)
swed$lang<-scales::rescale(as.numeric(swed$V44), to=c(0,1))  
table(as_factor(swed$V44), swed$lang)

#5.	Create index of social tolerance using those variables.
swed$tol_ind <- rowMeans(swed[c("race","immigr", "rel", "lang")], na.rm=T)

#6.	Create frequency table for the index.
table(swed$tol_ind, useNA = "ifany")
sum(is.na(swed$tol_ind)) #check NAs

#7.	Check if index is correct with plots 
plot(swed$tol_ind, swed$race)
plot(swed$tol_ind, swed$immigr)
plot(swed$tol_ind, swed$rel)
plot(swed$tol_ind, swed$lang)

#8.	Create a new subsample of Sweden males. Check recoding.
print_labels(wvs6$V240)
swedmale <- wvs6[as_factor(wvs6$V2)== "Sweden" & as_factor(wvs6$V240) != "Female",]
dim(swedmale)
table(as_factor(swedmale$V240)) #no females, only males
table(droplevels(as_factor(swedmale$V2))) #only Sweden

