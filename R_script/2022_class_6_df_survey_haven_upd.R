##################################################
####### Anna Almakaeva, Natalia Mikhailova #######
#####Reading survey data, data transformations####
##################### class 6 ####################
# it is a part from the previous script where we have stopped last week


getwd()
setwd("/Users/Natasha/Downloads") # \\ or /, otherwise  - error message 

library("haven")
wvs6_1<-read_sav("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav",  user_na = FALSE) 

#########################STOPPED CLASS 5 HERE
# Combining several conditions
wvs6_rus3<-wvs6_1[as_factor(wvs6_1$V2)=="Russia" & as_factor(wvs6_1$V240)!="Male",] # Russian females
dim(wvs6_rus3)
table(wvs6_rus3$V240, useNA = "ifany")
table(wvs6_rus3$V2)

# want labels? use as_factor()
table(as_factor(wvs6_rus3$V240), useNA = "ifany")


# subset function. repeat from previous class/ learn yourself
# wvs6_rus4<- subset(wvs6_1, V2==643,)
# wvs6_rus5<- subset(wvs6_1, V2==643 & V240!=2, select = V2:V10) # variables from 2 to 10 
# wvs6_rus6<- subset(wvs6_1, V2==643& V240!=2 & V242<19,select = c(V2,V10, V242, V240)) # variables 2 and 10


##### Data transformations

# Reverse order
# happiness, 4-point scale, 1 - high happiness, 4- low happines
# Better reverse to 1 - low happiness, 4  - high happiness
# For better and more straighfroward interpretation 
print_labels(wvs6_1$V10)

# 1st way,the fastest

wvs6_1$hapnumrev<-5-as.numeric(wvs6_1$V10)

dim(wvs6_1) # new column should appear in the data
table(wvs6_1$hapnumrev, useNA = "ifany") # as_factor doesn`t work any more`
# don`t forget to check
table(wvs6_1$V10, useNA = "ifany") # as_factor doesn`t work any more`
table(wvs6_1$V10, wvs6_1$hapnumrev, useNA = "ifany")
class(wvs6_1$hapnumrev)
# Want to add labels and create a labelled vector? 
# labelled () from haven

wvs6_1$hapnumrev<-labelled(wvs6_1$hapnumrev, 
                           c("unhappy" = 1, "rather unhappy" = 2, 
                             "rather happy" = 3, "happy" = 4))
class(wvs6_1$hapnumrev)# double 
table(as_factor(wvs6_1$hapnumrev), useNA = "ifany")

# check
table(droplevels(as_factor(wvs6_1$V10)), as_factor(wvs6_1$hapnumrev), useNA = "ifany")

table(as_factor(wvs6_1$V10), as_factor(wvs6_1$hapnumrev), useNA = "ifany")

# ! IMPORTANT
# haven reads data differently compared to foreign or readRDS
# It reads variables as labelled vectors
# Most of them are numeric and allows calculations without additional specifications
# foreign and recoding functions from other packages CHANGE VARIABLE TYPE
# You may get numeric, factor (or ordered factor), or character
# Calculations are not possible with characters and factors
# You need to change a variables type

# Example 
# 2nd way, longer way of recoding, changes variable type

wvs6_1$hapnumrev2<-NA
wvs6_1$hapnumrev2[wvs6_1$V10==1]<-4
wvs6_1$hapnumrev2[wvs6_1$V10==2]<-3
wvs6_1$hapnumrev2[wvs6_1$V10==3]<-2
wvs6_1$hapnumrev2[wvs6_1$V10==4]<-1
table(wvs6_1$V10, wvs6_1$hapnumrev2, useNA = "ifany")
class(wvs6_1$hapnumrev2)
# check
table(droplevels(as_factor(wvs6_1$V10)), wvs6_1$hapnumrev2, useNA = "ifany")

# 3nd way using car
library(car)
wvs6_1$hapnumrev3<-recode(as.numeric(wvs6_1$V10), "1=4; 2=3; 3=2; 4=1") # initial = new value
# check
table(droplevels(as_factor(wvs6_1$V10)), wvs6_1$hapnumrev3, useNA = "ifany")

# How to assign labels? Use factor()
table(wvs6_1$hapnumrev2)
wvs6_1$hapnumrev2<-factor(wvs6_1$hapnumrev2, 
                          labels = c("Unhappy", "Rather Unhappy", "Rarther happy", "Happy"),  
                                     levels=c(1,2,3,4), ordered=T)

# keep order for ordered variables while recoding to avoid mistakes

table(wvs6_1$hapnumrev2)
table(as_factor(wvs6_1$V10), wvs6_1$hapnumrev2, useNA = "ifany")
class(wvs6_1$hapnumrev2)
levels(wvs6_1$hapnumrev2)
max(wvs6_1$hapnumrev2, na.rm = T)
mean(wvs6_1$hapnumrev2, na.rm = T) # to calculate mean change variable type

mean(as.numeric(wvs6_1$hapnumrev2), na.rm = T)
class(wvs6_1$hapnumrev2)
mean(wvs6_1$hapnumrev, na.rm=T)# created by haven 
mean(as_factor(wvs6_1$hapnumrev), na.rm=T) # not working for as_factor

# Conclusion! 
# SPSS data has values (numbers) and labels (words)
# haven reads data and keeps both numbers and labels
# haven trat variables as labelled vectors
# by default it treats and shows them as numbers
# if you want to see labels use as_factor() before variable name, but calculations are impossible in this case
# To keep double information use labelled () from haven after recoding
# foreign and other recoding functions treat information either as numeric or as character(factor)
# to do calculations with factors variables change type using as.numeric ()
# as_factor () and as.numeric() are different functions
# as_factor fits double haven data, as.factor () doesn`t 

# CA, Check recoding for each step. Check dimensions for each step
# Recode V10 happines into dummy variable, where 1- happy, 0 - unhappy
# Recode keeping double data about values and labels
# Recode as a dummy ordered factor variable "happy" and "unhappy"
# Recode as dummy 1, 0. 1 - happy people, 0- ubhappy people
# Calculate means for recoded data using haven function and numeric dummy variable 0,1. 

# Changing a scale into different minimum and maximum
# V10  - happines, 1 - Very happy, 4 - Not at all happy
# target scale: 0 - low happiness, 1 - high happiess


wvs6_1$hap01<-round((4-as.numeric(wvs6_1$V10))/3, 2)
table(wvs6_1$V10, wvs6_1$hap01, useNA = "ifany")

# step by step 
#1 
table(as.numeric(wvs6_1$V10), useNA = "ifany") # this is the initial scale transformed into numeric
#2 invert scale, (opposite order), minimum 0, maximum -3
wvs6_1$hap01<-4-as.numeric(wvs6_1$V10)
table(wvs6_1$hap01, useNA = "ifany")
# devide by 3 to get a scale from 0 - minimum, to 1 - maximum 
wvs6_1$hap01<-(4-as.numeric(wvs6_1$V10))/3

table(wvs6_1$hap01, useNA = "ifany")
# round to 2 digits after comma
wvs6_1$hap01<-round((4-as.numeric(wvs6_1$V10))/3, 2)
# check recoding 
table(wvs6_1$V10, wvs6_1$hap01, useNA = "ifany")

# fastest way
library("scales")
wvs6_1$hap01a<-rescale(as.numeric(wvs6_1$V10), to=c(1,0)) # not working with double, change into numeric type
# We have a reverse scale for happines, therefore c(1,0), not c(0,1). 
table(droplevels(as_factor(wvs6_1$V10)),wvs6_1$hap01a, useNA = "ifany")

### Decreasing number of scale points 
### Useful when some of them have few observations
table(wvs6_rus$V248, useNA = "ifany")
print_labels(wvs6_rus$V248)
# combine together 1-4
wvs6_rus$edu<-ifelse(wvs6_rus$V248<=4, 4, wvs6_rus$V248)
table(wvs6_rus$edu)
class(wvs6_rus$edu) # not a double any more
# check the recoding 
table(wvs6_rus$V248, wvs6_rus$edu, useNA = "ifany")

# CA. Turn this into ordered scale which starts with 1, not 4
# Assign labels keeping type double (haven)
# Assign labels transforming this variable into ordered factor
# Don`t forget to check recoding. `

# Transforming metric variables

table(wvs6_rus$V242, useNA = "ifany")

wvs6_rus$ageord<-cut(wvs6_rus$V242, breaks=c(17, 22, 29, 39, 49, 59, 69, 100), ordered_result =T)
# Specify lowest and highest pount
table(wvs6_rus$ageord, useNA = "ifany")
table(wvs6_rus$V242, wvs6_rus$ageord, useNA = "ifany")
class(wvs6_rus$ageord)

#2nd way

wvs6_rus$ageord2<-car::recode(wvs6_rus$V242, 
                              "lo:22=1; 23:29=2; 30:39=3; 40:49=4; 50:59=5; 60:69=6; 70:hi=7")
table(wvs6_rus$ageord2, useNA = "ifany")
table(wvs6_rus$V242, wvs6_rus$ageord2, useNA = "ifany")
class(wvs6_rus$ageord2)
mean(wvs6_rus$V242, na.rm=T)
mean(wvs6_rus$ageord2, na.rm =T) 


# ordered scales 
# Dimension reduction should be based on the: 
# number of points (odd, even)
# theoretical assumptions
# real distribution (or distributions if you use several groups (countries). 

# Using the same card please tell me how likely it is that most people
# in [country] view those over 70 friendly?
# V161
print_labels(wvs6_rus$V161)
table(wvs6_rus$V161, useNA = "ifany")
wvs6_rus$oldfr<-car::recode(wvs6_rus$V161, "0:1=1; 2=2; 3:4=3")
wvs6_rus$oldfr<-labelled(wvs6_rus$oldfr, 
                         c("not likely" = 1, "neither" = 2, 
                           "likely" = 3))

table(as_factor(wvs6_rus$oldfr), useNA = "ifany")
table(wvs6_rus$oldfr, useNA = "ifany")
table(as_factor(wvs6_rus$V161), as_factor(wvs6_rus$oldfr), useNA = "ifany")
class(wvs6_rus$oldfr)
print_labels(wvs6_rus$oldfr)

# NA specification
wvs6_2<-read_sav("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav", user_na = TRUE)
dim(wvs6_2)
table(wvs6_2$V10)
wvs6_2$hap<-ifelse(wvs6_2$V10<0, NA,wvs6_2$V10)
table(wvs6_2$hap, useNA = "ifany")
table(wvs6_2$V10,wvs6_2$hap, useNA = "ifany")

# Conclusion! 
# You can recode varirables with recode() function from car package
# You can recode variables manually by adding new values regarding the initial variable separately
# To assign labels, use labelled() from car package or factor()
# Also, you can use ifelse() to recorde some simple cases^ i.e. with 2 options or to make NAs 
## that are less/more than some number 

