##################################################
##### Anna Almakaeva, Natalia Mikhailova #####
#####Reading survey data, data transformations####


#Don`t forget to set a working directory. It is convenient when working with different projects. 
#All files will be by default stored in this working directory. 
# http://www.sthda.com/english/wiki/running-rstudio-and-setting-up-your-working-directory-easy-r-programming 
getwd()
setwd("/Users/Natasha/Downloads") # \\ or /, otherwise  - error message 


# Reading r data ----------------------------------------------------------
# No additional packages are needed 
# The 6th wave of the WVS 
# https://www.worldvaluessurvey.org/wvs.jsp
# Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, 
# J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014. 
# World Values Survey: Round Six - Country-Pooled Datafile Version: www.worldvaluessurvey.org/WVSDocumentationWV6.jsp. 
# Madrid: JD Systems Institute.
# 
# R can read almost everything; txt, csv, sav, dat, sas, xlsx etc.


load("/Users/Natasha/Downloads/WV6_Data_R_v20201117.rdata") # load
# readRDS for rds files
# If not working try gc()
#gc() - A call of gc causes a garbage collection to take place. Clears memory without deleting objects 

wvs6<-WV6_Data_R_v20201117 # rewrite object with a short name for convenience 
rm(WV6_Data_R_v20201117) # remove redundant object from environment 
View(wvs6) # View data
class(wvs6)
dim(wvs6) # information about number of respondents and number of variables
nrow(wvs6) # number of respondents
ncol(wvs6) # number of variables
names(wvs6) # names of all variables 
length(names(wvs6)) # equal to ncol() (number of vars)

head(wvs6) # a mess
str(wvs6) # a mess
summary(wvs6) # a mess
# How to avoid a mess? 
# Use some variables instead of all variables
# For one variable $, indexing for several ones 
# Data frame is a collection of vectors with answers located in columns inside a data frame  
# $ tells R which variable (vector) to use

# V1 - wave
# V2 - country
# Case sensitive

head(wvs6$V1, 10)
head(wvs6$V2, 10)
str(wvs6$V2)
str(wvs6$V1)
summary(wvs6$V1)
summary(wvs6$V2) # attention! it doesn't make sense (there are ids of the countries)

# using column numbers 
head(wvs6[1:10], 10)
summary(wvs6[1:10])

# using labels
head(wvs6[c("V2", "V4","V5")], 10)

class(wvs6$V5) # importance of friends
class(wvs6$V6) # importance of leisure time

# Distribution of a variable 
table(wvs6$V2, useNA = "ifany") # shows distribution with missing values if they exist 

table(wvs6$V5, useNA = "ifany") # raw distribution in respondents
addmargins(table(wvs6$V5, useNA = "ifany")) # with total sum 
prop.table(table(wvs6$V5, useNA = "ifany")) # shares 
prop.table(table(wvs6$V5, useNA = "ifany"))*100 # proportions
round(prop.table(table(wvs6$V5, useNA = "ifany"))*100,2)
addmargins(round(prop.table(table(wvs6$V5, useNA = "ifany"))*100,2))
addmargins(table(wvs6$V5, useNA = "ifany"))

# Checking of distributions is an important part of data analysis. 
# It shows the real distribution of a scale.
# Sometimes the scale should be reduced
# Sometimes you deal with a constant

table(wvs6$V4, useNA = "ifany") # importance of family
round(prop.table(table(wvs6$V4, useNA = "ifany"))*100, 3)
# The real distribution turns a 4-point scale into 2-point scale. 

# ATTENTION! SOMETIMES MISSING VALUES ARE NOT SPECIFIED AS NA.
# IN THIS CASE YOU WILL SEE THEM IN THE DISTRIBUTION
# SUCH MISSING VALUES SHOULD BE RECODED AS MISSINGS 
# OTHERWISE CALCULATION WILL BE INCORRECT 

# Data provided by the WAVES does not contain labels for answers. 
# # This is inconvenient and may lead to false conclusions if you mix up numbers and labels

##### Reading data from SPSS files #####

# install.packages("foreign")# alternative 
# install.packages("haven") 

#####using "haven" package######
library("haven")

wvs6_1<-read_sav("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav",  user_na = FALSE) # takes much time, wait. 
?read_sav
# user_na = FALSE. Be careful, if TRUE, you need to recode missing values manually
wvs6_2<-read_sav("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav", user_na = TRUE)
dim(wvs6_1); dim(wvs6_2); 
head(wvs6_1[1:10]) # select several variables
str(wvs6_1$V6) # shows description of a variable
class(wvs6_1$V6)
attributes(wvs6_1$V6) # names? values and labels
attributes(wvs6_1$V6)$label
attributes(wvs6_1$V6)$labels
print_labels(wvs6_1$V6) # values and labels
table(wvs6_1$V6, useNA = "ifany") 

# MISSING VALUES ISSUE
# user_na = FALSE by default. Be careful, if TRUE you need to recode missing values manually
wvs6_2<-read_sav("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav", user_na = TRUE)
table(wvs6_2$V23, useNA = "ifany") # life satisfaction 
table(wvs6_1$V23, useNA = "ifany") # life satisfaction 

# calculation of means for two cases
round(mean(wvs6_1$V23, na.rm=T),3) # na.rm - omit missing values while calculationg mean 
round(mean(wvs6_2$V23, na.rm=T), 3) # a different mean for the same variable 

# Using labels instead of numbers 
head(wvs6_1[1:10])
as_factor(head(wvs6_1[1:10])) # shows labels instead of numbers

class(wvs6_1$V2) # double keeps information about values and labels
table(wvs6_1$V2, useNA = "ifany") # country codes
table(as_factor(wvs6_1$V2), useNA = "ifany") # country labels 
as.data.frame(table(as_factor(wvs6_1$V2), useNA = "ifany")) # for better presentation

# Export results to excel 
library(writexl)

countries_tab<-as.data.frame(table(as_factor(wvs6_1$V2), useNA = "ifany"))
write_xlsx(countries_tab, "/Users/Natasha/Downloads/countries_tab.xlsx")
write.csv2(countries_tab, "/Users/Natasha/Downloads/countries_tab.csv", sep=";") # csv

###### creating subsets######
###### Two options depending of data type
###### using labels 
wvs6_rus<-wvs6_1[as_factor(wvs6_1$V2)=="Russia", ] # don`t forget to ad as_factor
dim(wvs6_rus)
table(as_factor(wvs6_1$V2), useNA = "ifany")
table(as_factor(wvs6_rus$V2), useNA = "ifany")

# get rid of unused labels 
table(droplevels(as_factor(wvs6_rus$V2)), useNA = "ifany")

# or 
xtabs(~as_factor(wvs6_rus$V2), addNA = TRUE, drop.unused.levels = TRUE)
xtabs(~as_factor(wvs6_rus$V23), addNA = TRUE, drop.unused.levels = TRUE)

# selecting using country code
# how to get country code

attributes(wvs6_1$V2)
print_labels(wvs6_1$V2) # values and labels

# Russia=643
wvs6_rus2<-wvs6_1[wvs6_1$V2==643, ] # don`t use as_factor()`
dim(wvs6_rus) 
dim(wvs6_rus2)
# View(wvs6_rus)
# 
xtabs(~as_factor(wvs6_rus2$V23), addNA = TRUE, drop.unused.levels = TRUE)
xtabs(~as_factor(wvs6_rus$V23), addNA = TRUE, drop.unused.levels = TRUE)

# Check equality of two tables
# table is a labelled vector
ls_tab2<-xtabs(~as_factor(wvs6_rus2$V23), addNA = TRUE, drop.unused.levels = TRUE)
ls_tab1<-xtabs(~as_factor(wvs6_rus$V23), addNA = TRUE, drop.unused.levels = TRUE)
ls_tab2
ls_tab1

ls_tab2==ls_tab1 # creates a logical vector which compares elements, true-equal, false-not equal


wvs6_rus2a<-wvs6_1[wvs6_1$V2==643, (1:10)] # first ten variables
dim(wvs6_rus2a) 
# View(wvs6_rus2a)

wvs6_rus2b<-wvs6_1[wvs6_1$V2==643, c("V2", "V10", "V240")] # 3 variables
dim(wvs6_rus2b) 
# View(wvs6_rus2b)


# CA. Repeat for Sweden:
# select Sweden in a new dataset
# check dimentions 
# View data 
# tabulate variable V2 as labelled and get rid off empty labels. 
# Select Sweden and last 10 variables
# Check dimensions
# Select Sweden and variables V2, V4, V6, V248

# Combining several conditions
wvs6_rus3<-wvs6_1[as_factor(wvs6_1$V2)=="Russia" & as_factor(wvs6_1$V240)!="Male",] # Russin females
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
# Better reverse to 1 - low happines, 4  - high happines
# For better and more straghfroward interpretation 
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

# Want to assign labels? Use factor()
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



