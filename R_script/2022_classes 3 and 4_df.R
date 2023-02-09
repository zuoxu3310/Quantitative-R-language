#### Anna Almakaeva, Natalia Mikhailova ###
############ Class 3 ##############
# Data frames

# Part 1. Introduction

# A data frame is a combination of vectors (matrices) of different types. 
# Data frame can be treated as a set of vectors which are combined into columns.
# Variables (characteristics, questions) are located in columns. Each column is a vector 
# Data frame has a unique id for each row 

# Real data on trust accross countries from the 6th wave of the WVS
# file:///D:/Internet/F00001101-WV6_Official_Questionnaire_v4_June2012.pdf 
#V104. Trust in known people
#V105. Trust in strangers
#V107. Trust in people of another nationality
#Share of people who trust completely and trust somewhat (answers 1 and 2)
# Selected 12 countries 

# create several vectors
Code<-c(604, 458, 716, 156, 398, 410, 643, 233, 528, 724, 840, 752); Code; class(Code); length(Code)
Country<-c("Peru","Malaysia","Zimbabwe","China","Kazakhstan","SouthKorea","Russia","Estonia",
           "Netherlands","Spain","US","Sweden"); Country; class(Country); length(Country)
Trknown<-c(45,77,66,79,80,84,83,91,89,89,93,97); Trknown; class(Trknown); length(Country)
Trunk<-c(NA,7,11,13,19,19,21,23,24,33,39,60); Trunk; class(Trunk);length(Trunk)

# Important!
#NA-missing data
# Don`t use "NA", R treats it as a character

# Compare 
Trunk2<-c("NA",7,11,13,19,19,21,23,24,33,39,60); Trunk2; class(Trunk2);length(Trunk2)
class(Trunk); class(Trunk2)

# Calculations are not possible with character data! 
Trnat <-c(11,14,NA,18,57,35,43,57,42,50,69,86); Trnat; class(Trnat); length(Trnat)

# Combine all vectors into a data frame
# All vectors must have the same length!!!
trust<-data.frame(Code, Country, Trknown, Trunk, Trnat); trust # R assign a unique code as 1st column
rm(Code, Country, Trknown, Trunk, Trnat, Trank2) # remove separate vectors

class(trust)
dim(trust)
head(trust,5) # first 3 rows
tail(trust,1) # last 3 rows
names(trust)
variable.names(trust)

# Checking the class of variables
# Use $ to specify the variable (column) 
class(trust$Country)
class(trust$Trknown)
class(Country)
class(Trknown)

# Some useful functions

mean(trust$Trknown)
max(trust$Trknown)
min(trust$Trknown)
mean(trust$Country)

# Calculations with missing data

mean(trust$Trunk) # not working because of missing data
print(trust$Trunk)
sum(is.na(trust$Trunk)) #how many NAs 
mean(trust$Trunk, na.rm=T) # na.rm doesn`t erase data
max(trust$Trunk, na.rm=T)
min(trust$Trunk, na.rm=T)
range(trust$Trknown)

#Summary for the whole data frame
summary(trust)

# Assigning new labels
names(trust)<-c("id", "country", "trustknown", "truststrangers", "trustanothernat")
trust 

# Changing a column name
# Don`t overwrite the original variables`

colnames(trust)[colnames(trust)=="trustanothernat"]<-"trannat"
trust

# Assigning labels while creating data
# Using user unique id

trust2<-data.frame(cntr=Country, trkn=Trknown, trunk=Trunk, trannat=Trnat, row.names = T); trust2
dim(trust2)

# compare data frames
trust
trust2
dim(trust); dim(trust2)

#  Why trust2 has less columns? 

# CA. Create a 4 vectors with 5 elements
# 1 vector-character
# 1 vector-numeric
# 1 numeric vector with missing data
# one vector with unique id
# create a dataframe using your own id as fist column
# check number of dimensions
# show first 2 rows
# show last two rows
# show all variable names 
# detect class of the data frame
# detect classes of all variables (columns)
# calculate mean, max and min for two numeric variables
# assign new variables names (column names)

# reading data from a file 
# https://www.statmethods.net/input/importingdata.html
install.packages("readxl")
library("readxl")
trustall<-read_excel("/Users/Natasha/Desktop/TEACHING/CSR/trust.xlsx", sheet = 1, 
                     col_names = TRUE, col_types = NULL, na = "", skip = 0)
# https://www.rdocumentation.org/packages/readxl/versions/0.1.1/topics/read_excel
trustall
dim(trustall) # interpret
View(trustall)
summary(trustall)

# Using id as row names
trustall2<-data.frame(read_excel("/Users/Natasha/Desktop/TEACHING/CSR/trust.xlsx", sheet = 1, 
                                col_names = TRUE, col_types = NULL, na = "", skip = 0), row.names = T)

# compare
dim(trustall); dim(trustall2)

# Reading data from csv file

trustall3<-read.csv("/Users/Natasha/Desktop/TEACHING/CSR/trust.csv", sep=";", dec = ",") # reading from a file
dim(trustall3)

# Indexind data frames 
# Similar to matrix indexing
trustall3

# Select countries wit trust in known people higher than 50%
trustall3[trustall3$Trknown>=50,]
dim(trustall3[trustall3$Trknown>=50,]) # 48 countries

# Select countries with trust in strangers higher than 50%
trustall3[trustall3$Trunk>=50,]

# Select countries where either trust in strangers higher than 50% or trust in another nationality higher than 50%
trustall3[trustall3$Trunk>=50 | trustall3$Trnat>=50,]

# Select countries where trust in known people higher than 50% amd trust in people of another nationality >50%
trustall3[trustall3$Trknown>=50 & trustall3$Trnat>=50,]

# | - or
# & - and (more strict conditions)

# Select countries where Trust in another nationality higher than trust in strangers
trustall3[trustall3$Trunk < trustall3$Trnat, ]
dim(trustall3[trustall3$Trunk < trustall3$Trnat, ])

# Vise virsa 
trustall3[trustall3$Trunk > trustall3$Trnat, ]

# Select some countries
trustall3[trustall3$Country=="Peru"|trustall3$Country=="China",]

# Using country code
trustall3[trustall3$Code==604|trustall3$Code==156,]

# selecting some variables
trustall3[,c("Country", "Trunk")] 

# missing data 
# shows rows with missing data
trustall3[!complete.cases(trustall3),]

# rows without missing data (complete cases)
trustall3[!complete.cases(trustall3),]

# exclude missing data for all variables
# be careful! It erases data 
# create another object with only complete cases
# Don`t overwrite the original one

na.omit(trustall3)
dim(na.omit(trustall3))
trustall_comp<-na.omit(trustall3) # data frame without missing data

# exclude missing data. One variable (column). Trust in another nationality
is.na(trustall3)
trustall3[is.na(trustall3$Trnat),]
trustall3[!is.na(trustall3$Trnat),] # !is.na  - not equal to missing data



# CA. Calculate mean, max and mean for trust in people of another nationality 
# select complete cases (without missing data) for trust in unknown people. Assign them into a new object
# check dimensions
# Calculate mean, max and mean for trust in people of another nationality again
# Compare results. Draw conclusions
# Select countries with trust in known people less than 50% 
# Select countries with trust in people of another nationality less than 50% 
# Select countries with trust in known people less than 50% and trust in another nationality less than 50% 
# Select two any countries in a new object
# Exclude colunm "code" from data and assign the result in the new object. 

# !!!! Creating new variables
# One of the theories on how trust of in people in general emerge is
# 'Trust in strangers is learned through trust in close circle (e.g. known people)'
# If this theory is correct trust in known people should be equal or higher than trust in unknown people, not less

# trust in known people - indicators of ingroup ties
# trust in unknown people - indicators of generalized trust 

# To check it, create a new variable which tests the difference between trust in known people and unknown people

trustall3$difknounk<-trustall3$Trknown-trustall3$Trunk
difkno_un <- trustall3$Trknown-trustall3$Trunk #INCORRECT! CREATES A SINGLE VECTOR 

# the new variable must be attached to the data frame
# check dimentions
dim(trustall3)
head(trustall3,2)

# Use indexing to check number of countries

dim(trustall3[trustall3$difknounk>=0,]) # 50 countries have trust in known people > trust in unknown people
trustall3[trustall3$difknounk<=0,] # 0 countries have trust in known people < trust in unknown people
trustall3$difknounk

# more clear, but more complicated way
# ifelse () function
# creates dummy (dichotomous) variables, "1 or 0" or "yes or no"
trustall3$kn_unk_dif_dum <- ifelse(trustall3$difknounk>=0, 1, 0)
# If trustall3$difknounk>=0, than kn_unk_dif_dum =1, otherwise = 0

dim(trustall3)
head(trustall3,10)

# table shows distribution of a variable 
table(trustall3$kn_unk_dif_dum, useNA = "ifany")

# assigning labels
trustall3$kn_unk_dif_duml<-factor(trustall3$kn_unk_dif_dum, labels=c("less", "more"), 
                                  levels=c(0,1), ordered = T)
dim(trustall3)
head(trustall3, 3)
class(trustall3$kn_unk_dif_duml)
levels(trustall3$kn_unk_dif_duml)
is.ordered(trustall3$kn_unk_dif_duml)

# ordered factor means that R recognizes order, larger>smaller
table(trustall3$kn_unk_dif_duml, useNA = "ifany") # smaller value goes first

# It seems that there are no countries with trust in unknown people less than trust in known people

# CA! 
# there is a suggestion that trust inunknown people and trust in strangers are both indicators of trust 
# in people in general.
# They are identically good to measure generalized trust
# If yes, than the level of trust in unknown people and level of trust in people of another nationality 
# should be more or less equal to each other
# Check it. 
# Show countries where this hypothesis is correct and incorrect




