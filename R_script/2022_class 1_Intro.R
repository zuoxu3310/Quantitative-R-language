###Anna Almakaeva, Natalia Mikhailova###
############ Lecture 1 ##############
############Introduction, Vectors#############

# Use # to treat the line as comment not function. 



# #####Downloading R and R studio####### ----------------------------------

# https://www.r-project.org/
  
# https://www.rstudio.com/products/rstudio/download3/

# First install R, than R Studio 

# Online R cloud for using it online https://rstudio.cloud/projects 


# ############ Getting and setting working directory######### -------------
# run line  - Ctrl+Enter
# run line  - Cmd+Enter (Mac)


getwd()

setwd("D:/Research/Well_being") # windows
setwd("D:/clouds/1_Lectures/CSR/BS/2021")
setwd("D:\\Research\\Trust")
#For MAC users 
setwd("/path/to/my/directory")
# http://www.sthda.com/english/wiki/running-rstudio-and-setting-up-your-working-directory-easy-r-programming
setwd("/Users/Natasha/Downloads")
getwd()


# IMPORTANT!!!!!
# USE LATIN ALPHABET 
# IF YOU SEE A MISTAKE:
# Error in setwd("D:/Research/Well_being") : cannot change working directory
# CHECK THE PATH, MAYBE IT IS INCORRECT
# CREATE THE FOLDER FISRT AND SET A NEW WD AFTERWORDS
# CHECK SLASH 
# CHECK SPELLING, R IS A CASE SENTIVE SOFTWARE

# CA! 5 minutes
## create any folder on your laptop
## write a code in RStudio with the directory of your folder


# ############ R packages #########  ----------------------------------------

############installing packages######### 
#By default R has basic functions
#To save resources additinal functions are stored at specific packages
# In most cases to do something you have to install and attach a required package

# using menu 

install.packages("car") # data transformations, regression
install.packages("haven") # read spss, stat, excel files 
install.packages(c("haven", "Car"))
install.packages("car", lib="D:/Rpacks") # sets location to store packages

#########attaching a package#######
# you have to attach a package to use it

library("car", "haven")
library("car")
library("haven")
library("car", lib.loc ="D:/Rpacks") # specify your path

#########deattaching a package#######
# Sometimes packages may conflict (e.g. Hmisc)
# to solve this problem either detach a conflicting package 
# or specify a package before function. E.g. car::recode
detach(package:car, unload=TRUE)
detach(package:Hmisc, unload=TRUE)


######Getting help######### ----------------------------------------------

help.start() # general help
help("mean") # help for function "mean"
?mean # help for function "mean"
help.search("mean") #
??mean # all functions where mean is used
example("mean") #examples
RSiteSearch("mean") # online search  
#https://www.dummies.com/programming/r/how-to-do-basic-arithmetic-in-r/



#####Using R as calculator##### ------------------------------------------

2+2
2*3
2/2
2^10 # power
2**10 # another way of getting power
11%%3 # modula devision, return the remainder of division
11%/%3 # whole part of the division.
pi #  pi number

cos(pi) # cosine
sin(pi) # sine
exp(1) # exponent
log(1) # natural logarithm (by default)
log2(10) #binary logarithm
log10(100) # logarithm with base 10
log(4, base=2) # logarithm of 4 with base 2
tan(0) # tangent
abs(-5) #absolute value
sqrt(144) #square root
factorial(10) # factorial


#####Assigning##### ------------------------------------------------------

# to store and keep results in object use assigning
# used in 99% of cases, since it saves time

2*10+5
x<-2*10+5 
x # show x object
print(x) # here you can omit print() function 
show(x)
x+3+5

X<-100*100-10*10 # R is a case sensitive software
X

# possible to use "=" for assigning, but not recommended

# calculations with objects

X+x
a<-X+x; a # ";" separates two fuctions in one line
b<-x^2;b # power
d1<-sqrt(b); D1
A+b

# rounding up
c<-0.05^2
print(c)
round(c,3)# 3 digits, after comma

#small number and switching off scientific notation 
c2<-0.05^10;c2

# if you see a strange number
options(scipen=999) #swith off scientific notation 
c2
options(scipen=0) #swith on scientific notation back
c2 

# CA! (CLASS ASSIGNMENT). 10 MINUTES
# CREATE TWO OBJECTS USING CALCULATIONS
# USE ROUNDING UP (3DIGITS) IF NECESSARY
# MULTIPLY OBJECTS
# SUM THEM UP
#DEVIDE THEM
#EXTRACT ONE FROM ANOTHER
#FIND A SQUARE ROOT OF MULTIPLICATION
#RISE OBJECT ONE IN THE POWER OF OBJECT TWO


#####objects#####

objects() #printing objects
ls() #printing objects
rm(X) # removing objects
rm(list=ls()) # removing all objects



######################
#####Data types#######
######################

# numeric(integer as special case)
# character(factor as special case)
# logical/Boolean/(0,1)/(TRUE/FALSE)

# Data structures
#vectors  - 1 dimention
#matrices - >1 dimensions, the same data type in all cells
#data frames - >1 dimensions, different data types in cells
#lists - collection of several different objects

string<-"just a text"
string
class(string)
sum(string) 

numeric<-3.34
numeric
class(numeric)

integer<-as.integer(5)
integer
class(integer)

integer2<-as.integer(numeric) # erases the fraction part
integer2
class(integer2)

sum(integer, integer2)
class(sum(integer, integer2)) # use () to use several functions at once
sum(integer, integer2, numeric)
class(sum(integer, integer2, numeric))

#Important note! 
numeric3<-"3.34" # "" turns numbers into characters 
class(numeric3)
sum(numeric3)


logical<-TRUE
logical
class(logical)
sum(logical)
# Why? 
# TRUE=1; FALSE=0

logical2<-FALSE
logical2
sum(logical2)
logical+logical2 

logical0<-"TRUE"
class(logical0)

#####Creating numeric vectors#####

#VECTOR IS A SEQUENCE OF ELEMENTS(NUMBERS, LETTERS, WORDS ETC.), one-dimensional object
#IMPORTANT!!! REAL SURVEY DATASET IS A COMBINATION OF VECTORS ARRANGED IN COLUMNS
#TO LEARN HOW TO TRANSFORM DATA YOU HAVE TO LEARN HOW TO DEAL WITH VECTORS
# VECTORS ARE BASICS OF SUCCESSFUL RECODING

y1<-1:5
y2<-c((1:4),5) # c - combines elements (concatinate)
y3<-c((1:4),100)
y4<-c(1,2,3,4,5)
y1; y2; y3;y4
show(y1)
print(y1)

z1<-seq(1,5)
z2<-seq(1, 9, by=2) #step (difference between two elements)=2
z3<-seq(1, 9, by=0.2)
z4<-seq(1, 9, length=5) # number of elemets=5, step is fitted automatically
z5<-seq(1, 9, length=3)
z1;z2;z3;z4;z5

length(z3) # total number of elements
length(z2)

######Replication########
y1
length(y1)
rep(y1,3) # repeats a sequence
length(rep(y1,3)) # 15 elements
rep(y1, each=3) # repeats an element

#assigning labels to a vector 
# DC course to learn this part
LETTERS[1:5] # Capital letters from 1 to 5
letters[6:10] # 
z1
class(z1) # integer = without fractions
# to compare 
z3
class(z3) # numeric

names(z1)<-LETTERS[1:5] #assigning labels to a vector
z1

#Another way
z2
names(z2)<-c("aa","bb", "cc", "dd")
z2
names(z2)<-c("aa","bb", "cc", "dd", "ee"); z2


#!CA. 15 minutes
# Using all possible fucntions create 6 different vectors and assign them to 6 objects
# Using different ways assign labels to 3 out of 6 vectors


# Logical and string vectors

l<-c(TRUE, FALSE, FALSE, TRUE) #logical vector
l
w<-c ("A", "B") # (string character) vector
w
s<- c("I", "Like", "Statistics") #string vector


# calculations with vectors
y3;y2
y3+y2
length(y3)
length(y2)
y3^2

#! CA. 5 minutes
# Create 2 logical and two string vectors

# change into decreasing order use help
# DC course to learn this part
?sort
??sort
y1
yr2<-sort(y1, decreasing = TRUE)
yr2
s # 
sr<-sort(s, decreasing = T);sr 


##### indexing vectors (selecting elements from a vector )####
# This fuction will be used for selecting subsets from survey datasets
# https://www.datamentor.io/r-programming/operator/
s
s[2]
s[c(3,1,2)] #separate elements
s2<-s[c(3,1,2)] #separate elements assigned to a new object
s[-3]  # omitting elements
s[1:2] # sequence
z1["A"] # using labels
z2["bb"] 

# Conditional selection
y1
y1[y1==1] # all elements equal to 1
y1[y1>1] # all elements more than 1

l
l[l==TRUE] # TRUE elements
l[l!=TRUE] # FALSE elements, !=not equal 
l[l!=1]


### logical vectors######
# check if condition is satisfied # 
y3; y2
y3==y2 # double equality sign
y3>y2
y3>=y2


######### SUMMARY ###############
# Calculations
# Assigning (creating object)
# Vector types
# Creating vector of different types
# Assigning labels
# Indexing vectors
# 
# 
# FUNCTIONS
# getwd() # getting working directory
# setwd() # setting working directory
# install.packages()# installa package
# library()#attach a package
# detach(package:"", unload=TRUE) # detach a package
# help() # help
# <- "assigning"
# round() # round up
# sqrt() # square root
# ^ # power
# options(scipen=999) # switch off strange numbers
# objects() #printing objects
# ls() #printing objects
# rm(X) # removing objects
# rm(list=ls()) # removing all objects
# x:y # sequence from x to y
# seq(x,y) #sequence from x to y
# length() # number of elements
# rep()# replication
# class()# type of object
# names(x)# assigning labels to object
# sort() # sorting elements
# []#indexing object

