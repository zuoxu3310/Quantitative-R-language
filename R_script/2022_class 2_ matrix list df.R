### Anna Almakaeva, Natalia Mikhailova ###
############ Class 2 ##############
############ Matrices,lists, simple data frames #############

########### Matrices ########
# Matrix in R - a two dimensional object, it has rows and columns 
# Matrix  - combinations of vectors of the same type 
# Use vectors to create a matrix

m1<- matrix(1:12, nrow=3)  # fills by columns by default, 1:12  - vector from 1 to 12
m1
class(m1)
class(1:12)
m2<- matrix(1:12, nrow=3, byrow=T) # fill values by row
m2
dim(m1) # checks number of rows and columns 
dim(m2)
m1t<-t(m1) #transpose or flip a matrix, changes rows and columns
m1t
dim(m1t)
dim(m1) # compare

# Matrix from a vector

m3 <- 1:10
m3 # vector can be treated as a matrix with 1 column or 1 row
class(m3)
dim(m3)
m3a<-m3
dim(m3a)<-c(10,1) #  1 column
m3a

m3b<-m3
dim(m3b) <- c(1,10) #  1 row
m3b
dim(m3b)

# what is the difference between m3a and m3b? 

#or use as.matrix(). It changes object type

m3c<-as.matrix(m3); m3c
t(m3c)

# opposite 
m3c
m3cop<-as.numeric(m3c); m3cop
class(m3cop)
m1
m1op<-as.numeric(m1); m1op

# rbind() and cbind() functions
# row bind and column bind
m4<-round(seq(1,5, length=10),2); m4
m5<-1:10; m5
m6<-rbind(m4,m5); m6
m7<-cbind(m4, m5); m7
dim(m6); dim(m7)
# what is the difference between m6 and m7? 

# addition dimentions to a matrix
# rows
m6; dim(m6)
m6ext<-rbind(m6, ad=11:20); m6ext #ad - row label, NOT an option of the function
dim(m6ext); dim(m6)
#columns 
m7
m7ext<-cbind(m7, 11:20); m7ext
#how to find a name to a new column?

# CA. (15 min) Using 3 different ways, create 3 matrices
# Add two additional rows to the 1st matrix, check dimensions
# Add two additional columns to the 2nd matrix, check dimentions
# transpose the 3rd matrix. Check dimentions for both matrices



m3<-cbind(A=1:4,B=5:8,C=9:12) # A,B,C  - assigning names to created vector
m3<-rbind(A=1:4,B=5:8,C=9:12)
cbind(1:4, 5:8, 9:12) # not assigning names to created vector
y<-1:2
z<-1:2
m4<-rbind(y,z)
m4

####### Assigning colomn names and row names#####
#m1<- matrix(1:12, nrow=3)  # copied line to remind what m1 is
m1

rn<- c("RD", "RE", "RF"); rn
rownames(m1) <- rn; m1
cn<- c("cd", "ce", "cf", "cg"); cn
colnames(m1) <- cn; m1
colnames(m1)<- NULL #delete names
rownames(m1)<- NULL # delete names
m1

#matrix with rownames and colomn names

M1 <- matrix(1:12, nrow=3, byrow=T, 
             dimnames=list(rn, cn)); M1
M2 <- matrix(1:12, nrow=3, byrow=T, 
             dimnames=list(LETTERS[1:3], letters[1:4])); M2

# Another way 
M3<-cbind(AA=1:4,BB=5:8,CC=9:12); M3 
M4<-rbind(AA=1:4,BB=5:8,CC=9:12); M4

# CA. Using different ways, assign labels to your 3 created matrices
# Delete lavels for these three matrices



##### indexing matrices ####
# row in the first place
# column in the second place
m1
m1[1,1] # 1strow, 1st col
m1[3,2] # 3rd row, 2nd col
m1[,2]  #all rows, 2nd col 
m1[2,]  #2nd row, all col
m1[-1,] # everything except 1st row
m1[,-1] # everything except 1st col
m1[,c(1,3)] # all rows, 1st and third col
m1[,-c(1,3)] # all rows, except 1st and 3rd col
m8<- m1[-1,] # assigning result to a new object
m8

#### indexing labelled matrices####
M1
M1["RD","cd"]
M1[,"cd"]
M1["RD",]
M1["RD",]
M1[c("RD","RF"),"cd"]
M1["RD","cd"]


###### 3-dimensional matrices

d1<-array(1:12, dim = c(2,3,2)); d1 # dim=c(rows, columns, layers)

##### indexing 3d matrices#####
d1[,,1] #1st layer
d1[,,2] #2nd layrr
d1[1,1,] # elements 1,1 from both layers

# CA. Create a 3d matrix with 2 layers
# By using indexing, select 3 different elements from each layer (6 in total)

######lists#####
# list  - combination of different objects
s1<- c("I", "Like", "R") # string vector
c1<-sqrt(10000)-123*3-4^5 # result of a calculation
M1 # labelled matrix
s1; c1; M1
l1<- list(s1, c1,M1); l1
class(l1)
names(l1)<-c("vector", "calc", "matrix")
l1

###### indexing lists####
# double brackets to index an object, than regular brackets to identify an element
l1
l1[[2]] #layer
l1[[1]]
l1[[3]]

l1[[2]][1] #2 layer 1 object
l1[[2]][2] # error! 
l1[[1]][2]
l1[[3]][3] # treats matrix as a vector, incorrect
l1[[3]][1,3]
l1[[3]] ["RD","cd"]
l1[["vector"]]
l1[["vector"]][1]
l1[["matrix"]][1,1]
l1[["matrix"]]["RD","cd"]

# CA. Create a list contaning 4 different elements
# Index the list and detect 3 different elements in each of 4 objects (in total 12 indices)

