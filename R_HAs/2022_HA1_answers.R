# HOME ASSIGNMENT 1 
#
# Vectors  
# 1.	Using function seq() create a vector which starts with 1 and contains 10 elements. 
#Assign this vector to an object and show this object. 
a <- seq(1,10); a
# information about number of elements 
length(a)
class(a)
#OR
aa <- seq(1,20, by=2); aa
length(aa)
# OR
aaa<-seq(1,5, length=10); aaa
length(aaa)
class(aaa)


#various creative variants how to do that

# 2.	Using function seq() create a vector which starts with 1 and contains 20 elements. 
#Assign this vector to an object and show this object.
b <- seq(1,100, length=20); b
bb <- seq(1,20); bb

# 3.	Using function rep () create a vector 111333444, assign it to a new object and show it.  
c <- rep(c(1,3,4), each=3); c # here we combine 2 functions in one
# to combine (wrap up) functions use ()

#OR two steps instead of one
c1 <- c(1,3,4)
c2 <- rep(c1, each=3); c2

# 4.	Using function rep () create a vector 134134134, assign it to a new object and show it.  
d1 <- c(1,3,4)
d <- rep(d1,3); d
#OR
d2 <- rep(c(1,3,4),3); d2

# 5.	Using concatenation c() create a vector 12349, assign it to a new object and show it.  
e <- c(1,2,3,4,9); e

# 6.	Using combination of functions create a vector 1 1 2 2 3 3 1.25 1.5 2 10 11 12 13, assign it to the new object NLV and show it.  
f1 <- c(1,2,3)              #preparational vector
f1
f2 <- rep(f1,each=2)        #first part of final vector
f2
f3 <- c(1.25, 1.5, 2)       #middle part of final vector 
f3
f4 <- seq(10,13, by=1)      #last part of final vector
f4
NLV <- c(f2,f3,f4); NLV     #final vector
length(NLV)
class(NLV)

# OR 

NLV1<-c(rep(c(1,2,3),each=2), c(1.25, 1.5, 2), seq(10,13, by=1)); NLV1
length(NLV1)

# Identical? 
NLV==NLV1 # compare each element of two vectors


# 7.	Create a new string vector containing labels for the vector NLV. Assign it to the new object SV and show it.  
SV <- c("January", "February", "March", "April", "May", "June", "July", "August",
        "September", "October", "November", "December", "Year")
SV
class(SV)

# 8.	Assign labels to the vector NLV, show this object.  

names(NLV) <- SV; NLV
length(NLV)
names(NLV)
# or
labels(NLV)

# 9.	Create a logical vector “LV” with 5 elements and show it.  
LV <- c(FALSE, FALSE, TRUE, FALSE, TRUE); LV
class(LV)

# 10.	Sum up vector 111333444 and vector 134134134. 
        ##I made objects with this vectors in previous tasks. I called them c and d, respectively. 
c;d     #I show this vectors first
c+d

# 11.	Multiply these two vectors.  
c*d

# 12.	Extract vector 111333444 from vector 134134134, show results 
extract <- d-c; extract

# 13.	Rise vector 134134134 in the power of 2, show results 
power <- d^2; power

# 14.	Sum up vector 134134134 and pi number, assign to a new object pi show results.  
pi <- d+pi; pi

# 15.	Round up the sum to 2 elements after comma, show results. 
round(pi,2) 

# 16.	Assign rounded sum into a new object pi2 and show it.  
pi2 <- round(pi,2); pi2
