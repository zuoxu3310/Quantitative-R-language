 ##Home assignment 3 correct answers and common mistakes##
 ##Lists

#1.	Create a labeled matrix of (2,3) dimensions and assign it to a new object
a <- c(2,5,9)
b <- c(17,25,33)
matr <- rbind(a,b)
dim(matr)
rownames(matr) <- c("Ted","Robin")
colnames(matr) <- c("Q1","Q2","Q3"); matr

#2.	Create a string vector with 7 elements and assign it to a new object
c <- c("Somebody","Once","Told","Me", "The", "World", "Is")
c

#3.	Save the results of this expression (2^4-sqrt(9)-3*6) into a new object
d <- 2^4-sqrt(9)-3*6
d

#4.	Create a numeric vector with 4 elements
e <- 1:4
e

#5.	Sum up the vector of 4 elements and the result of the calculation and assign it to a new object
f <- e+d
f
#other possible solution
f1<-sum(e,d); f1

#6.	Combine these 4 objects into a list
g <- list(matr,c,d,f)
g

#7.	Select the 1st and 3rd objects from the list
g[[1]]
g[[3]]

#8.	Select the first elements from each object of the list
g[[1]][1,1]
g[[2]][1]
g[[3]][1]
g[[4]][1]

#9.	Select the last element from string vector in the list
length(g[[2]])
g[[2]][7]

#10.	Select the cell (1,2) from the matrix in the list
g[[1]][1,2]

#11.	Select the first row from the matrix in the list
g[[1]][1,]

#12.	Select the last column from the matrix in the list
g[[1]][,3]

 ##Data frames
#1.	Create the following table (data frame) and save it as data frame
#2.	Save table as data frame.
Country <- c("Russia", "Mali", "USA", "Ghana", "Germany", "Sweden", "Yemen", "Romania", "Hungary", "Brazil")
Code <- c(292, 566, 445, 219, 817, 715, 941, 120, 931, 498)
Males <- c(1025, 445, 715, NA, 1580, 515, 545, 498, 400, 224)
Females <- c(1154, 490, 729, NA, 1700, 520, 570, 548, 340, 321)
Total.number <- c(2189, 935, 1444, NA, 3280, 1035, 1115, 1056, 740, 545)
df <- data.frame(Country, Code, Males, Females, Total.number)
df
class(df)

#3.	Show the first 5 rows and 5 last rows in the data frame
head(df,5)
tail(df,5)

#4.	Check the class of all variables
class(df$Country)
class(df$Code)
class(df$Males)
class(df$Females)
class(df$Total.number)
#Do not forget to use $ to check or do anything with a variable in a data frame

#5.	Calculate mean, max, min for all numeric variables (columns)
mean(df$Code, na.rm=T)
max(df$Code, na.rm=T)
min(df$Code, na.rm=T)
#keep in mind that these statistics do not make sense as Code variable only shows assigned numbers to countries

mean(df$Males, na.rm=T)
max(df$Males, na.rm=T)
min(df$Males, na.rm=T)

mean(df$Females, na.rm=T)
max(df$Females, na.rm=T)
min(df$Females, na.rm=T)

mean(df$Total.number, na.rm=T)
max(df$Total.number, na.rm=T)
min(df$Total.number, na.rm=T)
