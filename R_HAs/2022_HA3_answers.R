# HOME ASSIGNMENT 3

# 1.	Make a new object with dataset of WVS-6 wave 
library(haven)
wvs6<-read_sav("/Users/Natasha/Downloads/WV6_Data_sav_v20201117.sav",  user_na = FALSE) 

#or
setwd("/Users/Natasha/Downloads")
wvs6.1 <- read_sav("WV6_Data_sav_v20201117.sav", user_na = F)

# 2.	Check class of the object
class(wvs6)
class(wvs6.1)

# 3.	Check number of dimensions
dim(wvs6); dim(wvs6.1)

#4.	Make a new object with data of respondents from Turkey
wvs6_turk<-wvs6[as_factor(wvs6$V2)=="Turkey", ]

#or
attributes(wvs6$V2)
print_labels(wvs6$V2)
wvs6_turk_1<-wvs6[wvs6$V2== 792, ]

dim(wvs6_turk); dim(wvs6_turk_1)

# 5.	Check gender (V240) distribution in raw values
table(wvs6_turk$V240)
attributes(wvs6_turk$V240)
#or
table(as_factor(wvs6_turk$V240))
#or
addmargins(table(wvs6_turk$V240))

# 6.	Make a new object with data for respondents from Turkey with 3 variables: 
# V240 (sex), V242 (age), and V11 (state of health)
# 7.	Check dimensions and class of the new object
turkey <- wvs6_turk[, c("V240", "V242", "V11")]
dim(turkey)
class(turkey)

#or
turkey1 <- wvs6[wvs6$V2== 792, c("V240", "V242", "V11")]
dim(turkey1)
class(turkey1)

# or
turkey2 <- wvs6[as_factor(wvs6$V2)=="Turkey", c("V240", "V242", "V11")]
dim(turkey2)
class(turkey1)

# 8.	Check distribution of state of health (V11) in raw values
table(turkey$V11)
addmargins(table(turkey$V11))
#or
addmargins(table(wvs6_turk$V11))

# 9.	Check the distribution of state of health (V11) in percentages
prop.table(table(turkey$V11))
prop.table(table(turkey$V11))*100 #%
round(prop.table(table(turkey$V11))*100, 2) #round

# 10.	Add total sum to the distribution from the previous task
addmargins(prop.table(table(turkey$V11))) #add sum
round(addmargins(prop.table(table(turkey$V11))), 2) #round
round(addmargins(prop.table(table(turkey$V11)))*100, 2) #make %

# 11.	Check labels for the variable V11 – state of health
print_labels(turkey$V11)
attributes(turkey$V11)

