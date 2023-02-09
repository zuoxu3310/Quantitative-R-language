#################################################
####### Anna Almakaeva, Natalia Mikhailova ######
######## Index and Descriptive statistics #######
##################### class 7 ###################

#####using "haven" package######
library("haven")

wvs6_1<-read_sav("/Users/Natasha/Downloads/WV6_Data_Spss_v20180912.sav") 
dim(wvs6_1)
wvs6_rus<-wvs6_1[as_factor(wvs6_1$V2)=="Russia", ]
dim(wvs6_rus)

#### Index ####
# Index of subjective well-being
# Russian sample 

table(droplevels(as_factor(wvs6_rus$V10)),  useNA = "ifany")
table(droplevels(as_factor(wvs6_rus$V23)),  useNA = "ifany")
attributes(wvs6_rus$V10)
attributes(wvs6_rus$V23)

library(scales)
# 	Hadley Wickham, Dana Seidel
# reverse order for happiness 
# recode into the same range from 0 to  1 

wvs6_rus$haprev<-scales::rescale(wvs6_rus$V10, to=c(1,0)) # error message 
wvs6_rus$haprev<-scales::rescale(as.numeric(wvs6_rus$V10), to=c(1,0))# c(1,0) not c(0,1) to get a reverse order 
wvs6_rus$haprev<-round(scales::rescale(as.numeric(wvs6_rus$V10), to=c(1,0)),3)
table(droplevels(as_factor(wvs6_rus$V10)), wvs6_rus$haprev) #check recoding

# Recode satisfaction with life 
wvs6_rus$lsatrec<-round(scales::rescale(as.numeric(wvs6_rus$V23), to=c(0,1)),3)
table(droplevels(as_factor(wvs6_rus$V23)),wvs6_rus$lsatrec) 

# index of well-being 
# Two different ways of treating NA (missing data)
# First way of treating NA 

wvs6_rus$swb<-rowMeans(wvs6_rus[c("lsatrec","haprev")], na.rm=F)
# calculation is not possible when at least 1 indicator is NA

table(wvs6_rus$swb, useNA = "ifany")

table(droplevels(as_factor(wvs6_rus$V10)),  useNA = "ifany")
table(droplevels(as_factor(wvs6_rus$V23)),  useNA = "ifany")

# To see if index is correct it is better to use correlation 
plot(wvs6_rus$swb, wvs6_rus$haprev)
plot(wvs6_rus$swb, wvs6_rus$lsatrec)

# Second way of treating NA
wvs6_rus$swb2<-rowMeans(wvs6_rus[c("lsatrec","haprev")], na.rm=T) # less NA 
# Calculation is not possible when all indicators are NA

table(wvs6_rus$swb, useNA = "ifany")

# difference 
dim(wvs6_rus)
View(wvs6_rus[(ncol(wvs6_rus)-3):ncol(wvs6_rus)])
View(wvs6_rus[c("haprev", "lsatrec", "swb", "swb2")])
sum(is.na(wvs6_rus$swb2))

# compare 
plot(wvs6_rus$swb, wvs6_rus$swb2)

# CA.
# Check scales of justification of
# Sex before marriage (V206), Abortion (V204), intolerance towards Homosexuals (V40)
# Recode reverse scales if necessary, recode variables as scales with min 0 and max 1
# Create an index of moral tolerance (using these variables)


#### Descriptive statistics ####
# Ordered variables
attributes(wvs6_rus$V223) #how often do you use Internet
print_labels(wvs6_rus$V223) # values and labels
table(as_factor(wvs6_rus$V223), useNA = "ifany")
addmargins(table(droplevels(as_factor(wvs6_rus$V223)), useNA = "ifany"))

# addmargins() - adds total sum
# shares, prop.table()
prop.table(table(droplevels(as_factor(wvs6_rus$V223)), useNA = "ifany"))
addmargins(prop.table(table(droplevels(as_factor(wvs6_rus$V223)), useNA = "ifany")))

# percentages and rounding up
round(addmargins(prop.table(table(droplevels(as_factor(wvs6_rus$V223)), useNA = "ifany"))),3)*100

# Valid cases, cumulative 
round(addmargins(prop.table(table(droplevels(as_factor(wvs6_rus$V223))))),3)*100
cumsum(prop.table(table(wvs6_rus$V223))) # cumulative 

library("Hmisc")
Hmisc::describe(as_factor(wvs6_rus$V223)) # combines frequences and proportions

# mode - most frequent value
# 1st way - visually 
table(droplevels(as_factor(wvs6_rus$V223))) # Never
table(wvs6_rus$V223)
max(table(droplevels(as_factor(wvs6_rus$V223))))# value for "Never"


# Median
# Visually, where a value crosses 50% 
cumsum(prop.table(table(wvs6_rus$V223)))
# or 
median(wvs6_rus$V223, na.rm=T)
min(wvs6_rus$V223, na.rm=T)
max(wvs6_rus$V223, na.rm=T)
quantile(wvs6_rus$V223,c(0, 0.25, 0.5, 0.75, 1), na.rm=T) # possible with caution 
summary(wvs6_rus$V223)

# Mean
# this is a pseudo mean, it is dependent on the scale. 
# reversed order of categories in the quest. Recode it to match maximum value and maximum label
wvs6_rus$intnews<-6-wvs6_rus$V223
table(wvs6_rus$intnews, wvs6_rus$V223, useNA = "ifany")
summary(wvs6_rus$intnews) # it is better to recode scales for correct interpretation
summary(wvs6_rus$V223)
mean(wvs6_rus$V223, na.rm = T); mean(wvs6_rus$intnews, na.rm = T)

# CA. Calculate all measures for Sweden for V223
# Frequences and proportions. 
# mean, mode, median, quartiles
# Compare measure with Russian ones and draw conclusions. 


# Metric variables
table(wvs6_rus$V242) # age
summary(wvs6_rus$V242) # always check min and max
table(wvs6_rus$V242, useNA = "ifany") # frequencies
prop.table(table(wvs6_rus$V242, useNA = "ifany")) # shares
prop.table(table(wvs6_rus$V242, useNA = "ifany"))*100 # percentages
# Frequences not conveniet for such variables
hist(wvs6_rus$V242) # counts 
hist(wvs6_rus$V242, freq = F) # shares

# manual calculation
mean(wvs6_rus$V242)
median(wvs6_rus$V242)
quantile(wvs6_rus$V242, c(0,0.25, 0.5, 0.75, 1), na.rm=T) 
# mode
agemd<-table(wvs6_rus$V242); agemd
agemd[agemd==max(agemd)]

# Standard diviation
sd(wvs6_rus$V242) 
# measure of range 
# low standard deviation indicates that the values tend to be close to the mean value

# Variance
sd(wvs6_rus$V242, na.rm = T)
var(wvs6_rus$V242, na.rm = T) # squared sd
sd(wvs6_rus$V242, na.rm = T)^2 # diversity of the group

# CA. Calculate all measures for Sweden and compare them to Russian ones
# min, max, mode, median, quartiles, sd. 
# Interpret and draw conclusions


# Cross-tables 
# religiousity, V147
Hmisc::describe(as_factor(wvs6_rus$V147))
Hmisc::describe(as_factor(wvs6_rus$V240))
table(droplevels(as_factor(wvs6_rus$V147)), droplevels(as_factor(wvs6_rus$V240))) # grouping (indep) variables in columns
addmargins(table(droplevels(as_factor(wvs6_rus$V147)), droplevels(as_factor(wvs6_rus$V240))))
# Since groups have different number of respondents, correct comparison is not possible
# 100% by gender group make them equal
relgnd_tab<-table(droplevels(as_factor(wvs6_rus$V147)), droplevels(as_factor(wvs6_rus$V240)))
relgnd_tab
# Indexing tables 
dim(relgnd_tab)
relgnd_tab[,1]

prop.table(relgnd_tab,2)
addmargins(prop.table(relgnd_tab,2))
round(addmargins(prop.table(relgnd_tab,2)),3)*100
(round(addmargins(prop.table(relgnd_tab,2)),3)*100)[,-3] # delete third column

# without creating object
prop.table(table(droplevels(as_factor(wvs6_rus$V147)), droplevels(as_factor(wvs6_rus$V240))),2) # 2 - means 100% by column
addmargins(prop.table(table(droplevels(as_factor(wvs6_rus$V147)), droplevels(as_factor(wvs6_rus$V240))),2))
round(addmargins(prop.table(table(droplevels(as_factor(wvs6_rus$V147)), droplevels(as_factor(wvs6_rus$V240))),2)), 3)*100

# CA! 
# Calculate descriptive statistics for Sweden for V147 and V240
# Compare all measures with Russian and Sweden and draw conclusions
# Calculate cross-table cheking the dependence between gender and religiousity for Sweden
# Compare it with Russian results and draw cinclusions. 

# statistics by group
wvs6_swru<-wvs6_1[as_factor(wvs6_1$V2)=="Sweden" | as_factor(wvs6_1$V2)=="Russia", ]
dim(wvs6_swru)

# Metric variables
table(wvs6_swru$V242) # age
summary(wvs6_swru$V242) # always check min and max

tapply(wvs6_swru$V242, droplevels(as_factor(wvs6_swru$V2)), mean, na.rm=T)
#(statistics for what variable,  by groups of what variable,  what statistics,  what to do with NAs)
tapply(wvs6_swru$V242, droplevels(as_factor(wvs6_swru$V2)), sd, na.rm=T)
tapply(wvs6_swru$V242, droplevels(as_factor(wvs6_swru$V2)), median, na.rm=T)

# aggregate function
aggregate(wvs6_swru$V242, by=list(droplevels(as_factor(wvs6_swru$V2))), FUN = mean)
# (statistics for what variable,  by groups of what variableS,  what statistics)

# cross-table
# compare religiousity in Russia and Sweden 
prop.table(table(droplevels(as_factor(wvs6_swru$V147)), droplevels(as_factor(wvs6_swru$V2))),2)
round(addmargins(prop.table(table(droplevels(as_factor(wvs6_swru$V147)), droplevels(as_factor(wvs6_swru$V2))),2)),3)*100

# for countries, 3-way tables

addmargins(prop.table(xtabs(~as_factor(V147)+as_factor(V240)+as_factor(V2),
                            data=wvs6_swru, drop.unused.levels = T ),c(2,3)))
# c(2,3), 2 - column share, 3 - within each subtable. 
# or
relgndrusw_tab<-xtabs(~as_factor(V147)+as_factor(V240)+as_factor(V2),
                      data=wvs6_swru, drop.unused.levels = T )
relgndrusw_tab
dim(relgndrusw_tab)
prop.table(relgndrusw_tab, c(2,3))
addmargins(prop.table(relgndrusw_tab, c(2,3)))
addmargins(prop.table(relgndrusw_tab, c(2,3)))[,-3,1:2] # delete unnecessary data
# row, column, layer
# index for object addmargins(prop.table(relgndrusw_tab, c(2,3)))
# rounding up with shares 
(round(addmargins(prop.table(relgndrusw_tab, c(2,3))),3)*100)[,-3,1:2]

# One by one

xtabs(~as_factor(V147)+as_factor(V240)+as_factor(V2),
      data=wvs6_swru, drop.unused.levels = T )

prop.table(xtabs(~as_factor(V147)+as_factor(V240)+as_factor(V2),
                 data=wvs6_swru, drop.unused.levels = T ),c(2,3))

addmargins(prop.table(xtabs(~as_factor(V147)+as_factor(V240)+as_factor(V2),
                            data=wvs6_swru, drop.unused.levels = T ),c(2,3)))

#CA. Select Russian and Sweden
# recode age into 3-point scale (young, middle age, old), assign labels. 
# check the distribution for V163
# recode V163 into 3-point scale, assign labels
# Don`t forget to check recoding
# Show frequences (counts and percentages) for recoded variables for Russia and Sweden separately
# Draw conclusions about attitudes towards elderly in Russia and Sweden
# Check the dependence between age (recoded) and attitudes (recoded) for Russia and Sweden separately
# Draw conclusions. `
