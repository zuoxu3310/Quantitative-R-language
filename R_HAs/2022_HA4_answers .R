## Home assignment 4 correct answers

#1.	Read the 6th wave of the WVS.

setwd("/Users/Natasha/Downloads") # \\ or /, otherwise  - error message 
library("haven")
wvs6<-read_sav("WV6_Data_sav_v20201117.sav",  user_na = FALSE) 


#2.	Select Russian and Sweden sample in a new data set. 
wvs6_rusw<-wvs6[as_factor(wvs6$V2)=="Russia" | as_factor(wvs6$V2)=="Sweden", ]
dim(wvs6_rusw)
dim(wvs6) # we can compare

# another way
wvs6_ru <- wvs6[as_factor(wvs6$V2)=="Russia", ]
wvs6_sw <- wvs6[as_factor(wvs6$V2)=="Sweden", ]
wvs6_rusw_1 <- rbind(wvs6_ru , wvs6_sw)
dim(wvs6_rusw_1)
dim(wvs6_rusw) # compare the results

# we could also use values instead of labels here
attributes(wvs6$V2)
wvs6_rusw_2<-wvs6[wvs6$V2==643 | wvs6$V2==752, ]
dim(wvs6_rusw_2)

#3.	Select variables trust in strangers (V105), education (V248), financial satisfaction (V59), family savings (V237) 
wvs6_rusw <- wvs6_rusw[c("V2","V105","V248","V59","V237")]
dim(wvs6_rusw)

#4.	Reverse scales matching the maximum level to the maximum numeric code and minimum levels to the minimum numeric code.
print_labels(wvs6_rusw$V105)
print_labels(wvs6_rusw$V59)
print_labels(wvs6_rusw$V248)
print_labels(wvs6_rusw$V237)
# We need to recode trust and we may recode family savings
#Trust
wvs6_rusw$trust_new<-5-wvs6_rusw$V105
table(wvs6_rusw$trust_new, wvs6_rusw$V105)
#Savings
wvs6_rusw$savings_new<-5-wvs6_rusw$V237
table(wvs6_rusw$savings_new, wvs6_rusw$V237)

#5.	Recode financial satisfaction into 5-point scale, assign labels, check recoding
library(car)
wvs6_rusw$fin_sat_rec<-recode(wvs6_rusw$V59, "1:2=1;3:4=2;5:6=3;7:8=4;9:10=5")
table(wvs6_rusw$V59,wvs6_rusw$fin_sat_rec, useNA="ifany")
wvs6_rusw$fin_sat_rec<-labelled(wvs6_rusw$fin_sat_rec, 
                        c("Dissatisfied" = 1, "Rather Dissatisfied" = 2, "Neither" = 3, "Rather Satisfied" = 4, 
                          "Satisfied" = 5))
table(as_factor(wvs6_rusw$fin_sat_rec))
table(droplevels(as_factor(wvs6_rusw$V59)), as_factor(wvs6_rusw$fin_sat_rec), useNA = "ifany")

#6.	Recode education into dummy variable 
# (1- a university education and higher, 0 ? no university education). Assign labels, check recoding.  
wvs6_rusw$educ_rec<-recode(wvs6_rusw$V248, "1:7=0;8:9=1")
table(wvs6_rusw$V248,wvs6_rusw$educ_rec, useNA="ifany")
wvs6_rusw$educ_rec<-labelled(wvs6_rusw$educ_rec, 
                          c("No university educ" = 0, "University educ" = 1))
table(as_factor(wvs6_rusw$educ_rec))
table(droplevels(as_factor(wvs6_rusw$V248)), as_factor(wvs6_rusw$educ_rec), useNA = "ifany")

#second way of recoding
wvs6_rusw$educ_rec2<-ifelse(wvs6_rusw$V248>=8, 1, 0)
table(wvs6_rusw$educ_rec2)
#labels can be assigned the way that is showed above

#7.	Recode trust in strangers into dummy (1-high trust, 0 ?low trust), assign labels, check recoding.
wvs6_rusw$trust_rec<-recode(wvs6_rusw$trust_new, "1:2=0;3:4=1")
table(wvs6_rusw$trust_rec,wvs6_rusw$trust_new,useNA="ifany")
wvs6_rusw$trust_rec<-labelled(wvs6_rusw$trust_rec, 
                               c("High trust" = 1, "Low trust" = 0))
table(as_factor(wvs6_rusw$trust_rec))
table(droplevels(as_factor(wvs6_rusw$V105)), as_factor(wvs6_rusw$trust_rec), useNA = "ifany") #to check labels I am using the original (not reversed) scale

# 8.	Recode family savings into 3-point scales combining together 
# "Spent some savings" and "Spent savings and borrowed money". Assign labels, check recoding.
wvs6_rusw$savings_rec<-recode(wvs6_rusw$V237, "1=1; 2=2; 3:4=3")
table(wvs6_rusw$savings_rec,wvs6_rusw$V237,useNA = "ifany" )
wvs6_rusw$savings_rec<-labelled(wvs6_rusw$savings_rec, 
                            c("Save money" = 1, "Just get by" = 2, 
                              "Spent savings and/or borrowed money" = 3))
table(droplevels(as_factor(wvs6_rusw$V237)), as_factor(wvs6_rusw$savings_rec), useNA = "ifany")


