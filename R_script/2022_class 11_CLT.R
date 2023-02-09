##### Anna Almakaeva and Natalia Mikhailova#####
### Normal distribution and confidence intervals#####


help(Normal)
help(Distributions)
options(scipen = 999, digits=3) 

# Finding probabilities for the normal distribution

pnorm(0.5) # probabily to the left, z scores in ()
pnorm(1.0) # probabily to the left, z scores in ()
pnorm(1.96) # probabily to the left, z scores in ()
pnorm(2) # probabily to the left, z scores in ()
pnorm(3)
pnorm(-1.96) 


1-pnorm(0.5) # probability to the right
1-pnorm(1.96) # probability to the right
# another way to calculate probability to the right
pnorm(1.96, lower.tail=F)

qnorm(0.975) # returns Z scores, using area under the curve (probability)
qnorm(0.9772) 
qnorm(0.8413)


# Central Limit Theorem (CLT)
# if there are many independent random variables summed up, their distribution is close to normal distribution

set.seed(1020)
# Selection from non-normal population nnorm  (nnorm for creating non-normally distributed data)
data1<- data.frame(t(replicate(1000, sample(nnorm$nn, size=900, replace=FALSE)))) 
dim(data1) # rows - samples, colomns - sample size

data1$means<-rowMeans(data1)# creating means for each sample

dim(data1) 
# plotting the distribution of all means 

ggplot(data1)+geom_density(aes(means), colour="red", fill="red", alpha=0.25)+
  geom_vline(xintercept = mean(nnorm$nn), linetype="dashed")


# Finding confidence intervals when sd of population is known

#confidence level 95%

round(qnorm(0.975),2) # returns Z scores, using % under the curve

# Example 1
n<-1000 # sample size
mean<-101 # sample mean
sdp<-5 # population sd
z<-qnorm(0.95+0.025)# Z score
ci.l<-mean-z*sdp/sqrt(n); ci.u<-mean+z*sdp/sqrt(n)
ci1<- round(c(ci.l, ci.u),2); ci1

# Example 2
n<-100
ci.l<-mean-z*sdp/sqrt(n); ci.u<-mean+z*sdp/sqrt(n)
ci2<- round(c(ci.l, ci.u),2); ci2

# Example 3

n<-200
ci.l<-mean-z*sdp/sqrt(n); ci.u<-mean+z*sdp/sqrt(n)
ci3<- round(c(ci.l, ci.u),2); ci3

# Example 4 - another confidence level 
n<-1000
z<-qnorm(0.90+0.05)# Z score
ci.l<-mean-z*sdp/sqrt(n); ci.u<-mean+z*sdp/sqrt(n)
ci4<- round(c(ci.l, ci.u),2); ci4

# Example 5
n<-1000
z<-qnorm(0.99+0.005)# Z score
ci.l<-mean-z*sdp/sqrt(n); ci.u<-mean+z*sdp/sqrt(n)
ci5<- round(c(ci.l, ci.u),2); ci5

# Example 6 different sd

n<-1000 # sample size
mean<-101 # sample mean
sdp<-150 # population sd

ci.l<-mean-z*sdp/sqrt(n); ci.u<-mean+z*sdp/sqrt(n)
ci6<- round(c(ci.l, ci.u),2); ci6

# Example 7, small sample size

n<-10
ci.l<-mean-z*sdp/sqrt(n); ci.u<-mean+z*sdp/sqrt(n)
ci7<- round(c(ci.l, ci.u),2); ci7

# Example 8, 95 Cl
z<-qnorm(0.95+0.025)# Z score

ci.l<-mean-z*sdp/sqrt(n); ci.u<-mean+z*sdp/sqrt(n)
ci8<- round(c(ci.l, ci.u),2); ci8

# All results

sd<-c(rep(5,5), rep(150,3))
conf.l<-c(rep(95,3), 90, rep(99,3), 95)
n1<-c(1000,100,200,1000,1000,1000,10,10)
ci.table<-data.frame(rbind(ci1,ci2,ci3,ci4,ci5,ci6,ci7,ci8)); colnames(ci.table)<-c("lower", "upper")
ci.results<-data.frame(sd=sd,conflev=conf.l, sample=n1)
ci.results<-data.frame(ci.results, ci.table) 
row.names(ci.results)<-c(1:8)
ci.results

# If you do not know population parameter use sample parameters and N-1 correction

# ! CA
# calculate confidence intervals for mean = 5500
# In addition calculate the total width of confidence intervals (ignore negative sign). 
# sample sd=3000, aplha=0.05, sample=100
# sample sd=3000, aplha=0.05, sample=500
# sample sd=3000, aplha=0.05, sample=1000
# sample sd=3000, aplha=0.05, sample=10000
# sample sd=3000, aplha=0.05, sample=20000
# sample sd=3000, aplha=0.05, sample=50000
# sample sd=3000, aplha=0.05, sample=100000
# sample sd=3000, aplha=0.05, sample=500000
# Create a data.frame which includes all results, including the total width of the confidence intervals. 
# You may create objects and use vectors 






