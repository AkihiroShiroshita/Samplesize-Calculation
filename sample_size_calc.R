library(pwr)
#One sample mean
###TO CALCULATE N
power.t.test(delta=0.5, sd=1.4, power=0.8, type="one.sample", alternative="one.sided")

t.res<-power.t.test(delta=0.5, sd=1.4, power=0.8, type="one.sample", alternative="one.sided")
names(t.res)

###TO CALCULATE DELTA
power.t.test(n=50, sd=1.4, power=0.8, type="one.sample", alternative="one.sided")

###TO CALCULATE POWER
power.t.test(n=50, delta=0.5, sd=1.4, type="one.sample", alternative="one.sided")

#Two sample size
###TO CALCULATE N
d<-2/2.8
pwr.t.test(d=d, n=30,sig.level=0.05,type="two.sample",alternative="two.sided")
#n is the total sample size. assume euqual allocation between the two groups.
#d is the standardized effect size. i.e. (mu1-mu0)/SD

#Simple linear regression
#To use R pwr package to mimick the PSS calculation for simple linear regression presented in the lecture using PASS

#In PASS example, the parameters given were standard deviation of y (=1.5), standard deviation of x (=6.3), the coefficient value under 
#	alternative hypothesis (B=0.1). To use R pwr package pwr.f2.test function, these parameters cannot be directly specified, rather you need
#	to directly specify the parameter f2=R_square/(1-R_squre). Remember the R_square statistic in linear regression model, it is a measure of how good the 
#	linear model fits the data, and it is 1-(Residual sum square/total sum square). 
#	Using the known information on SD of x and y, and B, we can calculate R_square using the following code:

sd.y<-1.5
sd.x<-6.3
b<-0.1
sd.e<-sqrt(sd.y^2-0.1^2*sd.x^2) #This is the standard deviation of residuals. 
r.sq<-1-(sd.e^2/sd.y^2)	#This is the R_square.
r.sq #=0.1764

pwr.f2.test(u=1, f2=r.sq/(1-r.sq), power=0.8) #u is the DF of numerator, since this is simple linear regression testing for one variable, u=1. 
#v is left unspecified but power= is specified, so this is requesting output for n. 


#The result looks:Multiple regression power calculation 
u = 1
v = 36.66389				
f2 = 0.2141816
sig.level = 0.05
power = 0.8
#Note, the result does not directly tell you N. Rather, since v (the DF of the denominator) was unspecified, and v=N-2, then we infer that the sample size should be
#	N=37+2=39.
# So you see, this R function is not very flexible and intuitive to do PSS for linear model. Normally, you may try different levels of f2, but f2 is a somewhat
#	abstract parameter, not as easily conceplized as B or SD(x) or SD(y). Reference: http://www.statmethods.net/stats/power.html 
#Another R package see http://cran.r-project.org/web/packages/powerMediation/powerMediation.pdf , has functions for SLR, which is easier to use by specifying B
#	SDx, and SDy directly.

#One sample proportion
#sample size
#H0: p = pnull
#Ha: p=palt > pnull
#pow = power
#alpha=type 1 error rate
#sss = 1 or 2 sided test
ssone <- function(pnull, palt, pow , alpha, sss){
  qnull <- 1-pnull
  qalt <- 1-palt
  sqv0 <- sqrt((pnull*qnull))
  sqv1 <- sqrt((palt*qalt))
  zalpha <- qnorm(1-alpha/sss)
  zbeta <- qnorm(pow)
  numerator <- zalpha*sqv0 + zbeta*sqv1
  denom <- palt - pnull
  (numerator/denom)**2
}
ssone(.022,.05,.8,.05,1)
#OR pwr package
#arcsin transformation
#ES.h(h of null hypothesis, h of alternative hypothesis)
h <- ES.h(0.022,0.05)
pwr.p.test(h=h, n=NULL, sig.level=0.05, power= 0.8, alternative = "less")

#two sample proportion
#power
sptwo <- function(p1, p2, n1, n2, alpha){
  pbar <- (p1+p2)/2
  delta <- abs(p2-p1)
  zalpha <- qnorm(1-alpha/2)
  intn <- (1/n1) + (1/n2)
  int1 <- (delta/sqrt(pbar*(1-pbar)*intn))
  pnorm(int1 - zalpha)
}
sptwo(0.0233, 0.0430, 13073, 1186, 0.05)

#OR pwr 
#arcsin transformation
h <- ES.h(0.233, 0.43)
pwr.2p2n.test(h, 13073, 1186)

#Survival analysis
#sample size calculation
library(Hmisc) #Need to install Hmisc first
?cpower #Note: this function is using exponential dist assumption to estimate the prob of having the event
#by study period. 

tref<-4		#Specify a time at which you have prior knowledge about the survival probablity
n<-1000		#sample size 
mc<-0.35	#Probability of having a event by tref, i.e. 1-Survial prob at tref. Obtained from prior
#data or literature review.
hr<-0.75	#Hazard ratio. Note: not needed using cpower. Here try to reproduce the breast cancer
#example

r<-(1-((1-(1-mc)^hr)/mc))*100	#This is 1- the ratio of event probability in intervention group compared to 
#control group at tref. Can be specified directly without useing 
#hazard ratio. Note: this should be the percentage, i.e. 20% 
#reduction in the event prob should enter 20 rather than 0.2 here.

accrual<-2		#Length of accrual period
tmin<-5			#Minimum length of follow-up 

pwr.res<-cpower(tref=tref, n=n, mc=mc, r=r, accrual=accrual, tmin=tmin)

Nsim<-seq(600,1000, 20)
pwr.res<-c()
for (i in 1:length(Nsim) ){
  pwr.tmp<-cpower(tref=tref, n=Nsim[i], mc=mc, r=r, accrual=accrual, tmin=tmin,pr=FALSE)
  pwr.res[i]<-pwr.tmp["Power"]
  
}

pwr.res.all<-cbind(Nsim, pwr.res)
