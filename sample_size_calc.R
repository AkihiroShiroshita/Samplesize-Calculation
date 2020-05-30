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