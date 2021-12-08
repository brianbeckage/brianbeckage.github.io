# PBIO 294 Final exam ques2:
# Brian O'Malley 
# Due: Dec 17,2015

# tree height~Normal(mu,sd)
# mu<-b0+b1*light metric
# b0~gamma(muG,sdG)
# muG<-b2+b3*soil nutrient level
# with appropriate priors.

setwd("C:/Users/Administrator/Desktop/Becakge Final Projects n Exam")
dir()
TDat = read.csv("treeStems.csv", header=T)
N= length(TDat$soil)
lightMetric = TDat$light
soilNute = TDat$soil
treeHT = TDat$height

modelString<-
"model{
for (i in 1:N){
treeHT[i] ~ dnorm(mu[i], sd)
mu[i] <- b0[i]+b1*lightMetric[i]
b0[i]~dgamma(muG[i],sdG)
muG[i]<-b2+b3*soilNute[i]
}


b1~ dnorm(0, .0001)
b2~ dnorm(0, .0001)
b3~ dnorm(0, .0001)
sd ~ dnorm(0, .0001)
sdG ~ dnorm(0, .0001)
}
"
writeLines(modelString, con='TEMPmodel.txt')

library('rjags')

jags <- jags.model('TEMPmodel.txt',
                   data = list('treeHT' = treeHT,
                               'lightMetric' = lightMetric,
                                'soilNute' = soilNute,
                                'N' = N),
                   inits<-list(
                     list('b1'=2,'b2'=2, 'b3'=4,'sd'=0.05,'sdG'=0.03),
                     list('b1'=1,'b2'=1, 'b3'=1,'sd'=0.01,'sdG'=0.02),
                     list('b1'=0.5,'b2'=0, 'b3'=2,'sd'=0.07,'sdG'=0.05),
                     list('b1'=3,'b2'=4, 'b3'=6,'sd'=0.04,'sdG'=0.01)),
                   n.chains = 4,
                   n.adapt = 1000)

library(coda)

update(jags,1000)
codaSamples<-coda.samples(jags, c('b1', 'b2', 'b3',
                                  'sd','sdG'), 10000, 100)


plot(codaSamples, trace = FALSE, density = TRUE)
summary(window(codaSamples,start=2000))
# Iterations = 2100:12000
# Thinning interval = 100 
# Number of chains = 4 
# Sample size per chain = 100 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean      SD Naive SE Time-series SE
# b1  5.02347 0.03981 0.001991       0.003101
# b2  2.07810 2.58969 0.129484       0.199269
# b3  1.63549 1.46660 0.073330       0.211969
# sd  0.04447 0.06441 0.003221       0.008689
# sdG 3.26175 2.89449 0.144725       0.425246
# 
# 2. Quantiles for each variable:
#   
#          2.5%      25%      50%     75%  97.5%
# b1   4.9876270 5.000983 5.007477 5.02692 5.1352
# b2  -0.5635407 0.110877 1.042957 3.61513 8.1583
# b3   0.0313469 0.253183 1.179528 2.75762 4.8450
# sd   0.0002965 0.001849 0.008799 0.04055 0.2034
# sdG  0.0995737 0.541447 2.403876 5.50822 9.6271
traceplot(codaSamples)
