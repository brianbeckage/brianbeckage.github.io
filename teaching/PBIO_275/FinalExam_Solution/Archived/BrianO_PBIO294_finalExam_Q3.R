# PBIO 294 Final exam ques3:
# Brian O'Malley 
# Due: Dec 17,2015

# 3). (25 points) The following data may contain 0 to 2 changepoints. If there is >0 changepoints,
# then assume the associated variances in each segment are different. Report the number of
# changepoints in the data, the evidence to support your inference, the beta's for all segments,
# the sd's for each segment, and the location of any changepoints. Assume normal errors.
# Report 0.025, 0.50, and 0.975 quantiles for all these parameters.
# Provide some evidence that your MCMC chains have converged.
# Include your R/JAGS/STAN code with your response as well as plot(s) of the data.

setwd("C:/Users/Administrator/Desktop/Becakge Final Projects n Exam")
dir()
cp = read.csv("cp.csv", header=T)
plot(cp$y~cp$x)
N = length(cp$y)

modelString<-
"model{
for (i in 1:N){
prp[i] ~ dnorm(prpMean[i], tau)
prpMean[i]<- ifelse(x[i]<changePoint, beta[1] + beta[2]*x[i], beta[1] + beta[2]*changePoint 
+ beta[3]*(x[i]-changePoint))
}

changePoint ~ dnorm(20, .0001)

for(i in 1:3){
beta[i] ~ dnorm(0, .0001)
}

tau ~ dgamma(0.001,0.001)
sigma<-pow(tau, -0.5)

}
"
writeLines(modelString, con='TEMPmodel.txt')

library('rjags')

jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = cp$x,
                               'prp' = cp$y,
                               'N' = N),
                   inits<-list(
                     list('beta'=c(1,2,4),'tau'=0.05,'changePoint'=15),
                     list('beta'=c(0.1,3,5),'tau'=0.02,'changePoint'=20),
                     list('beta'=c(10,1,2),'tau'=0.07,'changePoint'=25),
                     list('beta'=c(7,5,9),'tau'=0.015,'changePoint'=30)),
                   n.chains = 4,
                   n.adapt = 1000)

update(jags,1000)
codaSamples<-coda.samples(jags, c('beta',
                                  'changePoint','sigma'), 100000, 1000)


library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(window(codaSamples,start=2000))
# Iterations = 3000:102000
# Thinning interval = 1000 
# Number of chains = 4 
# Sample size per chain = 100 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean       SD  Naive SE Time-series SE
# beta[1]      1.210 0.612161 0.0306080      0.0298898
# beta[2]      2.024 0.041858 0.0020929      0.0020737
# beta[3]      3.001 0.009082 0.0004541      0.0004555
# changePoint 25.416 0.753582 0.0376791      0.0377133
# sigma        5.174 0.120161 0.0060081      0.0060054
# 
# 2. Quantiles for each variable:
#   
#                2.5%     25%    50%    75%  97.5%
# beta[1]      0.02467  0.7528  1.230  1.648  2.426
# beta[2]      1.94122  1.9926  2.028  2.051  2.104
# beta[3]      2.98436  2.9943  3.001  3.007  3.017
# changePoint 23.94655 24.9230 25.416 25.891 27.007
# sigma        4.95622  5.0863  5.174  5.259  5.416

traceplot(codaSamples)
# so my answer is there is a changepoint at approximately x=25.