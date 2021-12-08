# PBIO 294 Final exam ques4:
# Brian O'Malley 
# Due: Dec 17,2015

# An ecologist expects seed predation to be influenced by the presence of openings
# (gaps) in the forest overstory, seed mass, and species of the seed. The effect of seed mass on
# predation is expected be quadratic while the effect of a gap is additive:
# 
# Seeds predated~Binomial(p)
# Logit(p)<-b0+b1*gap
# b0~gamma(mu,sdGamma)
# mu<-b2+b3*seed mass-b4*seed mass^2
# with appropriate priors.

setwd("C:/Users/Administrator/Desktop/Becakge Final Projects n Exam")
dir()
seeds = read.csv("seeds.csv", header=T)
N = length(seeds$seedsLost)
Gap=seeds$gap
seedMass = seeds$mass
seedsLost = seeds$seedsLost

### Your code to fit the model in JAGS goes here.
library(rjags)
library(coda)
modelString<-"
model {

for (i in 1:N){
seedsLost[i] ~ dbin(p.hat[i], N)
logit(p.hat[i]) <- B0[i]+B1*Gap[i]
B0[i] <- dgamma(mu[i],sdGamma)
mu[i] <- B2+B3*seedMass[i]-B4*seedMass[i]^2

}
B1 ~ dnorm(1, .0001)
B2 ~ dnorm(1, .0001)
B3 ~ dnorm(1, .0001)
B4 ~ dnorm(1, .0001)
sdGamma ~ dnorm(0,.0001)
}
"

writeLines(modelString, con='TEMPmodel.txt')


jags <- jags.model('TEMPmodel.txt',
                   data = list('Gap' = Gap,
                               'seedMass'  = seedMass,
                               'seedsLost' = seedsLost,
                               'N' = N),
                   inits<-list(
                     list('B1'=0,'B2'=1,  'B3'=2,'B4'=3 ,'sdGamma'=0.05),
                     list('B1'=2,'B2'=0.5,'B3'=1,'B4'=1 ,'sdGamma'=0.10),
                     list('B1'=1,'B2'=2,  'B3'=3,'B4'=2 ,'sdGamma'=0.20)),
                   n.chains = 3,
                   n.adapt = 100)


update(jags, 1000)

jags.samples(jags,
             c('B0'),
             10000)

codaSamples<-coda.samples(jags, c('B0'), 1000, 1)
codaSamples<-coda.samples(jags, c('B0'), 10000, 1)


plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)