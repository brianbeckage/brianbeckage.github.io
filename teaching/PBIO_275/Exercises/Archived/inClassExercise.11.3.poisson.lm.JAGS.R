install.packages("rjags")
install.packages("runjags")
install.packages("coda")
# see https://cran.r-project.org/web/packages/coda/coda.pdf

# install JAGS from http://mcmc-jags.sourceforge.net

R.Version()

setwd("~/Documents/Web/Teaching/DataAnalysis/Exercises")

### Linear Regression ####################################

N <- 1000
x <- 1:N
epsilon <- rnorm(N, 0, 1)
y <- x + epsilon

### Or

nReps<-10
x<-seq(from=0,to=100,by=1)
x<-rep(x,nReps)
b0<-1.0; b1<-0.2
ymean<-b0+b1*x
y<-rnorm(n=length(ymean),mean=ymean,sd=1.5)
N<-length(y)

# write.table(data.frame(X = x, Y = y, Epsilon = epsilon),
#             file = 'example2.data',
#             row.names = FALSE,
#             col.names = TRUE)


modelString<-"
model {
  for (i in 1:N){
    y[i] ~ dnorm(y.hat[i], tau)
    y.hat[i] <- a + b * x[i]
  }
  a ~ dnorm(0, .0001)
  b ~ dnorm(0, .0001)
  tau <- pow(sigma, -2)
  sigma ~ dunif(0, 100)
}
"
writeLines(modelString, con='TEMPmodel.txt')


library('rjags')

jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
                   inits<-list(
                       list('a'=1,'b'=2,'sigma'=5),
                       list('a'=1,'b'=.2,'sigma'=0.5),
                       list('a'=.1,'b'=2,'sigma'=7),
                       list('a'=2,'b'=5,'sigma'=1)),
                   n.chains = 4,
                   n.adapt = 100)

update(jags, 1000)

jags.samples(jags,
             c('a', 'b'),
             10000)

codaSamples<-coda.samples(jags, c('a','b'), 1000, 1)
codaSamples<-coda.samples(jags, c('a','b','sigma'), 10000, 1)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)
traceplot(codaSamples)



### Poisson regression #########################################

# A ecologist surveys 10 1m^2 quadrats for number of seedlings and records the 
# corresponding light level for each quadrat
# The ecologist believes that the number of seedlings follows Poisson distribution.  

# Question:  What is the relationship between nSeedlings and light level?

setwd("~/Documents/Web/Teaching/DataAnalysis/Exercises")
# Generating some simulated data
light<-seq(0.1, 0.8,len=100)
# ln(lambda)<-beta0+beta1*light
beta0<-1.0; beta1<-3.0
myLambda<-exp(beta0+beta1*light)
seedlings<-rpois(n=length(myLambda),lambda=myLambda)
N<-length(seedlings)
plot(light,seedlings)

### Your code to fit the model in JAGS goes here.

modelString<-"
model {
for (i in 1:N){
seedlings[i] ~ dpois(lamba.hat[i])
log(lamba.hat[i]) <- a + b * light[i]
}
a ~ dnorm(0, .0001)
b ~ dnorm(0, .0001)
}
"
writeLines(modelString, con='TEMPmodel.txt')


library('rjags')

jags <- jags.model('TEMPmodel.txt',
                   data = list('seedlings' = seedlings,
                               'light' = light,
                               'N' = N),
                   inits<-list(
                       list('a'=1,'b'=2),
                       list('a'=1,'b'=.2),
                       list('a'=.1,'b'=2),
                       list('a'=2,'b'=5)),
                   n.chains = 4,
                   n.adapt = 100)

update(jags, 1000)

jags.samples(jags,
             c('a', 'b'),
             10000)

codaSamples<-coda.samples(jags, c('a','b'), 10000, 10)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)
traceplot(codaSamples)
