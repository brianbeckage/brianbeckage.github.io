#### JAGS ##############################################

### JAGS: Linear Regression ####################################

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

codaSamples<-coda.samples(jags, c('a','b','sigma'), 10000, 1)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)
traceplot(codaSamples)



######## JAGS: Change Point Model: simulated data ############
### Simulate data from a change point model
x<-seq(from=0,to=100,len=100)
yMean<-ifelse(x <50, 1.0 + 2.0*x,1.0+2.0*50+4.0*(x-50))
y<-sapply(yMean,rnorm,n=1,sd=4)
plot(x,y,typ='p')
lines(x,yMean)
N<-length(x)


### JAGS: Change point model 1 #####
modelString<-"
model {
for (i in 1:N) {
y[i] ~ dnorm(mu[i], tau)
# alpha is y value at changepoint location
mu[i] <- alpha + beta[J[i]] * (x[i] - x.change) 
J[i] <- 1 + step(x[i] - x.change)
}
tau ~ dgamma(0.001, 0.001)
alpha ~ dnorm(0.0, 1.0E-6)
for (j in 1:2) {
beta[j] ~ dnorm(0.0, 1.0E-6)
}
sigma <- 1 / sqrt(tau)
x.change ~ dunif(40,60)
}
"


writeLines(modelString, con='TEMPmodel.txt')

library('rjags')

jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
                   inits<-list(
                       list('alpha'=1,'beta'=c(2,4),'tau'=0.5,'x.change'=45),
                       list('alpha'=0.1,'beta'=c(3,5),'tau'=0.2,'x.change'=54),
                       list('alpha'=10,'beta'=c(1,2),'tau'=0.7,'x.change'=41),
                       list('alpha'=7,'beta'=c(5,9),'tau'=0.15,'x.change'=58)),
                   n.chains = 4,
                   n.adapt = 100)


codaSamples<-coda.samples(jags, c('alpha','beta',
                                  'x.change','sigma'), 100000, 100)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(window(codaSamples,start=2000))
traceplot(codaSamples)


#### JAGS: Change point model 2 #########
#### Marina's code: MODIFIED 
modelString<-
    "model{
for (i in 1:N){
prp[i] ~ dnorm(prpMean[i], tau)
prpMean[i]<- ifelse(year[i]<changePoint, beta[1] + beta[2]*year[i], beta[1] + beta[2]*changePoint 
+ beta[3]*(year[i]-changePoint))
}
changePoint ~ dnorm(1950, .0001)

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
                   data = list('year' = year,
                               'prp' = prp,
                               'N' = N),
                   inits<-list(
                       list('beta'=c(1,2,4),'tau'=0.05,'changePoint'=1945),
                       list('beta'=c(0.1,3,5),'tau'=0.02,'changePoint'=1954),
                       list('beta'=c(10,1,2),'tau'=0.07,'changePoint'=1941),
                       list('beta'=c(7,5,9),'tau'=0.015,'changePoint'=1958)),
                   n.chains = 4,
                   n.adapt = 100)

update(jags,1000)
codaSamples<-coda.samples(jags, c('beta',
                                  'changePoint','sigma'), 100000, 100)


library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(window(codaSamples,start=2000))
traceplot(codaSamples)
gelman.diag(codaSamples)
gelman.plot(codaSamples)

# Potential scale reduction factors:
#     
#     Point est. Upper C.I.
# beta[1]           1.19       1.54
# beta[2]           1.19       1.54
# beta[3]           1.00       1.01
# changePoint       1.02       1.06
# sigma             1.00       1.00
# 
# Multivariate psrf
# 
# 1.11

# Iterations = 2000:101100
# Thinning interval = 100 
# Number of chains = 4 
# Sample size per chain = 992 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#     
#     Mean       SD  Naive SE Time-series SE
# beta[1]      -79.192 46.99280 0.7460114      1.303e+01
# beta[2]        2.041  0.02434 0.0003865      6.734e-03
# beta[3]        4.039  0.04886 0.0007757      7.753e-04
# changePoint 1960.418  0.68019 0.0107980      5.499e-02
# sigma        170.033 50.27541 0.7981229      7.983e-01
# 
# 2. Quantiles for each variable:
#     
#     2.5%      25%      50%      75%    97.5%
# beta[1]     -170.430 -112.757  -78.299  -48.526   25.665
# beta[2]        1.987    2.026    2.041    2.059    2.089
# beta[3]        3.941    4.006    4.039    4.072    4.132
# changePoint 1958.976 1959.994 1960.443 1960.883 1961.678
# sigma         95.185  133.592  163.510  196.301  292.849



### JAGS: Poisson regression #########################################

# A ecologist surveys 10 1m^2 quadrats for number of seedlings and records the 
# corresponding light level for each quadrat
# The ecologist believes that the number of seedlings follows Poisson distribution.  

# Question:  What is the relationship between nSeedlings and light level?

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


#### STAN ####################################################

### Stan: Poisson regression #########################################

# A ecologist surveys 10 1m^2 quadrats for number of seedlings and records the 
# corresponding light level for each quadrat
# The ecologist believes that the number of seedlings follows Poisson distribution.  

# Question:  What is the relationship between nSeedlings and light level?


# Generating some simulated data
light<-seq(0.1, 0.8,len=100)
# ln(lambda)<-beta0+beta1*light
beta0<-1.0; beta1<-3.0
myLambda<-exp(beta0+beta1*light)
seedlings<-rpois(n=length(myLambda),lambda=myLambda)
N<-length(seedlings)
plot(light,seedlings)

### Stan: Version 1 of poisson regression ####
modelString<-"
data {
int<lower=0> N; // number of quadrats
int<lower=0> seedlings[N]; // seedlings in quadrats
real light[N]; // light levels
}

parameters {
real beta0; 
real beta1;
}

transformed parameters {
real lambdalog[N];
for (j in 1:N)
lambdalog[j] <- beta0 + beta1 * light[j];
}

model {
beta0 ~ normal(0, 100);
beta1 ~ normal(0, 100);
seedlings ~ poisson_log(lambdalog);
}
"

writeLines(modelString, con='TEMPmodel.txt')

seedling_dat <- list(N = N, 
                     seedlings= seedlings,
                     light = light)

fit <- stan(file = 'TEMPmodel.txt', data = seedling_dat, 
            iter = 1000, chains = 4)
fit

# Inference for Stan model: TEMPmodel.
# 4 chains, each with iter=1000; warmup=500; thin=1; 
# post-warmup draws per chain=500, total post-warmup draws=2000.
# 
# mean              se_mean   sd    2.5%     25%     50%     75%   97.5% n_eff Rhat
# beta0             0.91    0.01 0.10    0.72    0.84    0.92    0.98    1.10   347 1.02
# beta1             3.22    0.01 0.16    2.92    3.11    3.22    3.34    3.54   341 1.02

library(shinystan)

fit<-launch_shinystan(fit)



### Stan: Version 2 of poisson regression ####
modelString<-"
data {
int<lower=0> N; // number of quadrats
int seedlings[N]; // seedlings in quadrats
vector[N] light; // estimated treatment effects
}

parameters {
real beta0; 
real beta1;
}

transformed parameters {
vector[N] lambda;
lambda = exp(beta0 + beta1 * light);
}

model {
beta0 ~ normal(0, 100);
beta1 ~ normal(0, 100);
seedlings ~ poisson(lambda);
}
"

writeLines(modelString, con='TEMPmodel.stan')

seedling_dat <- list(N = N, 
                     seedlings= seedlings,
                     light = light)

fit <- stan(file = 'TEMPmodel.stan', data = seedling_dat) 
, 
            iter = 1000, chains = 4)

fit

# Inference for Stan model: TEMPmodel.
# 4 chains, each with iter=1000; warmup=500; thin=1; 
# post-warmup draws per chain=500, total post-warmup draws=2000.
# 
# mean se_mean   sd    2.5%     25%     50%     75%   97.5% n_eff Rhat
# beta0          0.91    0.00 0.09    0.73    0.84    0.90    0.96    1.10   360 1.01
# beta1          3.23    0.01 0.16    2.89    3.14    3.24    3.34    3.52   349 1.01

library(shinystan)

launch_shinystan(fit)



