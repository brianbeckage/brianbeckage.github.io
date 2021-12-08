install.packages('rstan')
library(rstan)


#### Random effects regression #####
modelString<-"
data {
int<lower=0> J; // number of schools 
real y[J]; // estimated treatment effects
real<lower=0> sigma[J]; // s.e. of effect estimates 
}
parameters {
real mu; 
real<lower=0> tau;
real eta[J];
}
transformed parameters {
real theta[J];
for (j in 1:J)
theta[j] = mu + tau * eta[j];
}
model {
eta ~ normal(0, 1);
y ~ normal(theta, sigma);
}
"

writeLines(modelString, con='8schools.stan')

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = '8schools.stan', data = schools_dat, 
            iter = 1000, chains = 4)

fit



### Poisson regression #########################################

# A ecologist surveys 10 1m^2 quadrats for number of seedlings and records the 
# corresponding light level for each quadrat
# The ecologist believes that the number of seedlings follows Poisson distribution.  

# Question:  What is the relationship between nSeedlings and light level?


# Generating some simulated data
light<-seq(0.1, 0.8,len=100)
# ln(lambda)<-beta0+beta1*light
beta0<-1.0; beta1<-3.0
myLambda<-exp(beta0+beta1*light)
nSeedlings<-rpois(n=length(myLambda),lambda=myLambda)
plot(light,nSeedlings)


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

### Stan: Version 1 of poisson regression; works ####
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



### Stan: Version 2 of poisson regression; works ####
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
lambda <- exp(beta0 + beta1 * light);
}

model {
beta0 ~ normal(0, 100);
beta1 ~ normal(0, 100);
seedlings ~ poisson(lambda);
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
# mean se_mean   sd    2.5%     25%     50%     75%   97.5% n_eff Rhat
# beta0          0.91    0.00 0.09    0.73    0.84    0.90    0.96    1.10   360 1.01
# beta1          3.23    0.01 0.16    2.89    3.14    3.24    3.34    3.52   349 1.01

library(shinystan)

launch_shinystan(fit)





### Stan: example from website ##############################################
### Example from https://groups.google.com/forum/#!topic/stan-users/TjI68DM3KYE
modelString<-"
data {
  int<lower=1> K;
int<lower=1> N;
matrix[N,K] x;
int y[N];
}
parameters {

vector[K] beta;
}
transformed parameters {
vector[N] theta;

theta <- exp(x * beta);
}
model {

y ~ poisson(theta);
beta ~ normal(0, 1000);
}
generated quantities{
vector[N] T;
int yrep[N];

for (i in 1:N) {

yrep[i] <- poisson_rng(theta[i]);

T[i] <- step(yrep[i] - y[i]) - 0.5 * if_else(yrep[i] == y[i], 1, 0);
}
}
"

writeLines(modelString, con='TEMPmodel.txt')

set.seed(875)
DATA <- list(K = 5, N = 10, x = matrix(rnorm(50, 5, 1), ncol = 5), y = sample.int(n = 25, size = 10))
fit <- stan(file= "TEMPmodel.txt", data = DATA, iter = 1000, chains = 4);

















### Logistic regression ########################################

### simulate data
# 1 / (1 + exp(-z)) ->inverse logit
N <- 1000
x <- 1:N
z <- 0.01 * x - 5
y <- sapply(1 / (1 + exp(-z)), function(p) {rbinom(1, 1, p)})

modelString<-"
model {
for (i in 1:N){
y[i] ~ dbern(p[i])
p[i] <- 1 / (1 + exp(-z[i]))
z[i] <- a + b * x[i]
}
a ~ dnorm(0, .0001)
b ~ dnorm(0, .0001)
}
"
writeLines(modelString, con='TEMPmodel.txt')

library('rjags')

jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

update(jags, 1000)

jags.samples(jags,
             c('a', 'b'),
             1000)

codaSamples<-coda.samples(jags, c('a','b'), 5000, 1)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)
traceplot(codaSamples)








modelString<-"
data {
int<lower=0> J; // number of schools 
real y[J]; // estimated treatment effects
real<lower=0> sigma[J]; // s.e. of effect estimates 
}
parameters {
real mu; 
real<lower=0> tau;
real eta[J];
}
transformed parameters {
real theta[J];
for (j in 1:J)
theta[j] <- mu + tau * eta[j];
}
model {
eta ~ normal(0, 1);
y ~ normal(theta, sigma);
}
"

writeLines(modelString, con='8schools.stan')

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = '8schools.stan', data = schools_dat, 
            iter = 1000, chains = 4)

fit

