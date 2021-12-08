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

jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
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

######## change point model #############

### Simulate data from a change point model
x<-seq(from=0,to=100,len=100)
yMean<-ifelse(x <50, 1.0 + 2.0*x,1.0+2.0*50+4.0*(x-50))
y<-sapply(yMean,rnorm,n=1,sd=4)
plot(x,y,typ='p')
lines(x,yMean)
N<-length(x)


modelString<-"
model {
    for (i in 1:N) {
        y[i] ~ dnorm(mu[i], tau)
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

# modelString<-"
# model {
# for (i in 1:N) {
# y[i] ~ dnorm(mu[i], tau)
# mu[i] <- alpha + beta[J[1]] * min(x[i],x.change) + 
#     beta[J[2]] * step(x[i] - x.change) 
# J[i] <- 1 + step(x[i] - x.change)
# }
# tau ~ dgamma(0.001, 0.001)
# alpha ~ dnorm(0.0, 1.0E-6)
# for (j in 1:2) {
# beta[j] ~ dnorm(0.0, 1.0E-6)
# }
# sigma <- 1 / sqrt(tau)
# x.change ~ dunif(40,60)
# }
# "

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

update(jags, 1000)

jags.samples(jags,
             c('a', 'b'),
             1000)

codaSamples<-coda.samples(jags, c('alpha','beta',
                            'x.change','sigma'), 100000, 100)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)
traceplot(codaSamples)



changepoint.dat <- list(Y = c(1.12, 1.12, 0.99, 1.03, 0.92, 0.90, 0.81,
                              0.83, 0.65, 0.67, 0.60, 0.59, 0.51, 0.44, 0.43, 0.43, 0.33, 0.30,
                              0.25, 0.24, 0.13, -0.01, -0.13, -0.14, -0.30, -0.33, -0.46, -0.43,
                              -0.65),
                        x = c(-1.39, -1.39, -1.08, -1.08, -0.94, -0.80, -0.63, -0.63, -0.25,
                              -0.25, -0.12, -0.12, 0.01, 0.11, 0.11, 0.11, 0.25, 0.25, 0.34, 0.34,
                              0.44, 0.59, 0.70, 0.70, 0.85, 0.85, 0.99, 0.99, 1.19),
                        N = 29)

changepoint.init1 <- list(alpha = 0.47, beta = c(-0.45, -1.0),
                          tau = 5, x.change = 0.5)
changepoint.init2 <- list(alpha = 0.47, beta = c(-0.45, -1.0),
                          tau = 5, x.change = -0.5)
changepoint.inits <- list(changepoint.init1, changepoint.init2)






