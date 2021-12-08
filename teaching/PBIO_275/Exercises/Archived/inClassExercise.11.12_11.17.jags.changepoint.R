install.packages("rjags")
install.packages("runjags")
install.packages("coda")
# see https://cran.r-project.org/web/packages/coda/coda.pdf

# install JAGS from http://mcmc-jags.sourceforge.net

setwd("~/Documents/Web/Teaching/DataAnalysis/Exercises")

######## change point model: simulated data ############

### Simulate data from a change point model
x<-seq(from=0,to=100,len=100)
yMean<-ifelse(x <50, 1.0 + 2.0*x,1.0+2.0*50+4.0*(x-50))
y<-sapply(yMean,rnorm,n=1,sd=4)
plot(x,y,typ='p')
lines(x,yMean)
N<-length(x)


### This model works #####
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
codaSamples<-coda.samples(jags, c('alpha','beta',
                                  'x.change','sigma'), 100000, 100)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(window(codaSamples,start=2000))
traceplot(codaSamples)



year<-seq(from=1900,to=2000,len=100)
prpMean<-ifelse(year <1960, 1.0 + 2.0*year, 1.0+2.0*1960+4.0*(year-1960) )
prp<-sapply(prpMean,rnorm,n=1,sd=4)
plot(year,prp,typ='p')
lines(year,prpMean)
N<-length(year)



#### Marina's code: MODIFIED ######
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
x<-seq(from=0,to=10,len=100)
y<-dgamma(x,0.001,0.001)
plot(x,y,typ='l')

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



######## change point model: using actual climate data ############
require(gdata)
list.files()
myDat = read.xls("NE annual data series.xlsx",sheet = 1, header=TRUE)
str(myDat)
head(myDat)
names(myDat)<-c("year","Annual.total", "Annual.extreme","Livneh","Annual.total.1",
                "Annual.extreme.1","NARR","Annual.total.2","Annual.extreme.2")
# Original: names(myDat)<-c("GHCN.Daily","Annual.total", "Annual.extreme","Livneh","Annual.total.1",
#                 "Annual.extreme.1","NARR","Annual.total.2","Annual.extreme.2")


plot(myDat$year,myDat$Annual.total)
plot(myDat$year,myDat$Annual.total.1)
plot(myDat$year,myDat$Annual.extreme)
range(myDat$year)

year<-x<-myDat$year
# y<-myDat$Annual.total
prp<-y<-myDat$Annual.extreme
N<-length(year)
plot(x,y)

### This model works #####
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
x.change ~ dunif(1901,2014)
}
"

writeLines(modelString, con='TEMPmodel.txt')

library('rjags')

jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
                   inits<-list(
                       list('alpha'=1000,'beta'=c(2,4),'tau'=0.5,'x.change'=1945),
                       list('alpha'=100,'beta'=c(3,5),'tau'=0.2,'x.change'=1954),
                       list('alpha'=10,'beta'=c(1,2),'tau'=0.7,'x.change'=1970),
                       list('alpha'=200,'beta'=c(5,9),'tau'=0.15,'x.change'=1990)),
                   n.chains = 4,
                   n.adapt = 100)


codaSamples<-coda.samples(jags, c('alpha','beta',
                                  'x.change','sigma'), 1000000, 500)

summary(codaSamples)

# Current iterations

# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#     
#     Mean      SD Naive SE Time-series SE
# alpha      74.5355  10.844  0.12124         0.1555
# beta[1]   -47.5579 195.400  2.18464         2.3889
# beta[2]     0.2854  66.681  0.74551         0.6994
# sigma      29.6797   2.043  0.02284         0.0233
# x.change 1958.3633  37.846  0.42313         0.6291
# 
# 2. Quantiles for each variable:
#     
#     2.5%       25%        50%       75%     97.5%
# alpha      55.622   66.4208   74.44240 8.184e+01   97.5260
# beta[1]  -578.948   -5.7030   -0.06381 8.928e-02    0.3186
# beta[2]     0.213    0.4458    1.16697 1.967e+00    3.8106
# sigma      25.988   28.2563   29.56916 3.097e+01   33.9705
# x.change 1901.085 1908.0860 1976.65741 1.989e+03 2006.8528


# Previous set of iterations
# Iterations = 200:100100
# Thinning interval = 100 
# Number of chains = 4 
# Sample size per chain = 1000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#     
#     Mean      SD Naive SE Time-series SE
# alpha      74.5623  10.678  0.16884         0.3983
# beta[1]   -43.8351 192.981  3.05129         4.1057
# beta[2]     0.8367  65.035  1.02830         0.9877
# sigma      29.6058   2.054  0.03248         0.0330
# x.change 1959.1811  37.436  0.59192         1.9204
# 
# 2. Quantiles for each variable:
#     
#     2.5%       25%        50%       75%     97.5%
# alpha      55.2229   66.7616   74.54420 8.174e+01   96.9494
# beta[1]  -519.5727   -4.8591   -0.06038 9.078e-02    0.3296
# beta[2]     0.2191    0.4559    1.18896 1.997e+00    3.7266
# sigma      25.9274   28.1652   29.45888 3.095e+01   33.7916
# x.change 1901.0910 1908.7782 1976.94698 1.989e+03 2004.0438


codaSamplesDIC<-dic.samples(jags, 1000000, 500)
codaSamplesDIC

# Mean deviance:  1096 
# penalty 7.704 
# Penalized deviance: 1104 

# previous iteration
# Mean deviance:  1096 
# penalty 7.727 
# Penalized deviance: 1104 




##### Fitting climate data using model formulation 2 #####

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
x<-seq(from=0,to=10,len=100)
y<-dgamma(x,0.001,0.001)
plot(x,y,typ='l')

codaSamplesDIC<-dic.samples(jags, 1000000, 100)
codaSamplesDIC
# Mean deviance:  1094 
# penalty 5.032 
# Penalized deviance: 1099 

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(window(codaSamples,start=2000))
traceplot(codaSamples)
gelman.diag(codaSamples)
gelman.plot(codaSamples)



# Iterations = 2000:101100
# Thinning interval = 100 
# Number of chains = 4 
# Sample size per chain = 992 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#     
#     Mean       SD  Naive SE Time-series SE
# beta[1]       -8.55613 97.16196 1.5424477       7.750700
# beta[2]        0.04447  0.05042 0.0008004       0.004034
# beta[3]        1.79824 22.35626 0.3549059       0.304304
# changePoint 1988.79883 29.66710 0.4709657       1.695101
# sigma         29.45355  2.09148 0.0332024       0.036401
# 
# 2. Quantiles for each variable:
#     
#     2.5%        25%        50%       75%     97.5%
# beta[1]     -211.68580  -71.54830   -5.86138 5.784e+01  176.3678
# beta[2]       -0.05107    0.01039    0.04333 7.707e-02    0.1501
# beta[3]       -7.65284    1.25443    1.78749 2.390e+00   13.1074
# changePoint 1958.47862 1980.22495 1987.02997 1.992e+03 2077.5850
# sigma         25.77975   28.00609   29.27482 3.076e+01   34.0887
# 





######## Simple regression  model: climate data ############

modelString<-"
model {
for (i in 1:N) {
y[i] ~ dnorm(mu[i], tau)
# alpha is y value at changepoint location
mu[i] <- alpha + beta * x[i]
}
tau ~ dgamma(0.001, 0.001)
alpha ~ dnorm(0.0, 1.0E-6)
beta~ dnorm(0.0, 1.0E-6)
sigma <- 1 / sqrt(tau)
}
"

writeLines(modelString, con='TEMPmodel.txt')


library('rjags')

jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
                   inits<-list(
                       list('alpha'=1000,'beta'=2,'tau'=0.5),
                       list('alpha'=100,'beta'=3,'tau'=0.2),
                       list('alpha'=10,'beta'=10,'tau'=0.7),
                       list('alpha'=200,'beta'=-2,'tau'=0.15)),
                   n.chains = 4,
                   n.adapt = 100)


codaSamples<-coda.samples(jags, c('alpha','beta',
                                  'sigma'), 100000, 100)
summary(codaSamples)

# Iterations = 100:1e+05
# Thinning interval = 100 
# Number of chains = 4 
# Sample size per chain = 1000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#     
#     Mean       SD Naive SE Time-series SE
# alpha -575.6955 457.6123 7.235485         58.240
# beta     0.3375   0.2342 0.003702          0.030
# sigma   40.8876 396.8228 6.274320          6.275
# 
# 2. Quantiles for each variable:
#     
#     2.5%       25%       50%       75%     97.5%
# alpha -1527.9346 -660.7701 -525.0906 -421.9477 -158.5643
# beta      0.1235    0.2588    0.3116    0.3812    0.8237
# sigma    27.1106   29.4121   30.8264   32.3601   39.3016
# 




codaSamplesDIC<-dic.samples(jags, 100000, 100)

codaSamplesDIC

# Mean deviance:  1104 
# penalty 2.97 
# Penalized deviance: 1107 



library(coda)
summary(codaSamplesDIC,start=1000)

plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)
traceplot(codaSamples)

#     2.5%       25%        50%       75%     97.5%
# alpha      55.622   66.4208   74.44240 8.184e+01   97.5260
# beta[1]  -578.948   -5.7030   -0.06381 8.928e-02    0.3186
# beta[2]     0.213    0.4458    1.16697 1.967e+00    3.8106
# sigma      25.988   28.2563   29.56916 3.097e+01   33.9705
# x.change 1901.085 1908.0860 1976.65741 1.989e+03 2006.8528

#### Plotting ##############

x<-myDat$year
# y<-myDat$Annual.total
y<-myDat$Annual.extreme
N<-length(x)
pdf(file='changePointModel')
plot(x,y)
abline(a=-575.6955,b= 0.3375,col='black')

x<-seq(from=1901,to=2014,len=500)
yMean<-ifelse(x <1977, 74.44 + -0.06381*(x-1976.95),74.44 + 1.167*(x-1976.95))
lines(x,yMean,col='red')

dev.off()







