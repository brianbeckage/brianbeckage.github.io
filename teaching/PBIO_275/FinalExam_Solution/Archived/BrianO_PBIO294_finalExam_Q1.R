#Final exam ques1
# Fit 4 diff models
# mod1: null model   (Yhat = B0 + error)
# mod2: linear model (Yhat = B0 + B1*Burns + error)
# mod3: quadr model  (Yhat = B0 + B1*Burns + B2*Burns^2 + error)
# mod4: satur model  (Yhat = B0 + B1* ( exp(B2*Burns) / (1+exp(B2*Burns)) ) + error)



setwd("C:/Users/Administrator/Desktop/Becakge Final Projects n Exam")
dir()
IDH = read.csv("idh.csv", header=T)
head(IDH)
plot(IDH$sr~IDH$nFires)
mean(IDH$sr) #[1] 16.67333
sd(IDH$sr)   #[1] 7.323848

# mod1: null model   (Yhat = B0 + error)

N = length(IDH$sr)

### Your code to fit the model in JAGS goes here.
library(rjags)
modelString<-"
model {
for (i in 1:N){
y[i] ~ dpois(y.hat[i])
log(y.hat[i]) <- B0
}
B0 ~ dnorm(15, .0001)
}
"
writeLines(modelString, con='TEMPmodel.txt')


jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = IDH$nFires,
                               'y' = IDH$sr,
                               'N' = N),
                   inits<-list(
                     list('B0'=15),
                     list('B0'=16),
                     list('B0'=17)),
                   n.chains = 3,
                   n.adapt = 100)


update(jags, 1000)

jags.samples(jags,
             c('B0'),
             10000)

codaSamples<-coda.samples(jags, c('B0'), 1000, 1)
codaSamples<-coda.samples(jags, c('B0'), 10000, 1)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)
# Iterations = 12101:22100
# Thinning interval = 1 
# Number of chains = 3 
# Sample size per chain = 10000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean             SD       Naive SE Time-series SE 
# 2.814e+00      1.426e-02      8.232e-05      8.920e-05 
# 
# 2. Quantiles for each variable:
#   
#   2.5%   25%   50%   75% 97.5% 
# 2.786 2.804 2.814 2.823 2.842
traceplot(codaSamples)



### Mod2:
### Your code to fit the model in JAGS goes here.
library(rjags)
modelString<-"
model {
for (i in 1:N){
y[i] ~ dpois(y.hat[i])
log(y.hat[i]) <- B0 + B1*x[i]
}
B0 ~ dnorm(15, .0001)
B1 ~ dnorm(1, .0001)
}
"
writeLines(modelString, con='TEMPmodel.txt')


jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = IDH$nFires,
                               'y' = IDH$sr,
                               'N' = N),
                   inits<-list(
                     list('B0'=15, 'B1'=0),
                     list('B0'=16, 'B1'=1),
                     list('B0'=17, 'B1'=2)),
                   n.chains = 3,
                   n.adapt = 100)


update(jags, 1000)

jags.samples(jags,
             c('B0','B1'),
             10000)

codaSamples<-coda.samples(jags, c('B0','B1'), 1000, 1)
codaSamples<-coda.samples(jags, c('B0','B1'), 10000, 1)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)
# Iterations = 12101:22100
# Thinning interval = 1 
# Number of chains = 3 
# Sample size per chain = 10000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean       SD  Naive SE Time-series SE
# B0  3.03085 0.029230 1.688e-04      5.120e-04
# B1 -0.04076 0.004945 2.855e-05      8.639e-05
# 
# 2. Quantiles for each variable:
#   
#   2.5%     25%      50%     75%    97.5%
# B0  2.97330  3.0109  3.03081  3.0507  3.08812
# B1 -0.05041 -0.0441 -0.04076 -0.0374 -0.03094

traceplot(codaSamples)

############### mod3: quadr model  (Yhat = B0 + B1*Burns + B2*Burns^2 + error)
library(rjags)
modelString<-"
model {
for (i in 1:N){
y[i] ~ dpois(y.hat[i])
log(y.hat[i]) <- B0 + B1*x[i] + B2*x[i]^2 
}
B0 ~ dnorm(15, .0001)
B1 ~ dnorm(1, .0001)
B2 ~ dnorm(1, .0001)
}
"
writeLines(modelString, con='TEMPmodel.txt')


jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = IDH$nFires,
                               'y' = IDH$sr,
                               'N' = N),
                   inits<-list(
                     list('B0'=15, 'B1'=0, 'B2'=1),
                     list('B0'=16, 'B1'=1, 'B2'=2),
                     list('B0'=17, 'B1'=2, 'B2'=3)),
                   n.chains = 3,
                   n.adapt = 100)


update(jags, 1000)

jags.samples(jags,
             c('B0','B1','B2'),
             10000)

codaSamples<-coda.samples(jags, c('B0','B1','B2'), 1000, 1)
codaSamples<-coda.samples(jags, c('B0','B1','B2'), 10000, 1)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)
# 
# Iterations = 12101:22100
# Thinning interval = 1 
# Number of chains = 3 
# Sample size per chain = 10000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean       SD  Naive SE Time-series SE
# B0  1.83686 0.058874 3.399e-04      0.0036664
# B1  0.54949 0.023777 1.373e-04      0.0017886
# B2 -0.05531 0.002187 1.263e-05      0.0001527
# 
# 2. Quantiles for each variable:
#   
#   2.5%     25%      50%      75%    97.5%
# B0  1.7235  1.7972  1.83635  1.87554  1.95681
# B1  0.4997  0.5341  0.55010  0.56560  0.59420
# B2 -0.0594 -0.0568 -0.05536 -0.05388 -0.05073

traceplot(codaSamples)

############### mod 4: (Yhat = B0 + B1* ( exp(B2*Burns) / (1+exp(B2*Burns)) ) + error)
library(rjags)
modelString<-"
model {
for (i in 1:N){
y[i] ~ dpois(y.hat[i])
log(y.hat[i]) <- B0 + B1*( exp(B2*x[i]) / (1 + exp(B2*x[i])) ) 
}
B0 ~ dnorm(15, .0001)
B1 ~ dnorm(1, .0001)
B2 ~ dnorm(1, .0001)
}
"
writeLines(modelString, con='TEMPmodel.txt')


jags <- jags.model('TEMPmodel.txt',
                   data = list('x' = IDH$nFires,
                               'y' = IDH$sr,
                               'N' = N),
                   inits<-list(
                     list('B0'=15, 'B1'=0, 'B2'=1),
                     list('B0'=16, 'B1'=1, 'B2'=2),
                     list('B0'=17, 'B1'=2, 'B2'=3)),
                   n.chains = 3,
                   n.adapt = 100)


update(jags, 1000)

jags.samples(jags,
             c('B0','B1','B2'),
             10000)

codaSamples<-coda.samples(jags, c('B0','B1','B2'), 1000, 1)
codaSamples<-coda.samples(jags, c('B0','B1','B2'), 10000, 1)

library(coda)
plot(codaSamples, trace = FALSE, density = TRUE)
summary(codaSamples)
# Iterations = 12101:22100
# Thinning interval = 1 
# Number of chains = 3 
# Sample size per chain = 10000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#   
#   Mean     SD Naive SE Time-series SE
# B0  8.547  4.162  0.02403         0.2833
# B1 -5.733  4.162  0.02403         0.2856
# B2 37.692 18.333  0.10585         0.1416
# 
# 2. Quantiles for each variable:
#   
#   2.5%     25%    50%    75%  97.5%
# B0   4.349   5.247  6.442 13.964 15.213
# B1 -12.400 -11.152 -3.628 -2.435 -1.536
# B2   8.464  21.774 37.051 53.265 69.068
traceplot(codaSamples)

### Now plot the models against data. Report AICs for 4 models.
### Using 50% quantile as parameters.
plot(IDH$sr~IDH$nFires)

IDH$nullSR = exp(2.814) # B0 param for null model 
lines(IDH$nullSR~IDH$nFires)

IDH$linearSR = exp(3.03081 + -0.04076*IDH$nFires) # B0 & B1 param for linear model 
lines(IDH$linearSR~IDH$nFires, col="red")

IDH$quadrSR = exp(1.83635  + 0.55010*IDH$nFires + -0.05536*IDH$nFires^2)
lines(IDH$quadrSR~IDH$nFires, col="blue")

IDH$saturSR = exp(6.442 + -3.628* ( exp(37.051*IDH$nFires) / (1+exp(37.051*IDH$nFires)) ) )
lines(IDH$saturSR~IDH$nFires, col="grey")
#############################################################

### Calc nll of each model and use AIC to compare:
xSeq<-seq(from=1,to=40,len=1000)
yLik<-vapply(xSeq,nllPois,FUN.VALUE=1,data=IDH$sr)   #countsByCorps
plot(xSeq,yLik,typ='l')

nllPois<-function(parVec,data){
  if(length(parVec)>1){
    b0 = parVec[1]
    b1 = parVec[2]
  }else{
    b0 = parVec[1]
    b1 = 0}   
  srPred = exp(b0 + b1*IDH$sr)
  nllik<- -sum(dpois(x=data,lambda=srPred,log=TRUE))
  return(nllik)
}

parVec= 2.814
nllPois(parVec,IDH$sr)
# [1] 1217.373 nll of null model
AIC = 2*(1) + 2*(1217.373) #one parameter which was just an intercept, AIC of negLogLik
> AIC
#[1] 2436.746

parVec= c(3.03081, -0.04076)
nllPois(parVec,IDH$sr)
# [1] 2475.803 nll of linear model  


nllPoisQUAD<-function(parVec,data){
  if(length(parVec)>1){
    b0 = parVec[1]
    b1 = parVec[2]
    b2 = parVec[3]
  }
  srPred = exp(b0 + b1*IDH$sr+b2*IDH$sr^2)
  nllik<- -sum(dpois(x=data,lambda=srPred,log=TRUE))
  return(nllik)
}
parVec = c(1.83635, 0.55010, -0.05536)
nllPoisQUAD(parVec,IDH$sr)
# [1] 68478.33
AIC =2*3+2*(68478.33)
AIC
# [1] 136962.7

nllPoisSATUR<-function(parVec,data){
  if(length(parVec)>1){
    b0 = parVec[1]
    b1 = parVec[2]
    b2 = parVec[3]
  }
  srPred = exp(6.442 + -3.628* ( exp(37.051*IDH$nFires) / (1+exp(37.051*IDH$nFires)) ) )
  nllik<- -sum(dpois(x=data,lambda=srPred,log=TRUE))
  return(nllik)
}
parVec = c(6.442, -3.628, 37.051)
nllPoisSATUR(parVec,IDH$sr)
# [1] 1217.373
AIC =2*3+2*(1217.373)
AIC
# [1] 2440.746

####### AICs
null_modAIC = 2436.746
linea_modAIC= 68478.33
quadr_modAIC= 136962.7
satur_modAIC= 2440.746

# Delta AIC are calculated as the distance from your best model, with the lowest AIC score.
deltaAIC_mod1 = null_modAIC - null_modAIC   # [1] 0
deltaAIC_mod2 = linea_modAIC - null_modAIC  # [1] 66041.58
deltaAIC_mod3 = quadr_modAIC - null_modAIC  # [1] 134526
deltaAIC_mod4 = satur_modAIC - null_modAIC  # [1] 4

best model was the null model according to deltaAIC. Saturating model was next best model,
but was greater then 2 units more then null model so it doesn't make the cut.


