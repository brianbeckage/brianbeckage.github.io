# A hierarchical model of owl fecudity.  We generate data and refit parameters.

setwd("~/Documents/Web/Teaching/DataAnalysis/Exercises")

### Simulating data ######

### Size of owl home range: u
nDataPoints<-1000
u<-runif(n=nDataPoints,min=2.67,max=57.34) # km^2
### Owl age: x
x<-runif(n=nDataPoints,min=1,max=17)
### Age vs fecundity
v<-0.04; g<-10
muA<-g*exp(-v*u)
plot(u,muA)
sigmaA<-0.01
a<-mapply(rgamma,1,muA^2/sigmaA,muA/sigmaA)
b1<-2; b2<- -0.1;
muL<-a+b1*x+b2*x^2
sigmaL<-1.0
lambdaVect<-mapply(rgamma,1,muL^2/sigmaL,muL/sigmaL)
lambdaVect<-as.vector(lambdaVect)
fecundityVect<-mapply(rpois,rep(1,length(lambdaVect)), lambdaVect)
uVect<-mapply(rep,u,1)
uVect<-as.vector(uVect)
xVect<-mapply(rep,x,1)
xVect<-as.vector(xVect)
owlDat<-data.frame(fecundityVect,xVect,uVect)
colnames(owlDat)<-c('fecundity','age','homerange')
ageGroup<-ifelse(owlDat$age<5,1,0)
owlDat<-data.frame(owlDat,ageGroup)
owlDat<-owlDat[-585,]

### Estimate g,v,b1,b2


plot(owlDat$age,owlDat$fecundity)
plot(owlDat$homerange,owlDat$fecundity)
require(lattice)
xyplot(fecundity~homerange|ageGroup,data=owlDat)

setwd("~/Documents/Web/Teaching/DataAnalysis/Exercises")

modelString<-"
model {

for (i in 1:N){
    muA[i]<-g*exp(-v*homerange[i])
    shapeA[i]<-(muA[i]^2)/(sigmaA^2)
    rateA[i]<-muA[i]/(sigmaA^2)
    a[i]~dgamma(shapeA[i],rateA[i])
    muL[i]<-a[i]+b1*age[i]+b2*age[i]^2
    shapeL[i]<-(muL[i]^2)/(sigmaL)^2
    rateL[i]<-muL[i]/(sigmaL)^2
    lambda.hat[i]~dgamma(shapeL[i],rateL[i])
    fecundity[i] ~ dpois(lambda.hat[i])
}
g ~ dnorm(0, .001)
v ~ dnorm(0, .001)
b1 ~ dnorm(0, .001)
b2 ~ dnorm(0, .001)
sigmaA ~ dunif(0,100)
sigmaL ~ dunif(0,100)
}
"

writeLines(modelString, con='TEMPmodel.txt')


library('rjags')

jags <- jags.model('TEMPmodel.txt',
                   data = list('fecundity' = owlDat[,'fecundity'],
                               'age'=owlDat[,'age'],
                               'homerange' = owlDat[,'homerange'],
                               'N' = length(owlDat[,'fecundity']) ),
                   inits<-list(
                       list('g'=5,'v'=0.08,'b1'=1,'b2'=-0.01,'sigmaA'=0.02,'sigmaL'=0.5),
                       list('g'=3,'v'=0.05,'b1'=1.5,'b2'=-0.05,'sigmaA'=1.0,'sigmaL'=0.05),
                       list('g'=25,'v'=0.1,'b1'=2,'b2'=-0.001,'sigmaA'=0.2,'sigmaL'=5),
                       list('g'=15,'v'=0.01,'b1'=4,'b2'=-0.2,'sigmaA'=0.005,'sigmaL'=2)),
                   n.chains = 4,
                   n.adapt = 1000)


codaSamples<-coda.samples(jags, c('g','v','b1','b2','sigmaA','sigmaL'), 20000, 100)

library(coda)
plot(codaSamples, trace = TRUE, density = TRUE)
summary(window(codaSamples,start=1000))
traceplot(codaSamples)



v<-0.04; g<-10
sigmaA<-0.01
b1<-2; b2<- -0.1;
sigmaL<-1.0






### STAN code ####################

# ********** Stan model ********** Marina

modelString <- "
data {
int<lower=0> N ; // number of observations (individuals)
real <lower=0> age[N] ; // age
real <lower=0> ter[N]; // territory size
int fec[N] ; // observed fecundity
}


parameters {
real b1; // might need  <lower=0> 
real b2; // might need <upper=0> 
real <lower=0> max_fec;
real decl_rate;
real <lower=0> sig_a;
real <lower=0> sig_w;
real <lower=0> a[N];
real <lower=0> lambda[N];

}

transformed parameters{
real <lower=0> mu_w[N]; // depends on age
real <lower=0> mu_a[N]; // mean alpha // depends on territory size

for (i in 1:N){
mu_w[i]<-a[i]+b1*age[i]+b2*pow(age[i],2);
mu_a[i]<-max_fec*exp(-decl_rate*ter[i]);
}
}

model {

for(i in 1:N){
a[i]~gamma(pow(mu_a[i],2) / pow(sig_a,2), mu_a[i] / pow(sig_a,2));
lambda[i]~gamma(pow(mu_w[i],2) / pow(sig_w,2), mu_w[i] / pow(sig_w,2));
fec[i]~poisson(lambda[i]) ;
}

// priors
sig_a~inv_gamma(.001,.001);
sig_w~inv_gamma(.001,.001); 
b1~normal(0,100);
b2~normal(0,100);
decl_rate~normal(0,100);
max_fec~uni
}
" 

writeLines(modelString, con="TEMPmodelStan.txt")
stan_code<-readChar("TEMPmodelStan.txt",file.info("TEMPmodelStan.txt")$size)

#data to be sent to Stan
fec<-owlDat[,'fecundity']
age<-owlDat[,'age']
ter<-owlDat[,'homerange']
N<-length(owlDat[,'fecundity']) 



dataList<-list( fec=fec, age=age, ter=ter, N=N)
library(rstan)

resStan <- stan(model_code = stan_code, data = dataList,
                chains = 3, iter = 10000, warmup = 2500, thin = 1)

show(resStan)

#resStan<-extract(resStan)
#plot(resStan)

library(shinystan)
resStan<-launch_shinystan(resStan)

# convert stan format to coda format
library('coda')
mcmcCoda <- mcmc.list( lapply( 1:ncol(resStan) ,
                               function(x) { mcmc(as.array(resStan)[,x,])}))
traceplot(mcmcCoda)

summaryStats<-summary(mcmcCoda)$statistics

            Mean        SD    Naive SE Time-series SE
b1         1.8148350 0.2465102 0.001643401   0.0032641355
b2         0.6583700 0.8927212 0.005951475   0.0001535992
max_fec    4.6928146 4.0981128 0.027320752   0.0079878352
decl_rate -0.5504247 0.4283010 0.002855340   0.0001735410
sig_a      0.6959199 0.3498833 0.002332555   0.0191883257
sig_w      2.1017250 1.0270946 0.006847297   0.0103789162

compared to
v<-0.04; g<-10
muA<-g*exp(-v*u)
plot(u,muA)
a<-rgamma(n=nDataPoints,muA^2/0.01,muA/0.01)
b1<-2; b2<- -0.1;
sigmaA 0.01
sigmaL 1



# ********** Stan model ********** Modified by Brian

modelString <- "
data {
int<lower=0> N ; // number of observations (individuals)
real <lower=0> age[N] ; // age
real <lower=0> ter[N]; // territory size
int fec[N] ; // observed fecundity
}


parameters {
real b1; // might need  <lower=0> 
real b2; // might need <upper=0> 
real <lower=0> max_fec;
real decl_rate;
real <lower=0> sig_a;
real <lower=0> sig_w;
real <lower=0> a[N];
real <lower=0> lambda[N];

}

transformed parameters{
real <lower=0> mu_w[N]; // depends on age
real <lower=0> mu_a[N]; // mean alpha // depends on territory size

for (i in 1:N){
mu_w[i]<-a[i]+b1*age[i]+b2*pow(age[i],2);
mu_a[i]<-max_fec*exp(-decl_rate*ter[i]);
}
}

model {

for(i in 1:N){
a[i]~gamma(pow(mu_a[i],2) / pow(sig_a,2), mu_a[i] / pow(sig_a,2));
lambda[i]~gamma(pow(mu_w[i],2) / pow(sig_w,2), mu_w[i] / pow(sig_w,2));
fec[i]~poisson(lambda[i]) ;
}

// priors
sig_a~inv_gamma(.001,.001);
sig_w~inv_gamma(.001,.001); 
b1~normal(0,100);
b2~normal(0,100);
decl_rate~normal(0,100);;
}
" 

writeLines(modelString, con="TEMPmodelStan.txt")
stan_code<-readChar("TEMPmodelStan.txt",file.info("TEMPmodelStan.txt")$size)

#data to be sent to Stan
fec<-owlDat[,'fecundity']
age<-owlDat[,'age']
ter<-owlDat[,'homerange']
N<-length(owlDat[,'fecundity']) 


dataList<-list( fec=fec, age=age, ter=ter, N=N)
library(rstan)

resStan <- stan(model_code = stan_code, data = dataList,
                chains = 3, iter = 10000, warmup = 2500, thin = 1)


compared to
v<-0.04; g<-10
muA<-g*exp(-v*u)
plot(u,muA)
a<-rgamma(n=nDataPoints,muA^2/0.01,muA/0.01)
b1<-2; b2<- -0.1;
sigmaA 0.01
sigmaL 1

1. Empirical mean and standard deviation for each variable,
plus standard error of the mean:
    
    Mean      SD Naive SE Time-series SE
b1      1.66865 0.92075 0.032554       0.021775
b2     -0.08311 0.04580 0.001619       0.001146
g      11.57452 4.34061 0.153464       0.514192
sigmaA  0.26012 0.38865 0.013741       0.085021
sigmaL  1.66074 0.75568 0.026717       0.075825
v       0.04951 0.03002 0.001061       0.001391

2. Quantiles for each variable:
    
    2.5%      25%      50%      75%     97.5%
b1      0.150224  1.17986  1.93350  2.43743  2.706858
b2     -0.137429 -0.12334 -0.09559 -0.04880 -0.007844
g       4.986416  6.27537 11.36200 15.07405 20.150560
sigmaA  0.008538  0.02889  0.09095  0.25478  1.428386
sigmaL  0.179811  1.26417  1.62925  2.21866  2.916438
v       0.009984  0.02180  0.05146  0.07728  0.091319





