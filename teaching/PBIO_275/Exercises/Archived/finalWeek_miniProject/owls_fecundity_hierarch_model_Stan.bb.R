#Marina Golivets
#12/1/2015

#owls fecundity example

#require rstan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# generate simulated data
N<-100 #sample size (total number of individuals)
ter<-runif(N, 0, 57)#territory size, km2, assumes no measurement error
age<-runif(N, 0, 16) #reproductive age

#average reproduction at age of 0 is the function of territory size
max_fec<-3 # maximum fecundity at age of 0
decl_rate<-0.03 #decline rate
mu_a<-max_fec*exp(-decl_rate*ter)
#sig_a<-rgamma(N,shape=1,rate=3)
sig_a<-0.3
a<-rgamma(N,mu_a^2/sig_a^2,mu_a/sig_a^2) #alpha, or intercept
plot(a,ter)

# average individual fecundity (w) changes with age according to the quadratic function
b1<-0.005
b2<--0.001
mu_w<-a+b1*age+b2*age^2 
#sig_w<-rgamma(N,shape=1,rate=3) 
sig_w<-0.5
w<-rgamma(N, mu_w^2/sig_w^2, mu_w/sig_w^2)
plot(w,age)

fec<-rpois(N,w) #individual fecundities
data<-data.frame(ter,age,mu_a,mu_w,fec)
data

# ********** Stan model **********

modelString <- "

data {
int<lower=0> N ; // number of observations (individuals)
real <lower=0> age[N] ; // age
real <lower=0> ter[N]; // territory size
int fec[N] ; // observed fecundity
int max_fec; // maximum fecundity at reproductive age 0
real decl_rate; // decline rate // might need <lower=0>
}

transformed data {
real <lower=0> mu_a[N]; // mean alpha // depends on territory size

for (i in 1:N)
mu_a[i]<-max_fec*exp(-decl_rate*ter[i]);
}

parameters {
real b1; // might need  <lower=0> 
real b2; // might need <upper=0> 
real <lower=0> sig_a;
real <lower=0> sig_w;
real <lower=0> a[N];
real <lower=0> lambda[N];
}

transformed parameters{
real <lower=0> mu_w[N]; // depends on age

for (i in 1:N)
mu_w[i]<-a[i]+b1*age[i]+b2*pow(age[i],2);
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
}
" 

writeLines(modelString, con="TEMPmodelStan.txt")
stan_code<-readChar("TEMPmodelStan.txt",file.info("TEMPmodelStan.txt")$size)

#data to be sent to Stan
fec<-fec
age<-age
ter<-ter
max_fec<-max_fec
decl_rate<-decl_rate
N<-N

dataList<-list( fec=fec, age=age, ter=ter, max_fec=max_fec, decl_rate=decl_rate, N=N)

resStan <- stan(model_code = stan_code, data = dataList,
                chains = 3, iter = 10000, warmup = 2500, thin = 1)

show(resStan)

#resStan<-extract(resStan)
#plot(resStan)

#library(shinystan)
#resStan<-launch_shinystan(resStan)
                      
# convert stan format to coda format
library('coda')
mcmcCoda <- mcmc.list( lapply( 1:ncol(resStan) ,
                               function(x) { mcmc(as.array(resStan)[,x,])}))
traceplot(mcmcCoda)

summary(mcmcCoda)$statistics
