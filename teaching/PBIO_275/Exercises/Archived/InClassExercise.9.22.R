#### Introduction to Maximum likelihood techniques

Binomial distribution: Pr(X=k)=choose(n, k) p^k (1-p)^(n-k)

where choose(n,k)=n!/(k!(n-k)!)

Bernoulli distribution
Pr(X=1)= p^k (1-p)^k for k= (0 or 1)

### Seed germination example: 4 seeds with 3 germinating to produce seedlings

### Sampling distribution

x<-rbinom(n=10000,size=4,prob=0.5)
xProb<-hist(x,breaks=c(0,0.5,1.5,2.5,3.5,4.5),plot=T,probability=TRUE)

# Sampling distribution assuming that p=0.5
plot(0:4,dbinom(x=0:4,size=4,prob=0.5),ylab="Prob",xlab="Number of Successes",ylim=c(0,0.4))
mtext(side=3,"Sampling Distribution",line=1)

# Likelihood: Here we determine the likely value of p given 3 seedlings for 4 seeds
plot(seq(from=0,to=1,length=100),dbinom(x=3,size=4,prob=seq(from=0,to=1,length=100)),
     typ="l",ylab="Likelihood",xlab="P")
mtext(side=3,"Likelihood")
abline(v=0.75)

#### EXERCISE 0

# Compare the likelihood function for p in the following two cases:
# A Bernoulli process that results in c(0,1,1,0,0,1,0,1,0,0,1,1,1,1,0,0,1,0)
# and a Binomial(k=9,n=18).  Plot the likelihood functions in the same figure.

mylikeBern<-function(p,x){
  return(p^x*(1-p)^(1-x))
}



BernLike<-function(p,k){
  y2<-vector()
  j<-0
  for(i in p){
    j<-j+1
    y2[j]<-MylikeBern(i,k)
  }
  return(y2)
}



#### EXERCISE 1

# Now instead of observing only (k=3,n=4) for a single coin, you are interested in testing the population of coins
# produced at a US mint.  You sample 10 coins, 10 times each.  You observe the following number of heads:
#   6 3 7 3 6 4 4 5 3 8
# Is the mint producing fair coins?  Plot the likelihood function.
#   


#### EXERCISE 3
# Make a figure with 6 panels that show the likelihoods for the following samples (seedlings,seeds):
# (3,4),(6,8),(12,16),(24,32),(300,400), and (3000,4000).
# Hint: Use par(mfrow=c(3,2)) to set up a figure with six panels


parOld<-par(mfrow=c(3,2))
plot(seq(from=0,to=1,length=100),dbinom(x=3,size=4,prob=seq(from=0,to=1,length=100)),
     typ="l",ylab="Likelihood",xlab="P")
plot(seq(from=0,to=1,length=100),dbinom(x=6,size=8,prob=seq(from=0,to=1,length=100)),
     typ="l",ylab="Likelihood",xlab="P")
plot(seq(from=0,to=1,length=100),dbinom(x=12,size=16,prob=seq(from=0,to=1,length=100)),
     typ="l",ylab="Likelihood",xlab="P")
plot(seq(from=0,to=1,length=100),dbinom(x=24,size=32,prob=seq(from=0,to=1,length=100)),
     typ="l",ylab="Likelihood",xlab="P")
plot(seq(from=0,to=1,length=100),dbinom(x=300,size=400,prob=seq(from=0,to=1,length=100)),
     typ="l",ylab="Likelihood",xlab="P")
plot(seq(from=0,to=1,length=100),dbinom(x=3000,size=4000,prob=seq(from=0,to=1,length=100)),
     typ="l",ylab="Likelihood",xlab="P")
par(parOld)


### EXERCISE 4
# Calculate the likelihood ratio for the Hypothesis A: Seedling germination probability is 1/2 vs
# Hypothesis B: Seedling germination probability is 3/4 for the sample sizes in Exercise 3.

# Solution: Likelihood at mle 0.75 = 0.421875
dbinom(x=3,size=4,prob=0.5)/dbinom(x=3,size=4,prob=0.75)
dbinom(x=300,size=400,prob=0.5)/dbinom(x=300,size=400,prob=0.75)

#### Exercise 5

# A ecologist surveys 10 1m^2 quadrats and finds the following number of seedlings:
#   c(3, 2, 1, 1, 4, 2, 2, 3, 6, 1, 3, 5, 0, 1, 3, 0, 1, 2, 4, 3)
# The ecologist believes that the number of seedlings follows Poisson distribution.  What is
# the mean of the Poisson?  Show (plot) the likelihood function for the mean.

# Another ecologist has a single 20m^2 plot and observes 47 seedlings and 
# wants to estimate the mean number of seedlngs per m^2.
# What does their likelihood function look like? 
# Compare the two likelihoods--how similar are they?

#### EXERCISE 6 Poisson regression

light<-seq(0.1, 0.8,len=10)
# ln(lambda)<-beta0+beta1*light
beta0<-1.0; beta1<-3.0
lam<-exp(beta0+beta1*light)
nSeedlings<-rpois(n=length(lam),lambda=lam)
plot(light,nSeedlings)

light=c(0.10, 0.18, 0.26, 0.33, 0.41, 0.49, 0.57, 0.64, 0.72, 0.80)
seedlings=c(3,  4,  8,  8,  9, 11, 11, 26, 23, 28)
### Recover parameters


nllPoi<-function(parVec,sdlgs,lght){
  if(length(parVec)>1){
    b0<-parVec[1]
    b1<-parVec[2]
  } else{
    b0<-parVec[1]
    b1<-0
  }

  sdlgPred<-exp(b0+b1*lght)
  
  nllik<- -sum(dpois(x=sdlgs,lambda=sdlgPred,log=TRUE))
  #nllik<- -(sum( (dbinom(x=nSurvive, size=nPlanted, prob=pSurv,log=TRUE)) ,na.rm=T))
  cat("nllik= ",nllik,sep=" ",fill=T);cat(" ",sep=" ",fill=T)
  return(nllik)
}

### Minimizing our function
parVec<-c(0.5,1.0) # Initial parameter values 
outNull<-optim(par=parVec,fn=nllPoi,method="L-BFGS-B",lower=c(-Inf),upper=c(Inf),sdlgs=seedlings,lght=light)
outNull$par # 1.026950 2.969625
outNull$val # nllik= 23.47263
myAIC<-2*2 + 2*outNull$val # 50.94526
myAIC

parVec<-c(0.5) # Initial parameter values 
outNull<-optim(par=parVec,fn=nllPoi,method="L-BFGS-B",lower=c(-Inf),upper=c(Inf),sdlgs=seedlings,lght=light)
outNull$par # 2.572612
outNull$val # nllik= 48.73138
myAIC<-2*1 + 2*outNull$val # 99.46276
myAIC



# AIC 
AIC = 2*k - 2*ln(L) where L is the maximized likelihood.

logit(p)=log(p/(1-p))
logit^-1=exp(p)/(1+exp(p))


