
#### Finding the MLE for a Binomial process #########################

Binomial distribution: Pr(X=k)=choose(n, k) p^k (1-p)^(n-k)
where choose(n,k)=n!/(k!(n-k)!)

Bernoulli distribution
Pr(X=1)= p^k (1-p)^k for k= (0 or 1)

#### Negative log ikelihood function
nllBinom<-function(p,k,nTrials){
  nll<--sum(dbinom(x=k,size=nTrials,prob=p,log=TRUE),na.rm=TRUE)
  return(nll)
}

### Minimizing nll function
kObs<-rbinom(n=10,size=100,prob=0.5)
p<-0.8 # c(0.8) # Initial parameter values 
outNull<-optim(par=p,fn=nllBinom,method="L-BFGS-B",lower=c(0.001),upper=c(0.999),k=kObs,nTrials=100)
outNull$par 
outNull$val 



#### Poisson Regression ###################################################

# A ecologist surveys 10 1m^2 quadrats for number of seedlings and records the 
# corresponding light level for each quadrat
# The ecologist believes that the number of seedlings follows Poisson distribution.  

# Question:  What is the relationship between nSeedlings and light level?

### What is the Poisson distn?




# Generating some simulated data
light<-seq(0.1, 0.8,len=100)
# ln(lambda)<-beta0+beta1*light
beta0<-1.0; beta1<-3.0
myLambda<-exp(beta0+beta1*light)
nSeedlings<-rpois(n=length(myLambda),lambda=myLambda)
plot(light,nSeedlings)

### Recovering parameters

nllPoi<-function(parVec,sdlgs,lght){
  if(length(parVec)>1){
    b0<-parVec[1]
    b1<-parVec[2]
  } else{
    b0<-parVec[1]
    b1<-0
  }

  sdlgPred<-exp(b0+b1*lght) # lmbdaPred
  
  nllik<- -sum(dpois(x=sdlgs,lambda=sdlgPred,log=TRUE))
  #nllik<- -(sum( (dbinom(x=nSurvive, size=nPlanted, prob=pSurv,log=TRUE)) ,na.rm=T))
  cat("nllik= ",nllik,sep=" ",fill=T);cat(" ",sep=" ",fill=T)
  return(nllik)
}

### Minimizing our function using a two parameter model
parVec<-c(0.5,1.0) # Initial parameter values 
outNull<-optim(par=parVec,fn=nllPoi,method="L-BFGS-B",lower=c(-Inf),upper=c(Inf),sdlgs=nSeedlings,lght=light)
outNull$par # compared to 1.0 3.0 
outNull$val # nllik= 23.47263
myAIC<-2*2 + 2*outNull$val 
myAIC

### Minimizing our function using a one parameter model
parVec<-c(0.5) # Initial parameter values 
outNull<-optim(par=parVec,fn=nllPoi,method="L-BFGS-B",lower=c(-Inf),upper=c(Inf),sdlgs=nSeedlings,lght=light)
outNull$par 
outNull$val # nllik
myAIC<-2*1 + 2*outNull$val 
myAIC

### Which is the better model according to AIC?


# AIC 
AIC = 2*k - 2*ln(L) where L is the maximized likelihood.

# for logistic regression
logit(p)=log(p/(1-p))
logit^-1=exp(p)/(1+exp(p))

p=exp(b0+b1*gap)/(1+exp(b0+b1*gap))

