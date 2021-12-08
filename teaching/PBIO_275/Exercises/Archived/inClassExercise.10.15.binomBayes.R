### 

Binomial distribution: Pr(X=k)=choose(n, k) p^k (1-p)^(n-k)
where choose(n,k)=n!/(k!(n-k)!)

Bernoulli distribution
Pr(X=1)= p^k (1-p)^k for k= (0 or 1)


#### Negative log ikelihood function
nllBinom<-function(p,k,nTrials){
  nll<--sum(dbinom(x=k,size=nTrials,prob=p,log=TRUE),na.rm=TRUE)
  return(nll)
}

#### Negative log ikelihood function
lBinom<-function(p,k,nTrials){
  nll<-prod(dbinom(x=k,size=nTrials,prob=p,log=FALSE),na.rm=TRUE)
  return(nll)
}

lBinom(0.3,3,10) # 0.2668279
exp(-nllBinom(0.3,3,10)) # 0.2668279

lBinom(0.3,30,100) # 0.08678386
exp(-nllBinom(0.3,30,100)) # 0.08678386

lBinom(0.3,300,1000) # 0.027521
exp(-nllBinom(0.3,300,1000)) # 0.027521


### Gridded method for calculating Bayesian posterior ##############

#### FLAT PRIOR
parOld<-par(mfrow=c(3,1))
gridLength=100
xSeq<-seq(from=0.01,to=0.99,len=gridLength)
### Flat prior
yPrior<-rep(1/gridLength,times=gridLength)
sum(yPrior) # 1
plot(xSeq,yPrior,typ='l')
### Likelihood
yLik<-vapply(xSeq,lBinom,FUN.VALUE=1,k=300,nTrials=1000)
plot(xSeq,yLik,typ='l')
sum(yLik)
### Posterior
yPost<-yPrior*yLik/(sum(yPrior*yLik))
sum(yPost) # 1
plot(xSeq,yPost,typ='l')
par<-parOld


#### BETA PRIOR
parOld<-par(mfrow=c(3,1))
gridLength=100
xSeq<-seq(from=0.01,to=0.99,len=gridLength)
### Flat prior
a<-5; b<-20
yPrior<-dbeta(xSeq,shape1=a,shape2=b,log=FALSE)
yPrior<-yPrior/sum(yPrior)
sum(yPrior) # 1
a/(a+b) # mean
(a*b)/(((a+b)^2)*(a+b+1)) # var
plot(xSeq,yPrior,typ='l')
### Likelihood
yLik<-vapply(xSeq,lBinom,FUN.VALUE=1,k=30,nTrials=100)
plot(xSeq,yLik,typ='l')
sum(yLik)
### Posterior
yPost<-yPrior*yLik/(sum(yPrior*yLik))
sum(yPost) # 1
plot(xSeq,yPost,typ='l')
par<-parOld


### Checking beta vs built-in dbeta
a<-5; b<-20; x<-0.3
(gamma(a+b)/(gamma(a)*gamma(b)))*x^(a-1)*(1-x)^(b-1)
# 1.962219

dbeta(x,shape1=a,shape2=b,log=FALSE) #  1.962219

#### PDFs
# Binomial distribution: Pr(X=k)=choose(n, k) p^k (1-p)^(n-k)
# where choose(n,k)=n!/(k!(n-k)!)
# 
# Bernoulli distribution
# Pr(X=1)= p^k (1-p)^k for k= (0 or 1)
# 
# Beta distribution
# gamma(a+b)/(gamma(a)*gamma(b)) x^(a-1) (1-x)^(b-1)

### Conjugate prior  ####
a<-5; b<-20 # prior
k=30; nTrials=100
parOld<-par(mfrow=c(3,1))
gridLength=100
xSeq<-seq(from=0.01,to=0.99,len=gridLength)
### Beta prior
yPrior<-dbeta(xSeq,shape1=a,shape2=b,log=FALSE)
#yPrior<-yPrior/sum(yPrior)
plot(xSeq,yPrior,typ='l')
### Likelihood
yLik<-vapply(xSeq,lBinom,FUN.VALUE=1,k=k,nTrials=nTrials)
plot(xSeq,yLik,typ='l')
sum(yLik)
### Posterior
yPost<-(yPrior*yLik)/(sum(yPrior*yLik))
sum(yPost) # 1
plot(xSeq,yPost,typ='l')

aPrime<-a+k; bPrime<-b+nTrials-k
yPostConj<-dbeta(xSeq,shape1=aPrime,shape2=bPrime,log=FALSE)
yPostConj<-yPostConj/sum(yPostConj)
lines(xSeq,yPostConj,col='red')
par<-parOld

sum(yPostConj)




### Minimizing nll function
kObs<-rbinom(n=10,size=100,prob=0.5)
p<-0.8 # c(0.8) # Initial parameter values 
outNull<-optim(par=p,fn=nllBinom,method="L-BFGS-B",lower=c(0.001),upper=c(0.999),
               k=kObs,nTrials=100)
outNull$par 
outNull$val 






