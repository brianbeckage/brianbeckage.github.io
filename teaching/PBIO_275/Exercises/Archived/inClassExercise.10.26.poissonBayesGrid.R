1. Generate random vector from a poisson with know lambda.
2. Assign a flat prior to this lambda. Plot prior.
3. Calculate and plot likelihood function.
4. Compute and plot posterior on lambda.
5. Insure it sums to 1. What is the mode?

obsData<-rpois(n=20,lambda=1.5)

lPois<-function(parVec,data){
  nllik<- -sum(dpois(x=data,lambda=parVec,log=FALSE))
  cat("nllik= ",nllik,sep=" ",fill=T);cat(" ",sep=" ",fill=T)
  lik<-exp(-nllik)
  return(lik)
}

### Gridded method for calculating Bayesian posterior ##############

#### FLAT PRIOR
parOld<-par(mfrow=c(3,1))
gridLength<-100
xSeq<-seq(from=0.0,to=3.0,len=gridLength)
### Flat prior
yPrior<-rep(1/gridLength,times=gridLength)
sum(yPrior) # 1
plot(xSeq,yPrior,typ='l')

### Likelihood
yLik<-vapply(xSeq,lPois,FUN.VALUE=1,data=obsData)
plot(xSeq,yLik,typ='l')
sum(yLik)

### Posterior
yPost<-yPrior*yLik/(sum(yPrior*yLik))
sum(yPost) # 1
plot(xSeq,yPost,typ='l')
par<-parOld

myMode<-which(yPost==max(yPost))
xSeq[myMode] # estimate of posterior mode


