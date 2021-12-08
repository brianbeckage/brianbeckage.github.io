### A LINEAR REGRESSION EXAMPLE ####

### Based on package MHadpative
see http://www.inside-r.org/packages/cran/MHadaptive/docs/Metro_Hastings

### simulated data from a linear model

nReps<-10
x<-seq(from=0,to=100,by=1)
x<-rep(x,nReps)
b0<-1.0; b1<-0.2
ymean<-b0+b1*x
y<-rnorm(n=length(ymean),mean=ymean,sd=1.5)
data<-data.frame(x,y)
plot(x,y)

# alternative way of simulating data
# x<-runif(30,5,15)
# y<-x+rnorm(30,0,5)
# d<-cbind(x,y)


## Calculate the log likelihood + log prior
li_reg<-function(pars,data)
{
    a<-pars[1]      #intercept
    b<-pars[2]      #slope
    sd_e<-pars[3]   #error (residuals)
    if(sd_e<=0){return(NaN)}
    pred <- a + b * data[,1]
    log_likelihood<-sum( dnorm(data[,2],pred,sd_e, log=TRUE) )
    prior<- prior_reg(pars)
    return(log_likelihood + prior)
}

## Define the Prior distributions
prior_reg<-function(pars)
{
  a<-pars[1]          #intercept
  b<-pars[2]          #slope  
  sd<-pars[3]    #error
  
  prior_a<-dnorm(a,0,100,log=TRUE)     ## non-informative (flat) priors on all 
  prior_b<-dnorm(b,0,100,log=TRUE)     ## parameters.  
  prior_sd<-dgamma(sd,1,1/100,log=TRUE)      
  
  return(prior_a + prior_b + prior_sd)
}

pars<-c(0.5,1,2)
li_reg(pars,data) # [1] -267316
prior_reg(pars) # [1] -15.67345

library(MHadaptive)
mcmc_r<-Metro_Hastings(li_func=li_reg,pars=c(0,1,1),
                       par_names=c('a','b','sd'),data=data)
plotMH(mcmc_r)

##  For best results, run again with the previously 
##  adapted variance-covariance matrix.

mcmc_r<-Metro_Hastings(li_func=li_reg,pars=c(0,1,1),
                       prop_sigma=mcmc_r$prop_sigma,par_names=c('a','b','epsilon'),data=d)

mcmc_r<-mcmc_thin(mcmc_r)
plotMH(mcmc_r)


########################################################
#### Creating a simple sampler for a linear regression


## Define a log likelihood function
li_reg_bb<-function(pars,data)
{
    a<-pars[1]      #intercept
    b<-pars[2]      #slope
    sd_e<-pars[3]   #error (residuals)
    if(sd_e<=0){return(NaN)}
    pred <- a + b * data[,1]
    log_likelihood<-sum( dnorm(data[,2],pred,sd_e, log=TRUE) )
    # prior<- prior_reg(pars)
    return(log_likelihood)
}

## Calculate the log prior
prior_reg<-function(pars)
{
    a<-pars[1]          #intercept
    b<-pars[2]          #slope  
    sd<-pars[3]    #error
    
    prior_a<-dnorm(a,0,100,log=TRUE)     ## non-informative (flat) priors on all 
    prior_b<-dnorm(b,0,100,log=TRUE)     ## parameters.  
    prior_sd<-dgamma(sd,1,1/100,log=TRUE)      
    
    return(prior_a + prior_b + prior_sd)
}


myDebug<-FALSE
parmVect<-c(0.5,1,2)
nIters<-10000
sdSampler<-0.5
iterMat<-matrix(NA,nrow=nIters+1,ncol=3)
colnames(iterMat)<-c("b0","b1","sd")
iterMat[1,]<-parmVect

for(i in 1:nIters){
    for(j in seq_along(parmVect)){
        if(myDebug) cat("i= ",i," ; j= ",j,fill=TRUE)
        if(myDebug) cat("Begin loop: parmVect= ",parmVect,fill=TRUE)
        
        logLikCurrent<-li_reg_bb(parmVect,data)
        logPriorCurrent<-prior_reg(parmVect) 
        if(myDebug) cat("logLikCurrent= ",logLikCurrent," ; logPriorCurrent= ",logPriorCurrent,fill=TRUE)
        if(j<3){
            #parmStar<-normStar(parmVect[i],1)
            parmStar<-rnorm(n=1,mean=parmVect[j],sd=sdSampler)
            logThetaStar<-dnorm(parmStar,mean=parmVect[j],sd=sdSampler,log=TRUE)
            logTheta<-dnorm(parmVect[j],mean=parmStar,sd=sdSampler,log=TRUE)
            if(myDebug)  {cat("parmStar for b0,b1 = ",parmStar,"logTheta= ",logTheta," ;
                logThetaStar= ",logThetaStar,fill=TRUE)}
        } else{
            parmStar<-rgamma(n=1,shape=2)
            logThetaStar<-dgamma(parmStar,shape=2,log=TRUE)
            logTheta<-dgamma(parmVect[j],shape=2,log=TRUE)
            if(myDebug) {cat("parmStar for sd = ",parmStar,"logTheta= ",logTheta," ;
                logThetaStar= ",logThetaStar,fill=TRUE)}
        }
        
        parmVectStar<-parmVect
        parmVectStar[j]<-parmStar
        if(myDebug) cat("parmVectStar = ",parmVectStar,fill=TRUE)
        logLikStar<-li_reg_bb(parmVectStar,data)
        logPriorStar<-prior_reg(parmVectStar) 
        if(myDebug) cat("logLikStar= ",logLikStar," ; logPriorStar= ",logPriorStar,fill=TRUE)
        if(myDebug) cat("logTheta= ",logTheta," ; logThetaStar= ",logThetaStar,fill=TRUE)
        q<-exp(logLikStar+logPriorStar+logTheta
               -logLikCurrent-logPriorCurrent-logThetaStar)
        r=min(1, q)
        if(myDebug) cat("r= ",r," ; q= ",q,fill=TRUE)
        if(runif(n=1)<r) parmVect<-parmVectStar
    }
    iterMat[i+1,]<-parmVect
}

matplot(iterMat)
plot(iterMat[,1])
plot(iterMat[,2])
plot(iterMat[,3])
hist(iterMat[,1],freq=FALSE)
hist(iterMat[,2],freq=FALSE)
hist(iterMat[,3],freq=FALSE)
apply(iterMat,2,mean)
# b0        b1        sd 
# 1.0168434 0.1997594 1.4677739 

plot(iterMat[,1],iterMat[,2])


### Gamma distribution shape
x<-seq(0,20,len=100)
y<-dgamma(x, shape=2, log = FALSE)
plot(x,y,type="l")



