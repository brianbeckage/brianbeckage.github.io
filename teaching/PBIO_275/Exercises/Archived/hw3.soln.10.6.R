
#### Fitting a logistic regression using maximum likelihood ############################

### Objective: Fit a logistic linear regression using maximum likelihood to simulated
### seed predation data.

### Generating simulated data
# logit(p)=log(p/(1-p))
# logit^-1=exp(p)/(1+exp(p))
b0<-0.5; b1<-1.5
gap<-1;p.gap<-exp(b0+b1*gap)/(1+exp(b0+b1*gap))
gap<-0;p.can<-exp(b0+b1*gap)/(1+exp(b0+b1*gap))

sdlg.can<-rbinom(n=10,size=20,p=p.can)
# 13 11 11 11 15  7 13  7 15 12
sdlg.gap<-rbinom(n=10,size=20,p=p.gap)
# 16 19 17 18 17 18 20 17 17 17


# Placing data into a matrix
sdlgMat<-cbind(c(sdlg.gap,sdlg.can),c(rep(20,10)),c(rep(1,10),rep(0,10)))
colnames(sdlgMat)<-c('surv','init','gap')
                    
# sdlgDF<-as.data.frame(sdlgMat) # Here its in a data frame

### Our function to minimize
nllLogReg<-function(parVec,sdlgDat){
  B0<-parVec[1];
  B1<-parVec[2];
  lm<-B0 + B1*sdlgDat[,3]
  m<-exp(lm)/(1+exp(lm))
  nPlanted<-sdlgDat[,2]
  nSurv<-sdlgDat[,1]
  nllik<- -sum(dbinom(nSurv,nPlanted,m,log=TRUE))
  #nllik<- -(sum( (dbinom(x=nSurvive, size=nPlanted, prob=pSurv,log=TRUE)) ,na.rm=T))
  cat("nllik= ",nllik,sep=" ",fill=T);cat(" ",sep=" ",fill=T)
  return(nllik)
}

### Minimizing our function
parVec<-c(1,0.1) # Initial parameter values 
outLogReg<-optim(par=parVec,fn=nllLogReg,method="L-BFGS-B",lower=c(-Inf,-Inf),upper=c(Inf,Inf),sdlgDat=sdlgMat)
outLogReg$par # 

### Examining extimated probabilities of seedling survival beneath canopy and in gaps:
exp(outLogReg$par[1])/(1+exp(outLogReg$par[1])) 
p.can

exp(outLogReg$par[1]+outLogReg$par[2])/(1+exp(outLogReg$par[1]+outLogReg$par[2])) 
p.gap


# AIC 
AIC = 2*k - 2*ln(L) where L is the maximized likelihood.



# ln(lambda)<-b0+b1*gap
b0<-1.2; b1<-10
gap<-1; lambda.gap<-exp(b0+b1*gap)

