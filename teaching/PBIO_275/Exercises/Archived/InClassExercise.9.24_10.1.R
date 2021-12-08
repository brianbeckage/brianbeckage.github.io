#### LAB 5 ####################################################################
## Fitting a logistic regression using maximum likelihood.
### Example where we fit a logistic linear regression using maximum likelihood to the seed predation data used in last lab.
### First, let's place the data into a matrix.
sdlg.can<-c(4, 5, 8, 3, 6, 3, 5, 4, 4, 5)
sdlg.gap<-c(8, 8, 8, 9, 5, 8, 5, 12, 10, 6)

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
outLogReg$par # -1.1802904 0.7539478

### Examining extimated probabilities of seedling survival beneath canopy and in gaps:
exp(outLogReg$par[1])/(1+exp(outLogReg$par[1])) # 0.235 for survival probability beneath canopy which is identical to our estimate from last lab. 
exp(outLogReg$par[1]+outLogReg$par[2])/(1+exp(outLogReg$par[1]+outLogReg$par[2])) # 0.395 for survival probability in gaps which is identical to our estimate from last lab.


# AIC 
AIC = 2*k - 2*ln(L) where L is the maximized likelihood.

logit(p)=log(p/(1-p))
logit^-1=exp(p)/(1+exp(p))



