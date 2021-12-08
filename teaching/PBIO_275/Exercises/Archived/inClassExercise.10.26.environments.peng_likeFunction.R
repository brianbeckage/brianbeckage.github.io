make.NegLogLik<-function(data,fixed=c(FALSE,FALSE)){
    params<-fixed
    
    function(p){
        params[!fixed]<-p
        mu<-params[1]
        sigma<-params[2]
        a<- -0.5*length(data)*log(2*pi*sigma^2)
        b<- -0.5*sum((data-mu)^2)/(sigma^2)
        -(a+b)
    }
    
}

make.NegLogLik.bb<-function(data,fixed=c(FALSE,FALSE)){
    params<-fixed
    
    function(p){
        params[!fixed]<-p
        mu<-params[1]
        sigma<-params[2]
        nll<- -sum(dnorm(data,mean=mu,sd=sigma,log=TRUE),na.rm=TRUE)
       return(nll)
    }
    
}



normalData<-rnorm(100,mean=1,sd=2)

nLL<-make.NegLogLik(normalData)
nLL(c(0,1))
nLL(c(1,2))
optim(c(0,1),nLL)$par

nLLbb<-make.NegLogLik.bb(normalData)
nLLbb(c(0,1)) # 320.53


nLL<-make.NegLogLik(normalData,c(FALSE,2))
nLL(1)
nLL(2)
optim(0,nLL,method="Brent",lower=-10,upper=10)$par

