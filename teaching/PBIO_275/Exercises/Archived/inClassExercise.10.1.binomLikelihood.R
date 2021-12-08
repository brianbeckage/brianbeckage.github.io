### 

k=c(48,51,45,54,48,49,53)
p=seq(from=0.1, to=0.9, length=100)
n=100
k<-rbinom(n=10,size=100,p=0.54)
varyP(p,k,n)

plot(p,varyP(p,k,n))

loglikbinom= function(p,k,n) {
  myNegloglikbinom= - sum(dbinom(k,n,p,log=TRUE))
  # k=c (48,51,45,54,48,49,53)
  # p=seq(from=0.1, to=0.9, length=100)
  # n=100
  return(myNegloglikbinom)
}

varyP<-function(pVect,k,n){
  mynll<-vector()
  j<-0
  for(i in pVect){
    j<-j+1
    mynll[j]<-loglikbinom(i,k,n)
  }
  return(mynll)
}


