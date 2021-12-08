




#Marina Golivets #### original code below

#change-point regression
x<-seq(from=0, to=100, len=100)
yMean<-ifelse(x<50, 1+2*x, 1+2*50+4*(x-50))
y<-sapply(yMean, rnorm, n=1, sd=4)
plot(x,y)
N<-length(x)

modelString<-
    "model{
for (i in 1:N){
y[i] ~ dnorm(y.hat[i], tau)
y.hat[i]<- ifelse(i<changePoint, a + b1*x[i], a + b1*changePoint + b2*(x[i]-changePoint))
}
changePoint ~ dnorm(0, .0001)
a ~ dnorm(0, .0001)
b1 ~ dnorm(0, .0001)
b2 ~ dnorm(0, .0001)
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)
}
"
writeLines(modelString, con="TEMPmodel.txt")

dataList<-list(x=x, y=y, N=N)
jags<-jags.model("TEMPmodel.txt",
                 data = dataList,
                 n.chains = 4, 
                 n.adapt = 200)
codaSamples<-coda.samples(jags, c('a','b1','b2','changePoint','sigma'), 10000, 1)
summary(codaSamples)
