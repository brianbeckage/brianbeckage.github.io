
### model by Marina 
### This code estimates model probabilities of a 1,2, or 3 change point model

#simulate data with two change points
x<-seq(from=0, to=200, len=201)
yMean<-ifelse(x<30, 1+2*x, ifelse(x<100, 1+2*30+5*(x-30), 1+2*30+5*(x-30)+10*(x-100)))
y<-sapply(yMean, rnorm, n=1, sd=4)
plot(x,y,pch = 1)
abline(v = 30, col="red")
abline(v = 100, col="red")

N<-length(x)


modelString<-
    "model{
for (i in 1:N){
y[i] ~ dnorm(y.hat[i], tau)

y.hat[i] <- equals(m,1)*y.hat1[i] + equals(m,2)*y.hat2[i]+equals(m,3)*y.hat3[i]

y.hat1[i]<- ifelse(x[i]<changePoint[1], beta[1] + beta[2]*x[i], 
beta[1]+beta[2]*changePoint[1]+ beta[3]*(x[i]-changePoint[1]))

y.hat2[i]<- ifelse(x[i]<changePoint[1], beta[1] + beta[2]*x[i], 
ifelse(x[i]<changePoint[2], 
beta[1]+beta[2]*changePoint[1]+ beta[3]*(x[i]-changePoint[1]), 
beta[1]+beta[2]*changePoint[1]+ beta[3]*(x[i]-changePoint[1])+
beta[4]*(x[i]-changePoint[2])))

y.hat3[i]<- ifelse(x[i]<changePoint[1], beta[1] + beta[2]*x[i], 
ifelse(x[i]<changePoint[2], 
beta[1]+beta[2]*changePoint[1]+ beta[3]*(x[i]-changePoint[1]), 
ifelse(x[i]<changePoint[3],
beta[1]+beta[2]*changePoint[1]+ beta[3]*(x[i]-changePoint[1])+
beta[4]*(x[i]-changePoint[2]),
beta[1]+beta[2]*changePoint[1]+ beta[3]*(x[i]-changePoint[1])+
beta[4]*(x[i]-changePoint[2])+ beta[5]*(x[i]-changePoint[3]))))
}

for(i in 1:3){
changePoint[i] ~ dnorm(0,.0001)
}

for (j in 1:5){
beta[j] ~ dnorm(0, .0001)
}

tau ~ dgamma(0.001,0.001)
sigma<-pow(tau, -0.5)

m ~ dcat(mPrior[])
mPrior[1]<-1/3
mPrior[2]<-1/3
mPrior[3]<-1/3

m1<-equals(m,1) # added by bb: mean of parm gives model prob
m2<-equals(m,2) # added by bb: mean of parm gives model prob
m3<-equals(m,3) # added by bb: mean of parm gives model prob

}
"

# setwd("~/") to appropriate directory

writeLines(modelString, con='TEMPmodel.txt')

dataList<-list(x=x, y=y, N=N)
jags <- jags.model('TEMPmodel.txt',
                   data = dataList,
                   n.chains = 4,
                   n.adapt = 300)

params<-c("m","beta","changePoint","m1","m2","m3")

codaSamples_model_compar<-coda.samples(jags, params, 10000, 2)

save(codaSamples_model_compar, file="change_point_model_comparison.Rdata")

burnin<-1000
summary(window(codaSamples_model_compar,start=burnin))

# yMean<-ifelse(x<30, 1+2*x, ifelse(x<100, 1+2*30+5*(x-30), 1+2*30+5*(x-30)+10*(x-100)))


# 
# Iterations = 10302:20300
# Thinning interval = 2 
# Number of chains = 4 
# Sample size per chain = 5000 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#     
#     Mean       SD Naive SE Time-series SE
# beta[1]        -8.23275  18.3820 0.129980       0.065125
# beta[2]         2.56502   1.0000 0.007071       0.005082
# beta[3]         7.46250   4.3414 0.030698       0.001029
# beta[4]         7.47259  50.9990 0.360617       0.359463
# beta[5]        -0.07407  96.5852 0.682960       0.679164
# changePoint[1] 46.86130  29.4648 0.208348       0.034121
# changePoint[2] 77.67554  66.7169 0.471760       0.551765
# changePoint[3]  6.41646 101.5246 0.717887       2.849439
# m               1.85130   0.5528 0.003909       0.064101
# m1              0.23820   0.4260 0.003012       0.001339
# m2              0.67230   0.4694 0.003319       0.064074
# m3              0.08950   0.2855 0.002019       0.064074
# 
# 2. Quantiles for each variable:
#     
#     2.5%     25%    50%    75%   97.5%
# beta[1]         -43.089 -10.254  1.711  2.971   5.008
# beta[2]           1.816   1.951  2.030  2.770   4.348
# beta[3]           4.914   4.946  4.965  7.515  15.038
# beta[4]        -131.325   9.995 10.030 10.056 128.878
# beta[5]        -193.462 -62.250  0.229 61.657 192.685
# changePoint[1]   28.615  29.554 30.143 49.198  98.339
# changePoint[2] -123.657  99.629 99.785 99.904 159.499
# changePoint[3] -191.092 -61.859  5.955 73.867 214.303
# m                 1.000   2.000  2.000  2.000   3.000
# m1                0.000   0.000  0.000  0.000   1.000
# m2                0.000   0.000  1.000  1.000   1.000
# m3                0.000   0.000  0.000  0.000   1.000
# 

