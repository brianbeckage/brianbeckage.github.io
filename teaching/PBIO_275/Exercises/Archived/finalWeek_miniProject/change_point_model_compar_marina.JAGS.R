#Marina Golivets
#12/2/2015
#Change-point regression -- model comparison

####################################

#packages
library('rjags')

####################################

#simulate data with two change points
x<-seq(from=0, to=200, len=201)
yMean<-ifelse(x<30, 1+2*x, ifelse(x<100, 1+2*30+5*(x-30), 1+2*30+5*(x-30)+10*(x-100)))
y<-sapply(yMean, rnorm, n=1, sd=4)
plot(x,y,pch = 1)
abline(v = 30, col="red")
abline(v = 100, col="red")

N<-length(x)

####################################

# model 1 with one change point 

modelString<-
    "model{
for (i in 1:N){
y[i] ~ dnorm(y.hat[i], tau)
y.hat[i]<- ifelse(x[i]<changePoint, beta[1] + beta[2]*x[i], beta[1] + beta[2]*changePoint 
+ beta[3]*(x[i]-changePoint))
}

changePoint ~ dnorm(0, .0001)
for(i in 1:3){
beta[i] ~ dnorm(0, .0001)
}
tau ~ dgamma(0.001,0.001)
sigma<-pow(tau, -0.5)
}
"

writeLines(modelString, con='TEMPmodel.txt')

dataList<-list(x=x, y=y, N=N)
jags1 <- jags.model('TEMPmodel.txt',
                   data = dataList,
                   n.chains = 4,
                   n.adapt = 100)

update(jags1,2000)
codaSamples_model1<-coda.samples(jags1, c('beta',
                                         'changePoint','sigma'), 10000, 1)

burnin<-2000
summary(window(codaSamples_model1,start=burnin))

# Mean      SD Naive SE Time-series SE
# beta[1]     -36.63 30.1376 0.150688         3.6962
# beta[2]       3.94  8.0002 0.040001         0.3853
# beta[3]      14.74  0.9052 0.004526         0.2130
# changePoint  93.97 15.8875 0.079437         3.5894
# sigma        18.73 24.2562 0.121281         5.2982

#####################################

#model 2 with two change points

modelString<-
    "model{
for (i in 1:N){
y[i] ~ dnorm(y.hat[i], tau)
y.hat[i]<- ifelse(x[i]<changePoint[1], beta[1] + beta[2]*x[i], ifelse(x[i]<changePoint[2], 
beta[1]+beta[2]*changePoint[1]+ beta[3]*(x[i]-changePoint[1]), 
beta[1]+beta[2]*changePoint[1]+beta[3]*(x[i]-changePoint[1])+beta[4]*(x[i]-changePoint[2])))
}

for(i in 1:2){
changePoint[i] ~ dnorm(0, .0001)
}

for(i in 1:4){
beta[i] ~ dnorm(0, .0001)
}

tau ~ dgamma(0.001,0.001)
sigma<-pow(tau, -0.5)
}
"

writeLines(modelString, con='TEMPmodel.txt')

dataList<-list(x=x, y=y, N=N)
jags2 <- jags.model('TEMPmodel.txt',
                   data = dataList,
                   n.chains = 4,
                   n.adapt = 100)

update(jags2,1000)
codaSamples_model2<-coda.samples(jags2, c('beta',
                                         'changePoint','sigma'), 10000, 1)
burnin<-1000
summary(window(codaSamples_model2,start=burnin))

# Mean      SD Naive SE Time-series SE
# beta[1]        -13.764 41.4799 0.207400        13.3451
# beta[2]          3.252 26.0421 0.130210         3.3181
# beta[3]          4.598  0.7790 0.003895         0.2811
# beta[4]         10.381  0.7755 0.003878         0.2751
# changePoint[1]  30.242 26.6256 0.133128        10.2852
# changePoint[2]  99.240  1.0211 0.005105         0.2119
# sigma            6.859  4.2089 0.021044         2.2025


##################################

#model 3 with three change points

modelString<-
    "model{
for (i in 1:N){
y[i] ~ dnorm(y.hat[i], tau)
y.hat[i]<- ifelse(x[i]<changePoint[1], beta[1] + beta[2]*x[i], 
ifelse(x[i]<changePoint[2], 
beta[1]+beta[2]*changePoint[1]+ beta[3]*(x[i]-changePoint[1]), 
ifelse(x[i]<changePoint[3],
beta[1]+beta[2]*changePoint[1]+ beta[3]*(x[i]-changePoint[1])+
beta[4]*(x[i]-changePoint[2]),
beta[1]+beta[2]*changePoint[1]+ beta[3]*(x[i]-changePoint[1])+
beta[4]*(x[i]-changePoint[2])+ beta[5]*(x[i]-changePoint[3]))))
}

for(i in 1:3){
changePoint[i] ~ dnorm(0, .0001)
}

for(i in 1:5){
beta[i] ~ dnorm(0, .0001)
}

tau ~ dgamma(0.001,0.001)
sigma<-pow(tau, -0.5)
}
"

writeLines(modelString, con='TEMPmodel.txt')

dataList<-list(x=x, y=y, N=N)
jags3 <- jags.model('TEMPmodel.txt',
                   data = dataList,
                   n.chains = 4,
                   n.adapt = 100)

update(jags3,1000)
codaSamples_model3<-coda.samples(jags3, c('beta',
                                         'changePoint','sigma'), 10000, 1)

burnin<-1000
summary(window(codaSamples_model3,start=burnin)) 

# Mean      SD Naive SE Time-series SE
# beta[1]        -12.9416 50.2840 0.251420       11.75326
# beta[2]          1.4540  7.2261 0.036131        0.66569
# beta[3]          3.2970  1.0634 0.005317        0.19563
# beta[4]          0.4558  2.7232 0.013616        0.23341
# beta[5]         11.2274  1.7239 0.008619        0.09874
# changePoint[1]  -1.8910 32.7562 0.163781        2.57891
# changePoint[2]  13.2374 82.1443 0.410722        5.32252
# changePoint[3]  99.2446  0.9621 0.004810        0.11012
# sigma            6.7119  4.1433 0.020716        0.78587


#################################
#model comparison using Deviance Information Criterion (DIC)
dic.pD.jags1<-dic.samples(jags1, 10000, "pD") #
dic.pD.jags2<-dic.samples(jags2, 10000, "pD") #Deviance Information Criterion
dic.pD.jags3<-dic.samples(jags3, 10000, "pD") #Deviance Information Criterion
diffdic(dic.pD.jags1,dic.pD.jags2)
# Difference: 3168.488
# Sample standard error: 123.6519
diffdic(dic.pD.jags2,dic.pD.jags3)
# Difference: -87.37518
# Sample standard error: 9.942755

##############################

#model comparison within one model

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
}
"
writeLines(modelString, con='TEMPmodel.txt')

dataList<-list(x=x, y=y, N=N)
jags <- jags.model('TEMPmodel.txt',
                   data = dataList,
                   n.chains = 4,
                   n.adapt = 300)

params<-c("m","beta","changePoint")

codaSamples_model_compar<-coda.samples(jags, params, 10000, 2)

save(codaSamples_model_compar, file="change_point_model_comparison.Rdata")

burnin<-1000
summary(window(codaSamples_model_compar,start=burnin)) 
# Iterations = 2000:100300
# Thinning interval = 10 
# Number of chains = 4 
# Sample size per chain = 9831 
# 
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#     
#     Mean       SD Naive SE Time-series SE
# beta[1]        -12.574476  33.8015 0.170454        7.58253
# beta[2]          2.745535   1.8841 0.009501        0.05039
# beta[3]          7.470652   4.3455 0.021914        0.04842
# beta[4]          7.425040  49.6560 0.250405        0.25430
# beta[5]         -0.007331 100.2093 0.505335        0.50404
# changePoint[1]  41.824037  38.6918 0.195114        6.57517
# changePoint[2]  77.028776  67.3494 0.339629        0.31166
# changePoint[3]   4.670043 103.6020 0.522444        0.52232
# m                1.781228   0.4609 0.002324        0.00107
# 
# 2. Quantiles for each variable:
#     
#                  2.5%     25%      50%       75%   97.5%
# beta[1]         -67.575 -39.711 -1.26630  -0.01273  27.007
# beta[2]           1.891   2.022  2.08994   4.28308   5.837
# beta[3]           4.319   5.013  5.03532   7.54064  15.047
# beta[4]        -126.402   9.951  9.97862  10.00993 127.216
# beta[5]        -197.853 -67.588  0.07054  67.13185 197.161
# changePoint[1]  -50.424  30.159 30.63900  48.54369  98.373
# changePoint[2] -126.152  98.168 99.93795 100.06727 157.784
# changePoint[3] -196.005 -65.223  3.29723  71.80531 218.326
# m                 1.000   2.000  2.00000   2.00000   2.000

# From Kruschke
#convert coda-object codaSamples to matrix object for easier handling.
mcmcMat<- as.matrix(codaSamples_model_compar, chains=TRUE)
m <- mcmcMat[,"m"]
# compute the proportion of m at each index value:
M1 <- sum( m == 1 ) / length( m ) #[1] 0.2395
M2 <- sum( m == 2 ) / length( m ) #[1] 0.739575
M2 <- sum( m == 3 ) / length( m ) #[1] 0.020925
# extract parameters' values for each model index:
for (m in 1:3){
    print(colMeans(subset(mcmcMat,mcmcMat[,"m"]==m)))
}
# CHAIN        beta[1]        beta[2]        beta[3]        beta[4]        beta[5] 
# 3.0000000    -42.7494028      4.3280965     14.9911436     -0.6660257     -0.8931862 
# changePoint[1] changePoint[2] changePoint[3]     m 
# 97.9131620     -1.1493318      0.4928745      1.0000000 
# 
# CHAIN        beta[1]        beta[2]        beta[3]        beta[4]        beta[5] 
# 2.3385052     -2.6111162      2.2518791      5.0341954      9.9824465      0.1550869 
# changePoint[1] changePoint[2] changePoint[3]     m 
# 23.3757944    100.7383725      1.2779571      2.0000000 
# 
# CHAIN        beta[1]        beta[2]        beta[3]        beta[4]        beta[5] 
# 2.485066     -12.753962       2.783670       7.439779       8.316966       3.500231 
# changePoint[1] changePoint[2] changePoint[3]     m 
# 40.969743     133.978313     173.301596       3.000000 

