
library(rethinking)
data("Howell1")
d<-Howell1

plot(weight~height, data=Howell1)


###### Fitting quadratic regression using base lm()

m_lm<-lm(weight~height + I(height^2),data=d)

a<-m_lm$coef[1]
b<-m_lm$coef[2]
b2<-m_lm$coef[3]

height_seq <- seq(from=min(d$height), to=max(d$height), length.out=100)
weight_pred<-a + height_seq*b + height_seq^2*b2

plot(weight~height, data=d)
lines(x=height_seq,y=weight_pred,col='black')


###### Fitting quadratic regression using quap
m_quap <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b * height + b2*height^2,
    a ~ dnorm(0,100),
    b ~ dnorm(0,100),
    b2~dnorm(0,100),
    sigma ~ dexp(1)
  ) , start=list(a=10.0,b=0.0,b2=0.0), data = d )


a<-coef(m_quap)['a']
b<-coef(m_quap)['b']
b2<-coef(m_quap)['b2']

# mean height | x
weight_pred<-a + height_seq*b + height_seq^2*b2

plot(weight~height, data=d)
lines(x=height_seq,y=weight_pred,col='red')

# samples of height | x
# samples <- link(m_quap, data=list(height=height_seq))



###### Fitting quadratic regression using ulam


### making sure cmdstan() is installed and path is found
library(cmdstanr)


#####################################################
# You may not need to run the following two lines if
# the code block starting with 'm_ulam<-ulam('
# below works

install_cmdstan()
options(cmdstanr_path = cmdstanr::cmdstan_path())
####################################################

# Define and fit model
m_ulam <- ulam(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b1*height + b2*height^2,
    a ~ dnorm(0, 20),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data=d, chains=4, cores=4, iter=20000)

# Check convergence
traceplot(m_ulam)
m_ulam_fit<-precis(m_ulam)

a<-m_ulam_fit$mean[1]
b<-m_ulam_fit$mean[2]
b2<-m_ulam_fit$mean[3]

# Plot similar to before

height_seq <- seq(from=min(d$height), to=max(d$height), length.out=100)

# mean height | x
weight_pred<-a + height_seq*b + height_seq^2*b2

plot(weight~height, data=d)
lines(x=height_seq,y=weight_pred,col='blue')



