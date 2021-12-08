#Tstation = T0 - B0*phiStation - lambda*Zstation + Error


#intercept is never given in paper. 
# I used 190 because it gives reasonable values
T0 = 190

#station elevation
#must divide by 1000
z=rlnorm(1000, meanlog = 6, sd=.5) / 1000

#beta is effect of latitude (from Blandford et al 2003)
Beta = -4

#gamma is effect of elevation (from Winter et al 2016)
gamma = -5.385


#phi = latitude range
phi = runif(1000, min = 44, max = 45.5)

#add error
err = rnorm(1000,0,1)

#y is surface temperature at weather station
y = T0 + Beta*phi + gamma*z + err
hist(y)

#make into a data frame
tempdata = as.data.frame(cbind(y, z, phi))
names(tempdata) = c("y", "z", "phi")
phi = tempdata$phi
z=tempdata$z
y = tempdata$y
N = length(y)
write.table(data.frame(Z = z, Y = y, Phi=phi),
            file = 'hcn.data',
            row.names = FALSE,
            col.names = TRUE)

#use lm to see if coefficients can be estimated
mod1 = lm(y ~ phi + z, data = tempdata)


summary(mod1)


#method 1
# do this in jags
modelString<-"
model {
for (i in 1:N){
y[i] ~ dnorm(y.hat[i], tau)
y.hat[i] <- int + beta1 * phi[i] + gamma*z[i]
}

int ~ dnorm(0, .0001)
beta1 ~ dnorm(0, .0001)
gamma ~ dnorm(0,.001)
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)

#predicted values for elevation 500 lat 44.2
#and elevation 1000, lat 45
pred1 = int + beta1 * 44.2 + gamma*0.500
pred2 = int+ beta1 * 45 + gamma*1.000

pred1dist ~ dnorm(pred1, tau)
pred2dist ~ dnorm(pred2, tau)
}
"
writeLines(modelString, con='hcnmodel2.txt')

tempdata2 <- with(tempdata, list(y = y, z = z, phi = phi, N = length(y)))

jags = jags.model('hcnmodel2.txt',
                  data = list('z' = z,
                              'phi' = phi,
                              'y' = y,
                              'N' = N),
                  inits<-list(
                    list('int' = 100, 'beta1' = -4, 'gamma' = 2, 'sigma' = 5),
                    list('int' = 200, 'beta1' = 1, 'gamma' = -1, 'sigma' = 0.5),
                    list('int' = 100, 'beta1' = -.1, 'gamma' = 2, 'sigma' = 7),
                    list('int' = 500, 'beta1' = 2, 'gamma' = 5, 'sigma' = 1)),
                  n.chains = 4,
                  n.adapt = 100)
update(jags, 50000)
codaSamples = coda.samples(jags, c('int','beta1', 'gamma', 'sigma', 'pred1dist', 'pred2dist'), 20000, 1)

plot(codaSamples, trace = FALSE, density = TRUE)
plot(codaSamples)


#this looks pretty good
summary(codaSamples)
traceplot(codaSamples)

############################
#stan model
############################
mystring = "
 data {
   // Define variables in data
   // Number of observations (an integer)
   int<lower=0> N;
   real y[N];
   // Number of parameters
   int<lower=0> p;
//Variables
real<lower=0> z[N];
 real<lower=0> phi[N];
 }
 
parameters {
  // Define parameters to estimate
   real beta[p];
  
  // standard deviation (a positive real number)
   real<lower=0> sigma;
 }
 
 transformed parameters  {
   // Mean
   real mu[N];
   for (i in 1:N) {
     mu[i] <- beta[1] + beta[2]*z[i] + beta[3]*phi[i]; 
   }
 }
 
 model {
   // Prior part of Bayesian inference (flat if unspecified)
 
   // Likelihood part of Bayesian inference
     y~ normal(mu, sigma);  
 }
"

fileName <- "hcnstan.txt"
stan_code <- readChar(fileName, file.info(fileName)$size)
cat(stan_code)
standat <- list(N        = nrow(tempdata),
            p        = 3,
            y        = tempdata2$y,
            z        = tempdata2$z,
            phi      = tempdata2$phi)
resStan <- stan(model_code = stan_code, data = standat,
                chains = 3, iter = 3000, warmup = 500, thin = 10)
print(resStan, "beta")
print(resStan, "sigma")


###########################################
#precip model
###########################################
#reference value for elevation
zref = 150

#chi = 0.000250
chi = 0.000250

#station elevation
zsta=rlnorm(1000, meanlog = 6, sd=.5)

hist(zsta)
###################################
#only for NLS!!
zsta=log(zsta)
###################################

x = zsta - zref

#reference value for precipitation
pref = 2

err = rnorm(1000, 0, .01)

#y is station precipitation
y = pref*((1+chi*(x)) / (1 - chi*(x))) + err


plot(x, y)
dat = as.data.frame(cbind(x,y))

# Run NLS as comparison
# must be run with log transformed values
mymod=nls(y ~ pref*((1+chi*(x)) / (1 - chi*(x))),
    data = dat, start = list(pref = 1, chi=.001), control = list(maxiter=100000))
summary(mymod)

#############################
#jags model
#############################

#precip model #NO LOG TRANSFORM
zref = 150
chi = 0.000250

#station elevation
zsta=rlnorm(1000, meanlog = 6, sd=.5)

x = zsta - zref

#reference value for precipitation
pref = 2

err = rnorm(1000, 0, .01)
y = pref*((1+chi*(x)) / (1 - chi*(x))) + err
n = length(y)

#data for jags model
mydata = list(x = x, y = y, n=n)

jags.script <- "
model{
# likelihood
for( i in 1:n) {
y[i] ~ dnorm(mu[i], tau)
mu[i] <- pref*((1 + chi*(x[i])) / (1 - chi*(x[i])))
}
# priors
pref ~  dunif(0, 10)
chi  ~ dnorm(0, 0.0001)
tau  ~ dgamma(0.001, 0.001)
sigma <- 1 / sqrt(tau)
}
" 


mod1 = jags.model(textConnection(jags.script), data = mydata, n.chains = 4, 
                   n.adapt = 100)
update(mod1, 40000)
mod1.samples <- coda.samples(model = mod1, variable.names = c("pref", "chi", "sigma"), n.iter = 4000)
plot(mod1.samples)  # plot trace and posterior density for each parameter

#means of posterior density
summary(mod1.samples)

#high density interval
HPDinterval(mod1.samples)
densplot(mod1.samples)

#gelman rubin statistic
gelman.diag(mod1.samples)

#stan model for precip(not run here)
#Also, this does not work.
#I am still working on getting estimates from stan.

"
data {
  int <lower=1> N;
  vector [N] y; 
  vector [N] x;
}
parameters {
  real <lower=0> sigma;
  real chi;
  real <lower=0> pref;
}
model {
  vector [N] denom;
  for (i in 1:N) denom[i] = 1 / (1 - chi*x[i]);
  y ~ normal(pref * (1 + chi*x) .* denom, sigma);
  sigma ~ normal(0, 10);
  chi ~ normal(0, .0001);
  pref ~ uniform(0, 10);
}
"