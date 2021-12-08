par(mfrow = c(3,2),mar = c(.75, 3, 2, 0), mgp = c(2, 1, 0), oma=c(6,1,0,1), new=FALSE)
###Total Carbon Storage (MCMC metropolis)
#Read in
d2015<-read.csv(file="~/Desktop/GradProjectAnalysis/d2015a.csv", sep=",", head=TRUE)
d2016<-read.csv(file="~/Desktop/GradProjectAnalysis/d2016a.csv", sep=",", head=TRUE)
both<-read.csv(file="fifteenandsixteen.csv", sep=",", head=TRUE)
#P2016 data
x<-d2016$FG
y<-d2016$Cstorage
sixteenmod<-lm(y~x)
summary(sixteenmod)
anova(sixteenmod)
#linear model of 2015 to get alpha and beta prior, and standard deviation
x1<-d2015$FG
y1<-d2015$TC
fifteenmod<-lm(y1~x1)
summary(fifteenmod)
anova(fifteenmod)
#likelihood function
likelihood <- function(param){
	a = param[1]
	b = param[2]
	sd = param[3]
	
	pred = a*x + b
	singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
	sumll = sum(singlelikelihoods)
	return(sumll)   
}
#prior function
prior <- function(param){
	a = param[1]
	b = param[2]
	sd = param[3]
	aprior = dnorm(a, mean=267.34, sd=(76.81/sqrt(33)), log = T)
	bprior = dnorm(b, mean=58.70, sd=(40.62/sqrt(33)), log = T)
	sdprior = dinvgamma(sd, shape=0.001,rate=0.001, log = T)
	return(aprior+bprior+sdprior)
}
#posterior function
posterior <- function(param){
	return (likelihood(param) + prior(param))
}
#Metropolis sampler (modified code from Florian Hartig, 2010)
proposalfunction <- function(param){
	return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

met_MCMC <- function(startvalue, iterations){
	chain = array(dim = c(iterations+1,3))
	chain[1,] = startvalue
	for (i in 1:iterations){
		proposal = proposalfunction(chain[i,])
		
		probab = exp(posterior(proposal) - posterior(chain[i,]))
		if (runif(1) < probab){
			chain[i+1,] = proposal
		}else{
			chain[i+1,] = chain[i,]
		}
	}
	return(chain)
}

#10,000 samples for each model parameter
library(invgamma)
startvalue = c(0,10,1.5)
chain = met_MCMC(startvalue, 10000)
#create coda object
library(coda)
chain<-mcmc(chain)
burn = 1000
chain<-mcmc(chain[-(1:burn),])
#Report model parameter estimates
summary(chain)
#Estimate 95% HDP
HPDinterval(chain, prob=.95)
#plot fit superimposed on data
plot(x, y, ylab="Stored Carbon",xlab="",main="Metropolis Sampler",xaxt="n")
axis(1,at=1:3,labels=1:3)
MLE2016<-abline(lm(y~x))												    #MLE (slope and intercept) from 2016
Bayesian<-abline(a=30.37, b=70.48, col="red")      #MCMC sampling the posterior (slope and intercept)
MLE2015<-abline(lm(d2015$TC~d2015$FG),col="blue")    	#MLE (slope and intercept) from 2015 Carbon storage
library(lme4)
fit <- lmer(both$Cstorage ~ both$FG + (both$Year||both$Mesocosm.ID))
abline(a=fit@beta[1], b=fit@beta[2], col="green", lty=2) #random effects from year and mesocosm ID

			

###Total Carbon Storage (Gibbs)
#install.packages("bayesm")
library(bayesm)
#response
yy1<-d2016$Cstorage
#treatment
xx1<-cbind(matrix(1,nrow(d2016),1),d2016$FG)
# data for the Bayesian analysis
dtdt1 <- list(yy=yy1,XX=xx1)


# mean of the normal prior. We have 2 estimates
# and 1 regression coefficients
mod2015<-lm(d2015$TC~d2015$FG)

betabar1 <- c(mod2015$coefficients[1],mod2015$coefficients[2])

# Pecision matrix for the normal prior.
A1 <- 0.01 * diag(2)
# note this is a very diffuse prior

# degrees of freedom for the inverse chi-square prior
n1 <- 2

# scale parameter for the inverse chi-square prior
ssq1 <- var(yy1, na.rm=TRUE) 

Prior1 <- list(betabar=betabar1, A=A1, nu=n1, ssq=ssq1)

# number of iterations of the Gibbs sampler
iter <- 10000  

# thinning/slicing parameter.
slice <- 1 

MCMC <- list(R=iter, keep=slice)

sim1 <- runiregGibbs(dtdt1, Prior1, MCMC)


# compare with maximum likelihood estimates:
fitcstorage <- lm(d2016$Cstorage~d2016$FG)
fitcstorage15 <- lm(d2015$TC~d2015$FG)
summary(fitcstorage)
plot(d2016$FG, d2016$Cstorage, ylab="",xlab="",main="Gibbs Sampler",xaxt="n")
axis(1,at=1:3,labels=1:3)
abline(fitcstorage)                                                    #MLE (slope and intercept) from 2016
abline(a=mean(sim1$betadraw[,1]), b=mean(sim1$betadraw[,2]), col="red")#MCMC sampling the posterior (slope and intercept)
quantile(sim1$betadraw[,2],c(.025,.975))
quantile(sim1$betadraw[,1],c(.025,.975))

abline(fitcstorage15, col="blue")                           #MLE (slope and intercept) from 2015 Carbon storage
fit <- lmer(both$Cstorage ~ both$FG + (both$Year||both$Mesocosm.ID))
summary(fit)
abline(a=fit@beta[1], b=fit@beta[2], col="green", lty=2) #random effects from year and mesocosm ID






###Total Nitrogen Storage (MCMC)
#Read in
#P2016 data
x<-d2016$FG
y<-d2016$Nstorage
sixteenmod<-lm(y~x)
summary(sixteenmod)
anova(sixteenmod)
#linear model of 2015 to get alpha and beta prior, and standard deviation
x1<-d2015$FG
y1<-d2015$TN
fifteenmod<-lm(y1~x1)
summary(fifteenmod)
anova(fifteenmod)
#likelihood function
likelihood <- function(param){
	a = param[1]
	b = param[2]
	sd = param[3]
	
	pred = a*x + b
	singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
	sumll = sum(singlelikelihoods)
	return(sumll)   
}
#prior function
prior <- function(param){
	a = param[1]
	b = param[2]
	sd = param[3]
	aprior = dnorm(a, mean=3.8968, sd=(1.4831/sqrt(33)), log = T)
	bprior = dnorm(b, mean=1.7923, sd=(0.7843/sqrt(33)), log = T)
	sdprior = dinvgamma(sd, shape=0.001,rate=0.001, log = T)
	return(aprior+bprior+sdprior)
}
#posterior function
posterior <- function(param){
	return (likelihood(param) + prior(param))
}
#Metropolis sampler (modified code from Florian Hartig, 2010)
proposalfunction <- function(param){
	return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

met_MCMC <- function(startvalue, iterations){
	chain = array(dim = c(iterations+1,3))
	chain[1,] = startvalue
	for (i in 1:iterations){
		proposal = proposalfunction(chain[i,])
		
		probab = exp(posterior(proposal) - posterior(chain[i,]))
		if (runif(1) < probab){
			chain[i+1,] = proposal
		}else{
			chain[i+1,] = chain[i,]
		}
	}
	return(chain)
}

#10,000 samples for each model parameter
library(invgamma)
startvalue = c(0,10,1.5)
chain = met_MCMC(startvalue, 10000)
#create coda object
library(coda)
chain<-mcmc(chain)
burn = 1000
chain<-mcmc(chain[-(1:burn),])
#Report model parameter estimates
summary(chain)
#Estimate 95% HDP
HPDinterval(chain, prob=.95)
#plot fit superimposed on data
plot(x, y, ylab="Stored Nitrogen",xlab="",main="",xaxt="n")
axis(1,at=1:3,labels=1:3)
MLE2016<-abline(lm(y~x))												    #MLE (slope and intercept) from 2016
Bayesian<-abline(a=3.178, b=1.703, col="red")      #MCMC sampling the posterior (slope and intercept)
MLE2015<-abline(lm(d2015$TN~d2015$FG),col="blue")    	#MLE (slope and intercept) from 2015 Carbon storage

library(lme4)
fit <- lmer(both$Nstroage ~ both$FG + (both$Year||both$Mesocosm.ID))
abline(a=fit@beta[1], b=fit@beta[2], col="green", lty=2) #random effects from year and mesocosm ID


###Total Nitrogen Storage (Gibbs)
#response
yy1<-d2016$Nstorage
#treatment
xx1<-cbind(matrix(1,nrow(d2016),1),d2016$FG)
#first run a one-way anova
summary(aov(yy1~xx1[,2]))
# data for the Bayesian analysis
dtdt1 <- list(yy=yy1,XX=xx1)


# mean of the normal prior.
mod2015<-lm(d2015$TN~d2015$FG)

betabar1 <- c(mod2015$coefficients[1],mod2015$coefficients[2])

# Pecision matrix for the normal prior.
A1 <- 0.01 * diag(2)
# note this is a very diffuse prior

# degrees of freedom for the inverse chi-square prior
n1 <- 2

# scale parameter for the inverse chi-square prior
ssq1 <- var(yy1, na.rm=TRUE) 

Prior1 <- list(betabar=betabar1, A=A1, nu=n1, ssq=ssq1)

# number of iterations of the Gibbs sampler
iter <- 10000  

# thinning/slicing parameter. 1 means we keep all all values
slice <- 1 

MCMC <- list(R=iter, keep=slice)

sim1 <- runiregGibbs(dtdt1, Prior1, MCMC)



summary(sim1$betadraw)
summary(sim1$sigmasqdraw)
sim2<-mcmc(c(sim1$betadraw[,1],sim1$betadraw[,2]))
HPDinterval(sim2, prob=.95)
# compare with maximum likelihood estimates:
fitcstorage <- lm(d2016$Nstorage~d2016$FG)
fitcstorage15 <- lm(d2015$TN~d2015$FG)
summary(fitcstorage)
plot(d2016$FG, d2016$Nstorage, ylab="",xlab="",xaxt="n")
axis(1,at=1:3,labels=1:3)
abline(fitcstorage)                                                    #MLE (slope and intercept) from 2016
abline(a=mean(sim1$betadraw[,1]), b=mean(sim1$betadraw[,2]), col="red")#MCMC sampling the posterior (slope and intercept)
quantile(sim1$betadraw[,2],c(.025,.975))
quantile(sim1$betadraw[,1],c(.025,.975))
abline(fitcstorage15, col="blue")                            #MLE (slope and intercept) from 2015 Carbon storage
both<-read.csv(file="fifteenandsixteen.csv", sep=",", head=TRUE)
library(lme4)
fit <- lmer(both$Nstroage ~ both$FG + (both$Year||both$Mesocosm.ID))
summary(fit)
abline(a=fit@beta[1], b=fit@beta[2], col="green", lty=2) #random effects from year and mesocosm ID





###Total Aboveground Biomass (MCMC)
#Read in

#P2016 data
x<-d2016$FG
y<-d2016$AGB
sixteenmod<-lm(y~x)
summary(sixteenmod)
anova(sixteenmod)
#plot(x,y, main="Original Data")

#linear model of 2015 to get alpha and beta prior, and standard deviation
x1<-d2015$FG
y1<-d2015$AboveBiomass
fifteenmod<-lm(y1~x1)
summary(fifteenmod)
anova(fifteenmod)


#likelihood function
likelihood <- function(param){
	a = param[1]
	b = param[2]
	sd = param[3]
	
	pred = a*x + b
	singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
	sumll = sum(singlelikelihoods)
	return(sumll)   
}



#prior function
prior <- function(param){
	a = param[1]
	b = param[2]
	sd = param[3]
	aprior = dnorm(a, mean=279.64, sd=(86.34/sqrt(33)), log = T)
	bprior = dnorm(b, mean=17.55, sd=(45.66/sqrt(33)), log = T)
	sdprior = dinvgamma(sd, shape=0.001,rate=0.001, log = T)
	return(aprior+bprior+sdprior)
}

#posterior function
posterior <- function(param){
	return (likelihood(param) + prior(param))
}

#Metropolis sampler (modified code from Florian Hartig, 2010)
proposalfunction <- function(param){
	return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

met_MCMC <- function(startvalue, iterations){
	chain = array(dim = c(iterations+1,3))
	chain[1,] = startvalue
	for (i in 1:iterations){
		proposal = proposalfunction(chain[i,])
		
		probab = exp(posterior(proposal) - posterior(chain[i,]))
		if (runif(1) < probab){
			chain[i+1,] = proposal
		}else{
			chain[i+1,] = chain[i,]
		}
	}
	return(chain)
}

#10,000 samples for each model parameter
library(invgamma)
startvalue = c(0,10,1.5)
chain = met_MCMC(startvalue, 10000)

####b)
#create coda object
library(coda)
chain<-mcmc(chain)
burn = 1000
chain<-mcmc(chain[-(1:burn),])


####c)
#Report model parameter estimates
summary(chain)
#Estimate 95% HDP
HPDinterval(chain, prob=.95)
#plot fit superimposed on data
plot(x, y, ylab="Aboveground Biomass",xlab="",xaxt="n")
mtext("Number of Functional Groups", side=1, line=2, cex=.75)
axis(1,at=1:3,labels=1:3)
abline(lm(y~x))		                                #MLE (slope and intercept) from 2016
abline(a=26.02, b=31.90, col="red")             #MCMC sampling the posterior (slope and intercept)
abline(lm(d2015$AboveBiomass~d2015$FG),col="blue")#MLE (slope and intercept) from 2015 Carbon storage
both<-read.csv(file="fifteenandsixteen.csv", sep=",", head=TRUE)
library(lme4)
fit <- lmer(both$AGB ~ both$FG + (both$Year||both$Mesocosm.ID))
abline(a=fit@beta[1], b=fit@beta[2], col="green", lty=2) #random effects from year and mesocosm ID


###Total Aboveground Biomass (Gibbs)
#response
yy1<-d2016$AGB
#treatment
xx1<-cbind(matrix(1,nrow(d2016),1),d2016$FG)
#first run a one-way anova
summary(aov(yy1~xx1[,2]))
# data for the Bayesian analysis
dtdt1 <- list(yy=yy1,XX=xx1)
# runiregGibbs uses a normal prior for the regression coefficients and 
# an inverse chi-squared prior for va

# mean of the normal prior. We have 2 estimates - 1 intercept 
# and 1 regression coefficients
mod2015<-lm(d2015$AboveBiomass~d2015$FG)

betabar1 <- c(mod2015$coefficients[1],mod2015$coefficients[2])

# Pecision matrix for the normal prior. 
A1 <- 0.01 * diag(2)


# degrees of freedom for the inverse chi-square prior
n1 <- 2

# scale parameter for the inverse chi-square prior
ssq1 <- var(yy1, na.rm=TRUE) 

Prior1 <- list(betabar=betabar1, A=A1, nu=n1, ssq=ssq1)

# number of iterations of the Gibbs sampler
iter <- 10000  

# thinning/slicing parameter.
slice <- 1 

MCMC <- list(R=iter, keep=slice)

sim1 <- runiregGibbs(dtdt1, Prior1, MCMC)



summary(sim1$betadraw)
summary(sim1$sigmasqdraw)
sim2<-mcmc(c(sim1$betadraw[,1],sim1$betadraw[,2]))
HPDinterval(sim2, prob=.95)
# compare with maximum likelihood estimates:
fitcstorage <- lm(d2016$AGB~d2016$FG)
fitcstorage15 <- lm(d2015$AboveBiomass~d2015$FG)
summary(fitcstorage)
par(xpd=TRUE)
plot(d2016$FG, d2016$AGB, ylab="",xlab="",xaxt="n")
mtext("Number of Functional Groups", side=1, line=2, cex=.75)

par(xpd=FALSE)
axis(1,at=1:3,labels=1:3)
abline(fitcstorage)                                                    #MLE (slope and intercept) from 2016
abline(a=mean(sim1$betadraw[,1]), b=mean(sim1$betadraw[,2]), col="red")#MCMC sampling the posterior (slope and intercept)
quantile(sim1$betadraw[,2],c(.025,.975))
quantile(sim1$betadraw[,1],c(.025,.975))
abline(fitcstorage15, col="blue")                            #MLE (slope and intercept) from 2015 Carbon storage
library(lme4)
fit <- lmer(both$AGB ~ both$FG + (both$Year||both$Mesocosm.ID))
summary(fit)
abline(a=fit@beta[1], b=fit@beta[2], col="green", lty=2) #random effects from year and mesocosm ID



par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

plot_colors <- c("black","red", "blue","green")
text <- c("MLE2016", "Bayesian2016", "MLE2015","RandomEffects(yr&site)")
legend("bottom",legend = text, text.width = max(sapply(text, strwidth)),
			 col=plot_colors, xpd=TRUE, cex=1,lwd=2, horiz = TRUE, inset = c(0, 0), bty = "n", x.intersp=0.1)





