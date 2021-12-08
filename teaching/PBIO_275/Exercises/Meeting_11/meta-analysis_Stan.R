# Marina Golivets
# Meta-analysis using Stan


# load packages
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(loo)
library(brms)
library(dplyr)
library(tidyr)

# set seed
set.seed(1365)



# 1) SIMPLE FIXED- and RANDOM-EFFECTS META-ANALYSES
# simulate data:

# sample size
Nobs <- 30

# true (i.e. overall) mean
trueMean <- 20

# between-observation variance
Var <- 16
randError <- rnorm(n = Nobs, mean = 0, sd = sqrt(Var))

# observation-level sampling variance
vi <- rgamma(n = Nobs, shape = 1, rate = 1)
samplError <- rnorm(n = Nobs, mean = 0, sd = sqrt(vi))

# observed variable
yi <- trueMean + randError + samplError

dat <- data.frame(yi, vi)


# non-multilevel model (i.e. fixed-effects model)
run.stan0 <- function (data, niter, warmup, thin, chains) {
  
  # Stan model
  modelString <- "
  data {
    int <lower=1> N; // num obs
    vector[N] yi; // effect sizes
    vector <lower=0> [N]  sei; // sampling errors
  } 
  parameters {
    real theta;  // global treatment effect, log odds
  } model {
    yi ~ normal(theta, sei);
  }
"
  # compile the model
  writeLines(modelString, con = "TEMPmodelStan.txt")
  stan_code <- readChar("TEMPmodelStan.txt", file.info("TEMPmodelStan.txt")$size)
  
  # data for Stan
  dataList <- list(N = Nobs, yi = dat$yi, sei = sqrt(dat$vi))
  
  # sample from posterior
  stan_out <- stan(model_code = stan_code, data = dataList, chains = chains, thin = thin,
                   iter = niter, warmup = warmup)
  
  return(stan_out)
  
}

stan_out <- run.stan0(data = dat, niter = 10000, warmup = 5000, thin = 20, chains = 1)

print(stan_out, "theta")
stan_hist(stan_out, "theta")

# fit a model in Stan
run.stan1 <- function (data, niter, warmup, thin, chains) {
  
  # Stan model
  modelString <- "
  data {
  int <lower=1> N; // num obs
  vector[N] yi; // effect sizes
  vector <lower=0> [N]  sei; // sampling errors
  }
  parameters {
  real mu; 
  real <lower=0> tau;
  vector[N] eta;
  }
  transformed parameters {
  vector[N] theta;
  theta = mu + tau * eta;
  }
  model {
  yi ~ normal(theta, sei);
  // priors
  mu ~ normal(10, 10);
  tau ~ normal(0, 10);
  eta ~ normal(0, 1);
  }
  generated quantities {
  vector[N] log_lik;
  for (i in 1:N) log_lik[i] = normal_lpdf(yi[i] | theta[i], sei[i]);
  }
  "
  
  # compile the model
  writeLines(modelString, con = "TEMPmodelStan.txt")
  stan_code <- readChar("TEMPmodelStan.txt", file.info("TEMPmodelStan.txt")$size)
  
  # data for Stan
  dataList <- list(N = Nobs, yi = dat$yi, sei = sqrt(dat$vi))
  
  # sample from posterior
  stan_out <- stan(model_code = stan_code, data = dataList, chains = chains, thin = thin,
                   iter = niter, warmup = warmup)
  
  return(stan_out)
  
}

stan_out <- run.stan1(data = dat, niter = 10000, warmup = 5000, thin = 20, chains = 3)

print(stan_out, c("mu", "tau"))
stan_hist(stan_out, pars = c("mu", "tau"))
stan_trace(stan_out, pars = c("mu", "tau"))

# cross-validation
log_lik <- extract_log_lik(stan_out)
loo1 <- loo(log_lik)
loo1

# fit the same model with brms
study <- 1:length(yi)
d <- data.frame(study = study, yi = yi, vi = vi, sei = sqrt(vi))
d$label <- paste("Study", study, sep = "")
fit <- brm(yi | se(sei) ~ 1 + (1|study), data = d,
               iter = 10000, warmup = 5000, thin = 10, cores = 3)
summary(fit)
make_stancode(yi | se(sei) ~ 1 + (1|study), data = d)

brms_forest (data = d, model = fit)



# 2) META-ANALYSIS WITH GROUPING EFFECTS

# simulate data:

# sample size
Nobs <- 500

# true mean
trueMean <- 20

# number of studies
Nstudies <- 30

# between-study variance
betweenStudyVar <- 16

# study-level means
studyMean <- rnorm(n = Nstudies, mean = trueMean, sd = sqrt(betweenStudyVar))

# observation-level variance
Var <- 4
randError <- rnorm(n = Nobs, mean = 0, sd = sqrt(Var))

# sampling variance
vi <- rgamma(n = Nobs, shape = 1, rate = 1)
samplError <- rnorm(n = Nobs, mean = 0, sd = sqrt(vi))

# assign study IDs
studyID <- sample(x = 1:Nstudies, size = Nobs, replace = T)

# generate observed values
yi <- rep(studyMean, as.numeric(table(studyID))) + randError + samplError

dat <- data.frame(obs = 1:Nobs, yi, vi, studyID = sort(studyID))

# fit a model in Stan
run.stan2 <- function (data, niter, warmup, thin, chains) {
  
  modelString <- "
  data {
  int <lower=1> N; // num obs
  int <lower=1> J; // num studies
  int <lower=1, upper=J> jj[N]; // study id
  vector[N] yi; // effect sizes
  vector <lower=0> [N]  sei; 
  }
  parameters {
  real mu; // true mean 
  real <lower=0> sigma_study;
  vector[J] eta_study;
  real <lower=0> sigma_obs; 
  vector[N] eta_obs;
  }
  transformed parameters {
  vector[N] theta;
  for (i in 1:N) theta[i] = mu + sigma_study * eta_study[jj[i]] + sigma_obs * eta_obs[i];
  }
  model {
  yi ~ normal(theta, sei);
  // priors
  // mu ~ normal(0, 10);
  // sigma_study ~ normal(0, 10);
  // sigma_obs ~ normal(0, 10);
  eta_study ~ normal(0, 1);
  eta_obs ~ normal(0, 1);
  }
  "

  # compile the model
  writeLines(modelString, con = "TEMPmodelStan.txt")
  stan_code <- readChar("TEMPmodelStan.txt", file.info("TEMPmodelStan.txt")$size)

  # data for Stan
  dataList <- list(N = Nobs, J = Nstudies, yi = data$yi, sei = sqrt(data$vi), jj = data$studyID)

  # sample from posterior
  stan_out <- stan(model_code = stan_code, data = dataList, chains = chains, thin = thin,
                 iter = niter, warmup = warmup)

  return(stan_out)

  }

outputStan <- run.stan2(data = dat, niter = 10000, warmup = 5000, thin = 20, chains = 3)

print(outputStan, c("mu", "sigma_study", "sigma_obs"))


# compare to REML inference
library(metafor)
summary(rma.mv(yi = yi, V = vi, data = dat, random = ~ 1|studyID/obs))



# 3) META-ANALYSIS WITH PHYLOGENETIC NON-INDEPENDENCE

# simulate data:

# total number of observations
Nobs <- 500

# true mean
trueMean <- 20

# number of studies
Nstudies <- 20

# assign study IDs
studyID <- sample(x = 1:Nstudies, size = Nobs, replace = T)

# between-study variance
betweenStudyVar <- 16

# generate study-level means
studyMean <- rnorm(n = Nstudies, mean = trueMean, sd = sqrt(betweenStudyVar))
studyMean <- data.frame(studyID = 1:Nstudies, studyMean)

# number of species
Nspecies <- 10

# assign species IDs
speciesID <- sample(x = 1:Nspecies, size = Nobs, replace = T)

# phylogenetic variance
phyloVar <- 9

# phylogenetic correlation matrix
VCV <- matrix(c(1, rep(0, 10), 1, 0.9456935, rep(0.4654608, 7), 0, 0.9456935, 1, 
  rep(0.4654608, 7), 0, 0.4654608, 0.4654608, 1, 0.8598278, 
0.6962383, rep(0.6616872, 4), 0, 0.4654608, 0.4654608, 0.8598278, 1,
0.6962383, rep(0.6616872, 4), 0, 0.4654608, 0.4654608, 0.6962383, 0.6962383, 1, 
rep(0.6616872, 4), 0, 0.4654608, 0.4654608, rep(0.6616872, 3), 1,
rep(0.6686945, 3), 0, 0.4654608, 0.4654608, rep(0.6616872, 3), 0.6686945, 1,
0.9998447, 0.7079462, 0, 0.4654608, 0.4654608, rep(0.6616872, 3), 0.6686945, 0.9998447, 1, 
0.7079462, 0, 0.4654608, 0.4654608, rep(0.6616872, 3), 0.6686945, 0.7079462, 0.7079462, 1), 
nrow = Nspecies, ncol = Nspecies)

# phylogenetic covariance matrix
SigmaPhylo <- matrix(rep(phyloVar, Nspecies^2), ncol = Nspecies, nrow = Nspecies) * VCV

# sample from a multivariate normal distribution
library(MASS)
phyloEff <- mvrnorm(n = 1, mu = rep(0, Nspecies), Sigma = SigmaPhylo, tol = 1e-6, 
                    empirical = FALSE, EISPACK = FALSE)
phyloEff <- data.frame (speciesID = 1:10, phyloEff = as.numeric(phyloEff))

# observation-level variance
Var <- 4
randError <- rnorm(n = Nobs, mean = 0, sd = sqrt(Var))

# sampling variance
vi <- rgamma(n = Nobs, shape = 1, rate = 1)
samplError <- rnorm(n = Nobs, mean = 0, sd = sqrt(vi))

dat <- data.frame(obs = 1:Nobs, vi = vi, samplError = samplError, 
                  randError = randError, studyID = studyID, speciesID = speciesID) %>%
  left_join(studyMean, by = "studyID") %>%
  left_join(phyloEff, by = "speciesID") %>%
  mutate(yi = studyMean + phyloEff + samplError + randError)


run.stan3 <- function (data, niter, warmup, thin, chains) {
  
  modelString <- "
  data {
  int <lower=1> N; // num obs
  int <lower=1> J; // num studies
  int <lower=1, upper=J> jj[N]; // study id
  int <lower=1> M; // num species
  int <lower=1, upper=M> mm[N]; // species id
  vector[M] I_sp; // vector of 1s
  corr_matrix[M] VCV; // phylogenetic correlation matrix
  vector[N] yi; // effect sizes
  vector <lower=0> [N]  sei; // sampling errors
  }
  transformed data {
  cholesky_factor_corr[M] VCV_chol;
  VCV_chol = cholesky_decompose(VCV); 
  }
  parameters {
  real mu; // global mean 
  real <lower=0> sigma_study; // study-level sd
  vector[J] eta_study;
  real <lower=0> sigma_obs; 
  vector[N] eta_obs;
  real <lower=0> sigma_phylo;
  vector[M] eta_phylo;
  }
  transformed parameters {
  vector[N] theta;
  vector[M] sigma_phylo_v;
  vector[M] eff_phylo; // phylogenetic effect
  sigma_phylo_v = sigma_phylo * I_sp;
  eff_phylo = sigma_phylo_v .* (VCV_chol * eta_phylo);
  for (i in 1:N) theta[i] = mu + sigma_study * eta_study[jj[i]] + eff_phylo[mm[i]] + sigma_obs * eta_obs[i];
  }
  model {
  yi ~ normal(theta, sei);
  // priors
  // mu ~ normal(0, 10);
  // sigma_study ~ normal(0, 10);
  // sigma_obs ~ normal(0, 10);
  // sigma_phylo ~ normal(0, 5);
  eta_study ~ normal(0, 1);
  eta_obs ~ normal(0, 1);
  eta_phylo ~ normal(0, 1);
  }
  "
  
  # compile the model
  writeLines(modelString, con = "TEMPmodelStan.txt")
  stan_code <- readChar("TEMPmodelStan.txt", file.info("TEMPmodelStan.txt")$size)
  
  # data for Stan
  dataList <- list(N = Nobs, J = Nstudies, M = Nspecies, yi = data$yi, sei = sqrt(data$vi), 
                   jj = data$studyID,
                   mm = data$speciesID, VCV = VCV, I_sp = rep(1, Nspecies))
  
  # sample from posterior
  stan_out <- stan(model_code = stan_code, data = dataList, chains = chains, thin = thin,
                   iter = niter, warmup = warmup)
  
  return(stan_out)
  
}

outputStan <- run.stan3(data = dat, niter = 10000, warmup = 5000, thin = 20, chains = 3)

print(outputStan, c("mu", "sigma_study", "sigma_obs", "sigma_phylo"))
