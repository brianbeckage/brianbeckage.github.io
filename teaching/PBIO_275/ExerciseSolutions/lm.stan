//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] weight;
  vector[N] height;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real intercept;
  real beta;
  real<lower=0> sigma;
  //real mu[N];
}

transformed parameters {
 
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
   vector[N] mu;
  mu=intercept + beta*weight;
  
  intercept ~ normal(0., 10.0);
  beta ~ normal(0., 10.0);
  sigma ~ exponential(1.0);
  height ~ normal(mu, sigma);
}

