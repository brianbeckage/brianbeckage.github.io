data {
  int<lower=0> N;
  real<lower=0,upper=1> light[N];
  int<lower=0> nSeedlings[N];
}

parameters {
  real beta0;
  real beta1;
}

transformed parameters {
  real lp[N];
  real <lower=0> mu[N];
  
  for(i in 1:N){
    lp[i] = beta0 + beta1 * light[i];
    mu[i] = exp(lp[i]);
  }
}

model {
  nSeedlings ~ poisson(mu);
}



