
data {
int<lower=0> N; // number of quadrats
vector[N] x; // 
vector[N] y; // 
}

transformed parameters {
 vector[N] ypred;
 ypred <- beta0 + beta1*x;

}

parameters {
real beta0; 
real beta1;
real<lower=0> sigma;
}

model {
beta0 ~ cauchy(0,10); //normal(0, 100);
beta1 ~ cauchy(0,10);
sigma ~ cauchy(0,10)
y ~ normal(ypred,sigma);
}
