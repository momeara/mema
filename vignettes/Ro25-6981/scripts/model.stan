// Poisson model for spike rate


// Adapted from: https://rpubs.com/kaz_yos/stan-pois1
data {
  // Define variables in data
  // Number of neurons
  int<lower=0> N;

  // Number of beta parameters
  int<lower=0> p;

  int<lower=0> n_electrodes;
  int<lower=0> n_conditions;

  // Covariates
  int <lower=0, upper=1> intercept[N];
  int <lower=0, upper=1> electrode[n_electrodes, N];
  int <lower=0, upper=1> condition[N];
  int <lower=0, upper=1> experiment[N];
  int <lower=0, upper=1> animal[N];

  // offset
  real offset[N];

  // in seconds
  real exposure[N];

  // number of spikes during exposure
  int<lower=0> y[N];
}

parameters {
  // Define parameters to estimate
  real beta[p];
}

transformed parameters  {
  //
  real lp[N];
  real <lower=0> mu[N];

  for (i in 1:N) {
    // Linear predictor
    lp[i] <- beta[1]

    for (i in 1:n_electrodes){
      beta[2]*electrode[i] +
      beta[3]*[i] + beta[4]*age65_69[i] + beta[5]*age70_74[i] + beta[6]*age75plus[i]+ beta[7]*cityHorsens[i] + beta[8]*cityKolding[i] + beta[9]*cityVejle[i] + offset[i];

    // Mean
    mu[i] <- exp(lp[i]);
  }
}

model {
  // Prior part of Bayesian inference
  // Flat prior for mu (no need to specify if non-informative)


  // Likelihood part of Bayesian inference
  y ~ poisson(mu);
}
