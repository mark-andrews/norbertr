data {
  int<lower=1> n;
  real<lower=0> y[n];
  int<lower=0, upper=1> z[n];
  real<lower=0> alpha_ub;
  real<lower=0> sigma;
  real<lower=0> tau;
}

parameters {
  real<lower=0, upper=alpha_ub> alpha;
  real<lower=0, upper=1> beta;
  real delta;
}

model {

  // priors
  alpha ~ uniform(0, alpha_ub);
  beta ~ uniform(0, 1);
  delta ~ normal(0, sigma);

  // likelihood model
  for (i in 1:n) {

    if (z[i] == 1){
      // upper barrier crossing times
      y[i] ~ wiener(alpha, tau, beta, delta);
    } else {
      // lower barrier crossing times
      y[i] ~ wiener(alpha, tau, 1-beta, -delta);
    }

  }

}
