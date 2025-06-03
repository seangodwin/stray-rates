// stray rate test model
data {
  int<lower=1> N;              // Number of fish observations
  int<lower=0,upper=1> y[N];   // Stray indicator (1 = strayed, 0 = homed)
  int<lower=1> K;              // Number of unique release watersheds
  int<lower=1,upper=K> ws[N];  // Watershed index for each fish
  int<lower=1> T;              // Number of unique years
  int<lower=1,upper=T> year[N];// Year index for each fish (e.g., brood or recovery year)
}

parameters {
  // Watershed-level random effects
  real mu_alpha;                     // Global average stray tendency
  real<lower=0> sigma_alpha;         // SD of stray tendency across watersheds
  vector[K] alpha_raw;               // Raw (standard normal) intercepts per watershed

  // Time trend (RW1)
  vector[T] f;                    // Year effects (ordered via RW1)
  real<lower=0> tau_f;            // SD for RW1

  // Residual individual-level noise
  real<lower=0> sigma_eps;           // SD for residual noise
  vector[N] eps;                     // Random noise term per fish
}

transformed parameters {
  // Compute actual watershed intercepts
  vector[K] alpha = mu_alpha + sigma_alpha * alpha_raw;

  // Compute log-odds (eta) for each fish’s stray probability
  vector[N] eta;
  for (n in 1:N)
    eta[n] = alpha[ws[n]] + f[year[n]] + sigma_eps * eps[n];  // Watershed + year + noise
}

model {
  // Priors for watershed effects
  alpha_raw ~ normal(0, 1);
  mu_alpha ~ normal(0, 2);
  sigma_alpha ~ normal(0, 1);

  // Random Walk (RW1) prior for year effects
  f[1] ~ normal(0, 1);  // weak prior on starting point
  for (t in 2:T)
    f[t] ~ normal(f[t - 1], tau_f);

  tau_f ~ normal(0, 1);  // prior for RW1 step size

  // Priors for residual variation
  eps ~ normal(0, 1);
  sigma_eps ~ normal(0, 1);

  // Likelihood: each fish's stray status ~ Bernoulli(logit(eta))
  y ~ bernoulli_logit(eta); // Using Bernoulli logit model without parallelization
}


generated quantities {
  int y_rep[N];
    for (n in 1:N)
      y_rep[n] = bernoulli_logit_rng(eta[n]);
}

